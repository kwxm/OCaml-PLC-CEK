open Printf

module IntMap = Map.Make(struct type t = int let compare = compare end)

type builtin
  =  AddInteger
   | SubtractInteger
   | MultiplyInteger
   | DivideInteger
   | QuotientInteger
   | RemainderInteger
   | ModInteger
   | LessThanInteger
   | LessThanEqInteger
   | GreaterThanInteger
   | GreaterThanEqInteger
   | EqInteger
   | Concatenate
   | TakeByteString
   | DropByteString
   | SHA2
   | SHA3
   | VerifySignature
   | EqByteString
   | LtByteString
   | GtByteString
   | IfThenElse
   | CharToString
   | Append
   | Trace

let show_builtin =
  function
    | AddInteger           -> "addInteger"
    | SubtractInteger      -> "subtractInteger"
    | MultiplyInteger      -> "multiplyInteger"
    | DivideInteger        -> "divideInteger"
    | ModInteger           -> "modInteger"
    | QuotientInteger      -> "quotientInteger"
    | RemainderInteger     -> "remainderInteger"
    | LessThanInteger      -> "lessThanInteger"
    | LessThanEqInteger    -> "lessThanEqualsInteger"
    | GreaterThanInteger   -> "greaterThanInteger"
    | GreaterThanEqInteger -> "greaterThanEqualsInteger"
    | EqInteger            -> "equalsInteger"
    | Concatenate          -> "concatenate"
    | EqByteString         -> "equalsByteString"
    | LtByteString         -> "lessThanByteString"
    | GtByteString         -> "greaterThanByteString"
    | TakeByteString       -> "takeByteString"
    | DropByteString       -> "dropByteString"
    | SHA2                 -> "sha2_256"
    | SHA3                 -> "sha3_256"
    | VerifySignature      -> "verifySignature"
    | IfThenElse           -> "ifThenElse"
    | CharToString         -> "charToString"
    | Append               -> "append"
    | Trace                -> "trace"


type const =
  | Int_const of Z.t  (* Big integer *)
  | Bytestring_const of string
  | String_const of string
  | Char_const of char
  | Unit_const of unit
  | Bool_const of bool

let show_const =
  function
  | Int_const n        -> sprintf "(con integer %s)" (Z.to_string n)
  | Bytestring_const s -> sprintf "(con bytestring #%s)" s
  | String_const s     -> sprintf "(con string \"%s\")" s
  | Char_const c       -> sprintf "(con char '%s')" (Char.escaped c)
  | Unit_const u       -> sprintf "(con unit %s)" (Unit.to_string u)
  | Bool_const b       -> sprintf "(con boolean %s)" (string_of_bool b)

type name = {id: string; uniq: int}

let show_name n = sprintf "%s_%d" n.id n.uniq

type 'name term =
  | Constant of const
  | Builtin  of builtin
  | Var      of 'name
  | LamAbs   of 'name * 'name term
  | Apply    of 'name term * 'name term
  | Delay    of 'name term
  | Force    of 'name term
  | Error

let rec show_term =
  function
  | Constant c -> sprintf "%s" (show_const c)
  | Builtin  bn -> sprintf "(builtin %s)" (show_builtin bn)
  | Var      v -> show_name v
  | LamAbs   (name, body) -> sprintf "(lam %s\n %s\n)" (show_name name) (show_term body)
  | Apply    (t1, t2) -> sprintf "[\n%s\n%s\n]" (show_term t1) (show_term t2)
  | Delay t -> sprintf "(delay %s)" (show_term t)
  | Force t -> sprintf "(force %s)" (show_term t)
  | Error -> "(error)"

type 'name program = Program of int * int * int * 'name term

let show_program (Program (v1, v2, v3, body)) =
  sprintf "(program %d.%d.%d\n%s\n)\n" v1 v2 v3 (show_term body)

type argType = Term | Type
type arity = argType list

let arities = [
    (AddInteger, [Term, Term]),
    (SubtractInteger, [Term, Term]),
    (MultiplyInteger, [Term, Term]),
    (DivideInteger, [Term, Term]),
    (QuotientInteger, [Term, Term]),
    (RemainderInteger, [Term, Term]),
    (ModInteger, [Term, Term]),
    (LessThanInteger, [Term, Term]),
    (LessThanEqInteger, [Term, Term]),
    (GreaterThanInteger, [Term, Term]),
    (GreaterThanEqInteger, [Term, Term]),
    (EqInteger, [Term, Term]),
    (Concatenate, [Term, Term]),
    (TakeByteString, [Term, Term]),
    (DropByteString, [Term, Term]),
    (SHA2, [Term]),
    (SHA3, [Term]),
    (VerifySignature, [Term, Term, Term]),
    (EqByteString, [Term, Term]),
    (LtByteString, [Term, Term]),
    (GtByteString, [Term, Term]),
    (IfThenElse, [Term, Term]),
    (CharToString, [Type, Term, Term, Term]),
    (Append, [Term, Term]),
    (Trace, [Term, Term])
  ]

exception DecodeError of string
let fail fmt = ksprintf (fun s -> raise (DecodeError s)) fmt

type 'name cek_val_env =  ('name term) IntMap.t

and 'name cek_value =
  | VCon of const
  | VDelay of 'name term * 'name cek_val_env
  | VLamAbs of 'name * 'name term  * 'name cek_val_env
  | VBuiltin of
      builtin
      * arity             (* Sorts of arguments to be provided (both types and terms): *don't change this*. *)
      * arity             (* A copy of the arity used for checking applications/instantiatons: see Note [Arities in VBuiltin] *)
      * int               (* The number of @force@s to apply to the builtin. *)
                          (* We need it to construct a term if the machine is returning a stuck partial application. *)
      * ('name cek_value) list    (* Arguments we've computed so far. *)
      * 'name cek_val_env   (* Initial environment, used for evaluating every argument *)


(* Attempt to reconstruct a partial builtin application when there's an arity mismatch *)
let mk_builtin_application ex bn arity0 forces0 args0 =
    let rec go arity forces args term =
      match (arity, args, forces) with
      (* We've got to the end and successfully constructed the entire application *)
      | ([], [], 0)
        -> term (* got an expected term argument *)
      | (Term::arity', arg::args', _)
        -> go arity' forces args' (Apply (term, arg))  (* term expected, type found *)
      | (Term::_, [], _)
        -> if forces >= 1
           then Force term (* got an expected type argument *)
           else term
      | (Type::arity', _, _)
        -> if forces >= 1
           then go arity' (forces-1) args (Force term) (* type expected, term found *)
           else begin
               match args with
               | [] -> term
               | arg::_ -> Apply (term, arg)  (* something else, including partial application *)
             end
      | _ -> term
    in go arity0 forces0 args0 (Builtin bn)

let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

open CBOR

type input_stream = string * int ref


let decode_failure s x (bytes, r)  =
  if !r >= String.length bytes
  then fail "%s failed at offset %d: found %s (past end of input)" s  !r (to_diagnostic x)
  else fail "%s failed at offset %d: found %s (tag %d)" s !r (to_diagnostic x) (Char.code bytes.[!r])

let decode_bigint is =
  match extract is with
  | `Int n -> Z.of_int n
  | `BigInt n -> n
  | x -> decode_failure "decode_integer" x is

let decode_integer is =
  match extract is with
  | `Int n -> n
  | x -> decode_failure "decode_integer" x is
     
let decode_bytestring is =
  match extract is with
  | `Bytes bs -> bs
  | x ->  decode_failure "decode_bytestring" x is
        
let decode_string is =
  match extract is with
  | `Text s -> s
  | x ->  decode_failure "decode_string" x is
        
let decode_char is = (* characters are apparently encoded as 1-element strings, but that's probably for Unicode *)
  match extract is with
  | `Text s -> s.[0]  (* FIXME *)
  | x ->  decode_failure "decode_char" x is
        
let decode_unit is =
  match extract is with
  | `Null -> ()
  | x ->  decode_failure "decode_unit" x is
        
let decode_bool is =
  match extract is with
  | `Bool b -> b
  | x ->  decode_failure "decode_bool" x is
        
        
        
let decode_builtin is =
  let tag = decode_integer is
  in match tag with
     | 0  -> AddInteger
     | 1  -> SubtractInteger
     | 2  -> MultiplyInteger
     | 3  -> DivideInteger
     | 4  -> RemainderInteger
     | 5  -> LessThanInteger
     | 6  -> LessThanEqInteger
     | 7  -> GreaterThanInteger
     | 8  -> GreaterThanEqInteger
     | 9  -> EqInteger
     | 10 -> Concatenate
     | 11 -> TakeByteString
     | 12 -> DropByteString
     | 13 -> SHA2
     | 14 -> SHA3
     | 15 -> VerifySignature
     | 16 -> EqByteString
     | 17 -> QuotientInteger
     | 18 -> ModInteger
     | 19 -> LtByteString
     | 20 -> GtByteString
     | 21 -> IfThenElse
     | 22 -> CharToString
     | 23 -> Append
     | 24 -> Trace
     | _ -> fail "Unexpected BuiltinName tag: %d" tag


let decode_constant is =
  match extract is with
    `Array l -> begin
      match l with
      | [`Int tag] -> begin
          match tag with
          | 0 -> Int_const        (decode_bigint is)
          | 1 -> Bytestring_const (decode_bytestring is)
          | 2 -> String_const     (decode_string is)
          | 3 -> Char_const       (decode_char is)
          | 4 -> Unit_const       (decode_unit is)
          | 5 -> Bool_const       (decode_bool is)
          | _ -> fail "Unexpected Constant tag: %d" tag
        end
      | [t] -> decode_failure "decode_constant" t is
      | _ -> decode_failure (sprintf "decode_constant (list has %d elements)" (List.length l)) `Undefined is
    end
  | x -> decode_failure "decode_constant" x is

let decode_name is =
  let id = decode_string is in
  let uniq = decode_integer is in
  {id = id; uniq = uniq}
           

let rec decode_term is =
  let tag = decode_integer is
  in match tag with
     | 0 -> Var      (decode_name is)
     | 1 -> Delay    (decode_term is)
     | 2 -> let x = decode_name is in
            let t = decode_term is
            in LamAbs   (x,t)
     | 3 -> let t1 = decode_term is in
            let t2 = decode_term is
            in Apply (t1,t2)
     | 4 -> Constant (decode_constant is)
     | 5 -> Force    (decode_term is)
     | 6 -> Error
     | 7 -> Builtin  (decode_builtin is)
     | _ -> fail "Unexpected Term tag: %d "tag


let decode_prog is =
  let v1 = decode_integer is in
  let v2 = decode_integer is in
  let v3 = decode_integer is in
  let body = decode_term is in
  Program (v1, v2, v3, body)

let read_cbor fname =
  let cbor = read_whole_file fname
  in let p = decode_prog (cbor, ref 0)
     in print_string (show_program p)

let () = if Array.length Sys.argv <= 1
        then print_string "File?\n"
        else read_cbor (Sys.argv.(1))
