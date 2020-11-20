(* ---------------- Abstract syntax ---------------- *)

open Printf

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
