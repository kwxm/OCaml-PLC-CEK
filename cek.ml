(* CEK machine for untyped Plutus Core *)

open Absyn

(* ---------------- Machine ---------------- *)

module IntMap = Map.Make(struct type t = int let compare = compare end)

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


let () = if Array.length Sys.argv <= 1
        then print_string "File?\n"
        else Decoder.read_cbor (Sys.argv.(1))
