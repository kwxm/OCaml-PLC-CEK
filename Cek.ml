(* CEK machine for untyped Plutus Core *)

(* This should only be run on ASTs with Names *)

open Absyn
open Printf

let (@@) f x = f x

(* ---------------- Machine ---------------- *)

exception CekError of string
let fail fmt = ksprintf (fun s -> raise (CekError s)) fmt


module IntSet = Set.Make(struct type t = int let compare = compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)
              
type argType = TermArg | TypeArg
type arity = argType list

let arities : (builtin * arity) list =
  [
    (AddInteger, [TermArg; TermArg]);
    (SubtractInteger, [TermArg; TermArg]);
    (MultiplyInteger, [TermArg; TermArg]);
    (DivideInteger, [TermArg; TermArg]);
    (QuotientInteger, [TermArg; TermArg]);
    (RemainderInteger, [TermArg; TermArg]);
    (ModInteger, [TermArg; TermArg]);
    (LessThanInteger, [TermArg; TermArg]);
    (LessThanEqInteger, [TermArg; TermArg]);
    (GreaterThanInteger, [TermArg; TermArg]);
    (GreaterThanEqInteger, [TermArg; TermArg]);
    (EqInteger, [TermArg; TermArg]);
    (Concatenate, [TermArg; TermArg]);
    (TakeByteString, [TermArg; TermArg]);
    (DropByteString, [TermArg; TermArg]);
    (SHA2, [TermArg]);
    (SHA3, [TermArg]);
    (VerifySignature, [TermArg; TermArg; TermArg]);
    (EqByteString, [TermArg; TermArg]);
    (LtByteString, [TermArg; TermArg]);
    (GtByteString, [TermArg; TermArg]);
    (IfThenElse, [TypeArg;TermArg; TermArg; TermArg]);
    (CharToString, [TermArg]);
    (Append, [TermArg; TermArg]);
    (Trace, [TermArg; TermArg])
  ]

let lookup_arity : builtin -> arity =
  fun bn ->
  try
    List.assoc bn arities
  with Not_found -> fail "Can't find arity for builtin %s" (show_builtin bn)

type cek_val_env =  cek_value IntMap.t

and cek_value =
  | VCon of const
  | VDelay of name term * cek_val_env
  | VLamAbs of name * name term  * cek_val_env
  | VBuiltin of
      builtin
      * arity             (* Sorts of arguments to be provided (both types and terms): *don't change this*. *)
      * arity             (* A copy of the arity used for checking applications/instantiatons: see Note [Arities in VBuiltin] *)
      * int               (* The number of @force@s to apply to the builtin. *)
                          (* We need it to construct a term if the machine is returning a stuck partial application. *)
      * cek_value list    (* Arguments we've computed so far. *)
      * cek_val_env       (* Initial environment, used for evaluating every argument *)

let lookup_name : name -> cek_val_env -> cek_value =
  fun  x env ->
  try
    IntMap.find x.uniq env
  with Not_found -> fail "Variable %s_%d not found in environment" x.id x.uniq

let show_cek_value : cek_value -> string =
  fun v ->
  match v with
  | VCon c -> show_const c
  | VDelay (t,_) -> show_term t
  | VLamAbs (x,t,_) -> sprintf "lam %s %s" (show_name x) (show_term t)
  | VBuiltin (_,_,_,_,_,_) -> "builtin"

(* Attempt to reconstruct a partial builtin application when there's an arity mismatch *)
let mk_builtin_application : builtin -> arity -> int -> (name term list) -> name term =
  fun bn ar0 forces0 args0 ->
  let rec go ar forces args term =
      match (ar, args, forces) with
      (* We've got to the end and successfully constructed the entire application *)
      | ([], [], 0)
        -> term (* got an expected term argument *)
      | (TermArg::ar', arg::args', _)
        -> go ar' forces args' (Apply (term, arg))  (* term expected, type found *)
      | (TermArg::_, [], _)
        -> if forces >= 1
           then Force term (* got an expected type argument *)
           else term
      | (TypeArg::ar', _, _)
        -> if forces >= 1
           then go ar' (forces-1) args (Force term) (* type expected, term found *)
           else begin
               match args with
               | [] -> term
               | arg::_ -> Apply (term, arg)  (* something else, including partial application *)
             end
      | _ -> term
    in go ar0 forces0 args0 (Builtin bn)

let rec subst_free_names: cek_val_env -> name term -> name term =
  fun env t ->
  let rec go bvs t =
    match t with
      Var x ->  if IntSet.mem x.uniq bvs
                then Var x
                else let v = lookup_name x env in
                     discharge_cek_value v
    | LamAbs (x,body) -> let body' = go (IntSet.add x.uniq bvs) body
                        in LamAbs (x, body')
    | Apply (fn, arg) -> let fn' = go bvs fn in
                         let arg' = go bvs arg in
                         Apply (fn',arg')
    | Delay term       -> Delay (go bvs term)
    | Force term       -> Force (go bvs term)
    | Constant _ -> t
    | Builtin _ -> t
    | Error -> t
  in go IntSet.empty t

and discharge_cek_value : cek_value -> name term =
  fun v ->
  match v with
  | VCon      v                            -> Constant v
  | VDelay    (t,env)                      -> Delay (subst_free_names env t)
  | VLamAbs   (x,t,env)                    -> LamAbs (x, subst_free_names env t)
  | VBuiltin  (bn,ar0,_,forces,args, _)    -> mk_builtin_application bn ar0 forces (List.map discharge_cek_value args)

type frame
  = FrameApplyFun of cek_value
  | FrameApplyArg of cek_val_env * name term
  | FrameForce

type context = frame list

let extend_env x v env = IntMap.add x.uniq v env

let rec compute_cek : context -> cek_val_env -> name term -> name term =
  fun ctx env term ->
  match term with
  | Force t         -> compute_cek (FrameForce::ctx) env t
  | Apply (fn, arg) -> compute_cek (FrameApplyArg (env, arg)::ctx) env fn
  | Delay t         -> return_cek ctx (VDelay (t, env))
  | LamAbs (x,t)    -> return_cek ctx (VLamAbs(x,t,env))
  | Constant c      -> return_cek ctx (VCon c)
  | Builtin bn      -> let ar = lookup_arity bn
                       in return_cek ctx (VBuiltin (bn,ar,ar,0,[],env))
  | Error           -> Error
  | Var x           -> return_cek ctx (lookup_name x env)

and return_cek : context -> cek_value -> name term =
  fun ctx v ->
  match ctx with
  | [] -> discharge_cek_value v
  | FrameForce :: ctx'             -> force_evaluate ctx' v
  | FrameApplyArg(env,arg) :: ctx' -> compute_cek (FrameApplyFun v :: ctx') env arg
  | FrameApplyFun f :: ctx'        ->  apply_evaluate ctx' f v


and force_evaluate : context -> cek_value -> name term =
  fun ctx v ->
  match v with
  | VDelay (t,env) -> compute_cek ctx env t
  | VBuiltin (bn,ar0,ar,forces,args,env) ->
     begin
       match ar with
       | [] -> fail "force_evaluate: empty arity for %s" (show_builtin bn)
       | TermArg::_ -> fail "force_evaluate: force expected, term argument found"
       | TypeArg::ar' ->
              begin
                match ar' with
                | [] -> apply_builtin ctx bn args (* Final argument is a type argument *)
                | _ -> return_cek ctx (VBuiltin (bn,ar0,ar',forces+1,args,env))
              end
     end
  | _ -> fail "force_evaluate: non-polymorphic instantiation"

and apply_evaluate : context -> cek_value -> cek_value -> name term =
  fun ctx fn arg ->
  match fn with
  | VLamAbs (x,t,env) -> compute_cek ctx (extend_env x arg env) t
  | VBuiltin  (bn,ar0,ar,forces,args,env) ->
     begin
       match ar with
       | [] -> fail "empty arity in apply_evaluate"
       | TypeArg::_ -> fail "Error evaluating builtin %s: term expected, type found" (show_builtin bn)
       | TermArg::ar' ->
          let args' = args @ [arg] in
          begin
            match ar' with
            | [] -> apply_builtin ctx bn args' (* We've got all of the arguments *)
            |  _ -> return_cek ctx (VBuiltin(bn, ar0, ar', forces, args', env))
          end
     end
  | _ -> fail "Attempting to apply non-function: %s" (show_cek_value fn)

and apply_builtin : context -> builtin -> cek_value list -> name term =
  fun ctx bn args ->
  let retcon r = return_cek ctx (VCon r) (* For functions which always return constants *)
  in match (bn, args) with
    | (AddInteger,           [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Int_const (Z.add a b)
    | (SubtractInteger,      [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Int_const (Z.sub a b)
    | (MultiplyInteger,      [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Int_const (Z.mul a b)
    | (DivideInteger,        [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Int_const (Z.ediv a b)
    | (ModInteger,           [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Int_const (Z.erem a b)
    | (QuotientInteger,      [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Int_const (Z.div a b)
    | (RemainderInteger,     [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Int_const (Z.rem a b)
    | (LessThanInteger,      [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Bool_const (Z.lt a b)
    | (LessThanEqInteger,    [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Bool_const (Z.leq a b)
    | (GreaterThanInteger,   [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Bool_const (Z.gt a b)
    | (GreaterThanEqInteger, [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Bool_const (Z.geq a b)
    | (EqInteger,            [VCon (Int_const a); VCon (Int_const b)]) -> retcon @@ Bool_const (Z.equal a b)
    | (Concatenate,          [VCon (Bytestring_const s); VCon (Bytestring_const t)]) -> retcon @@ String_const (s^t)

    | (TakeByteString,       [VCon (Int_const n); VCon(Bytestring_const s)]) -> let n1 = Z.to_int n in
                                                                                let s' = String.sub s 0 n1 in
                                                                                retcon @@ Bytestring_const s'
    | (DropByteString,       [VCon (Int_const n); VCon(Bytestring_const s)]) -> let n1 = Z.to_int n in
                                                                                let s' = String.sub s n1 (String.length s - n1) in
                                                                                retcon @@ Bytestring_const s'
    | (SHA2,                 [VCon (Bytestring_const _)]) -> fail "SHA2: not implemented"
    | (SHA3,                 [VCon (Bytestring_const _)]) -> fail "SHA3: not implemented"
    | (VerifySignature,      [VCon (Bytestring_const _);
                              VCon (Bytestring_const _);
                              VCon (Bytestring_const _)]) -> fail "VerifySignature: not implemented"

    | (EqByteString,         [VCon(Bytestring_const s); VCon(Bytestring_const t)]) -> retcon @@ Bool_const (s=t)
    | (LtByteString,         [VCon(Bytestring_const s); VCon(Bytestring_const t)]) -> retcon @@ Bool_const (s<t)
    | (GtByteString,         [VCon(Bytestring_const s); VCon(Bytestring_const t)]) -> retcon @@ Bool_const (s>t)
    | (CharToString,         [VCon (Char_const c)])                                -> retcon @@ String_const (String.make 1 c)
    | (Append,               [VCon (String_const s); VCon (String_const t)])       -> retcon @@ String_const (s^t)
    | (Trace,                [VCon(String_const s)])                               -> let () = printf "%s" s in
                                                                                      retcon @@ Unit_const ()
    | (IfThenElse, [VCon(Bool_const b); v1; v2]) -> if b
                                                    then return_cek ctx v1
                                                    else return_cek ctx v2
    | _ -> fail "Bad application in apply_builtin: %s: [%s]"
             (show_builtin bn)
             (String.concat "," (List.map show_cek_value args))

