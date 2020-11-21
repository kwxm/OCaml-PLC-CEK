(* ---------------- CBOR decoding ---------------- *)

(* We're expecting CBOR with names and uniques, not deBruijn indices *)

open Absyn
open CBOR
open Printf

exception DecodeError of string
let decode_error fmt = ksprintf (fun s -> raise (DecodeError s)) fmt

type input_stream = string * int ref

let decode_decode_errorure s x (bytes, r)  =
  if !r >= String.length bytes
  then decode_error "%s decode_errored at offset %d: found %s (past end of input)" s  !r (to_diagnostic x)
  else decode_error "%s decode_errored at offset %d: found %s (tag %d)" s !r (to_diagnostic x) (Char.code bytes.[!r])

let decode_bigint is =
  match extract is with
  | `Int n -> Z.of_int n
  | `BigInt n -> n
  | x -> decode_decode_errorure "decode_integer" x is

let decode_integer is =
  match extract is with
  | `Int n -> n
  | x -> decode_decode_errorure "decode_integer" x is

let decode_bytestring is =
  match extract is with
  | `Bytes bs -> bs
  | x ->  decode_decode_errorure "decode_bytestring" x is

let decode_string is =
  match extract is with
  | `Text s -> s
  | x ->  decode_decode_errorure "decode_string" x is

let decode_char is = (* characters are apparently encoded as 1-element strings, but that's probably for Unicode *)
  match extract is with
  | `Text s -> s.[0]  (* FIXME *)
  | x ->  decode_decode_errorure "decode_char" x is

let decode_unit is =
  match extract is with
  | `Null -> ()
  | x ->  decode_decode_errorure "decode_unit" x is

let decode_bool is =
  match extract is with
  | `Bool b -> b
  | x ->  decode_decode_errorure "decode_bool" x is



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
     | _ -> decode_error "Unexpected BuiltinName tag: %d" tag


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
          | _ -> decode_error "Unexpected Constant tag: %d" tag
        end
      | [t] -> decode_decode_errorure "decode_constant" t is
      | _ -> decode_decode_errorure (sprintf "decode_constant (list has %d elements)" (List.length l)) `Undefined is
    end
  | x -> decode_decode_errorure "decode_constant" x is

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
     | _ -> decode_error "Unexpected Term tag: %d "tag


let decode_prog is =
  let v1 = decode_integer is in
  let v2 = decode_integer is in
  let v3 = decode_integer is in
  let body = decode_term is in
  Program (v1, v2, v3, body)

let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let read_cbor fname =
  let cbor = read_whole_file fname
  in decode_prog (cbor, ref 0)
