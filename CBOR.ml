(* Deserialisation Code extracted from the OCaml CBOR library at
   https://opam.ocaml.org/packages/cbor/, modified to handle bignums and expose
   most functions. *)

open Printf
module BE = EndianBytes.BigEndian_unsafe
module SE = EndianString.BigEndian_unsafe

exception Error of string

let (@@) f x = f x
let (|>) x f = f x
let list_iteri f l = let i = ref 0 in List.iter (fun x -> f !i x; incr i) l
let fail_cbor fmt = ksprintf (fun s -> raise (Error s)) fmt

let hex_char x =
  assert (x >= 0 && x < 16);
  if x <= 9 then Char.chr @@ Char.code '0' + x
  else Char.chr @@ Char.code 'a' + x - 10

let to_hex s =
  let r = Bytes.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    Bytes.set r (i*2) @@ hex_char @@ Char.code s.[i] lsr 4;
    Bytes.set r (i*2+1) @@ hex_char @@ Char.code s.[i] land 0b1111;
  done;
  Bytes.to_string r


type t =
[ `Null
| `Undefined
| `Simple of int
| `Bool of bool
| `Int of int
| `BigInt of Z.t
| `Float of float
| `Bytes of string
| `Text of string
| `Array of t list
| `Map of (t * t) list
]

let need (s,i) n =
  if n > String.length s || !i + n > String.length s then
    fail_cbor "truncated: len %d pos %d need %d" (String.length s) !i n;
  let j = !i in
  i := !i + n;
  j

let get_byte (s,_ as r) = int_of_char @@ s.[need r 1]
let get_n (s,_ as r) n f = f s @@ need r n
let get_s (s,_ as r) n = String.sub s (need r n) n

let get_additional byte1 = byte1 land 0b11111
let is_indefinite byte1 = get_additional byte1 = 31

let int64_max_int = Int64.of_int max_int
let two_min_int32 = 2 * Int32.to_int Int32.min_int

let extract_number byte1 r =
  match get_additional byte1 with
  | n when n < 24 -> n
  | 24 -> get_byte r
  | 25 -> get_n r 2 SE.get_uint16
  | 26 ->
    let n = Int32.to_int @@ get_n r 4 SE.get_int32 in
    if n < 0 then n - two_min_int32 else n
  | 27 ->
    let n = get_n r 8 SE.get_int64 in
    if n > int64_max_int || n < 0L then fail_cbor "extract_number: %Lu" n;
    Int64.to_int n
  | n -> fail_cbor "bad additional %d" n

let get_float16 s i =
  let half = Char.code s.[i] lsl 8 + Char.code s.[i+1] in
  let mant = half land 0x3ff in
  let value =
    match (half lsr 10) land 0x1f with (* exp *)
    | 31 when mant = 0 -> infinity
    | 31 -> nan
    | 0 -> ldexp (float mant) ~-24
    | exp -> ldexp (float @@ mant + 1024) (exp - 25)
  in
  if half land 0x8000 = 0 then value else ~-. value

exception Break

let z_of_string s = (* Convert a bytestring into a big integer *)
  let k = Z.of_int 256 in
  Seq.fold_left (fun n c -> Z.add (Z.mul k n) (Z.of_int (Char.code c))) Z.zero (String.to_seq s)

let extract_list byte1 r f =
  if is_indefinite byte1 then
    let l = ref [] in
    try while true do l := f r :: !l done; assert false with Break -> List.rev !l
  else
    let n = extract_number byte1 r in Array.to_list @@ Array.init n (fun _ -> f r)

let rec extract_pair r =
  let a = extract r in
  let b = try extract r with Break -> fail_cbor "extract_pair: unexpected break" in
  a,b
and extract_string byte1 r f =
  if is_indefinite byte1 then
    let b = Buffer.create 10 in
    try while true do Buffer.add_string b (f @@ extract r) done; assert false with Break -> Buffer.contents b
  else
    let n = extract_number byte1 r in get_s r n
and extract_bigint r =
  let byte1 = get_byte r in
  let s = extract_string byte1 r (function `Bytes s -> s | _ -> fail_cbor "extract_bigint: not a bytes chunk") in
  z_of_string s                     (* FIXME: ^ not sure if this is correct *)
and extract_positive_bigint r = `BigInt (extract_bigint r)
and extract_negative_bigint r =
  let n = extract_bigint r
  in `BigInt (Z.sub Z.minus_one n)  (* draft-ietf-cbor-7049bis-16, 3.4.3 *)
and extract_semantic_value byte1 r =
  match get_additional byte1 with
  | 2 -> extract_positive_bigint r
  | 3 -> extract_negative_bigint r
  | n -> fail_cbor "extract_semantic_value: can't handle semantic tag %d" n
and extract r =
  let byte1 = get_byte r in
  match byte1 lsr 5 with
  | 0 -> `Int (extract_number byte1 r)
  | 1 -> `Int (-1 - extract_number byte1 r)
  | 2 -> `Bytes (extract_string byte1 r (function `Bytes s -> s | _ -> fail_cbor "extract: not a bytes chunk"))
  | 3 -> `Text (extract_string byte1 r (function `Text s -> s | _ -> fail_cbor "extract: not a text chunk"))
  | 4 -> `Array (extract_list byte1 r extract)
  | 5 -> `Map (extract_list byte1 r extract_pair)
  | 6 -> extract_semantic_value byte1 r 
  | 7 -> 
    begin match get_additional byte1 with
    | n when n < 20 -> `Simple n
    | 20 -> `Bool false
    | 21 -> `Bool true
    | 22 -> `Null
    | 23 -> `Undefined
    | 24 -> `Simple (get_byte r)
    | 25 -> `Float (get_n r 2 get_float16)
    | 26 -> `Float (get_n r 4 SE.get_float)
    | 27 -> `Float (get_n r 8 SE.get_double)
    | 31 -> raise Break
    | a -> fail_cbor "extract: (7,%d)" a
    end
  | _ -> assert false

let decode s : t =
  let i = ref 0 in
  let x = try extract (s,i) with Break -> fail_cbor "decode: unexpected break" in
  if !i <> String.length s then fail_cbor "decode: extra data: len %d pos %d" (String.length s) !i;
  x

let to_diagnostic item =
  let b = Buffer.create 10 in
  let put = Buffer.add_string b in
  let rec write = function
  | `Null -> put "null"
  | `Bool false -> put "false"
  | `Bool true -> put "true"
  | `Simple n -> bprintf b "simple(%d)" n
  | `Undefined -> put "undefined"
  | `Int n -> bprintf b "%d" n
  | `BigInt n -> bprintf b "%s" (Z.to_string n)
  | `Float f ->
    begin match classify_float f with
    | FP_nan -> put "NaN"
    | FP_infinite -> put (if f < 0. then "-Infinity" else "Infinity")
    | FP_zero | FP_normal | FP_subnormal -> bprintf b "%g" f
    end
  | `Bytes s -> bprintf b "h'%s'" (to_hex s)
  | `Text s -> bprintf b "\"%s\"" s
  | `Array l ->
    put "[";
    l |> list_iteri (fun i x -> if i <> 0 then put ", "; write x);
    put "]"
  | `Map m ->
    put "{";
    m |> list_iteri (fun i (k,v) -> if i <> 0 then put ", "; write k; put ": "; write v);
    put "}"
  in
  write item;
  Buffer.contents b

