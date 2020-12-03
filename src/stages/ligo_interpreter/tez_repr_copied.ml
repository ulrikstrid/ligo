(*THIS HAS BEEN CHANGED*)
type 't tzresult = 't option

let (>>?) x f = match x with
| Some x -> f x
| None -> None

let ok x = Some x
let error _ = None

module Compare = struct
  module Int = struct
    let (=) = (=)
    let (<) = (<)
    let (>) = (>)
    let (<=) = (<=)
  end

  module Int64 = struct
    type t = Int64.t
    let compare = Int64.compare
    let (=) = Int64.equal
  end
end

type error = ..

(*THIS HAS BEEN CHANGED*)

let id = "tez"

let name = "mutez"

include Compare.Int64 (* invariant: positive *)

type error +=
  | Addition_overflow of t * t (* `Temporary *)
  | Subtraction_underflow of t * t (* `Temporary *)
  | Multiplication_overflow of t * int64 (* `Temporary *)
  | Negative_multiplicator of t * int64 (* `Temporary *)
  | Invalid_divisor of t * int64

(* `Temporary *)

let zero = 0L

(* all other constant are defined from the value of one micro tez *)
let one_mutez = 1L

let one_cent = Int64.mul one_mutez 10_000L

let fifty_cents = Int64.mul one_cent 50L

(* 1 tez = 100 cents = 1_000_000 mutez *)
let one = Int64.mul one_cent 100L

let of_string s =
  let triplets = function
    | hd :: tl ->
        let len = String.length hd in
        Compare.Int.(
          len <= 3 && len > 0 && List.for_all (fun s -> String.length s = 3) tl)
    | [] ->
        false
  in
  let integers s = triplets (String.split_on_char ',' s) in
  let decimals s =
    let l = String.split_on_char ',' s in
    if Compare.Int.(List.length l > 2) then false else triplets (List.rev l)
  in
  let parse left right =
    let remove_commas s = String.concat "" (String.split_on_char ',' s) in
    let pad_to_six s =
      let len = String.length s in
      String.init 6 (fun i -> if Compare.Int.(i < len) then s.[i] else '0')
    in
    Int64.of_string_opt (remove_commas left ^ pad_to_six (remove_commas right))
  in
  match String.split_on_char '.' s with
  | [left; right] ->
      if String.contains s ',' then
        if integers left && decimals right then parse left right else None
      else if
        Compare.Int.(String.length right > 0)
        && Compare.Int.(String.length right <= 6)
      then parse left right
      else None
  | [left] ->
      if (not (String.contains s ',')) || integers left then parse left ""
      else None
  | _ ->
      None

let pp ppf amount =
  let mult_int = 1_000_000L in
  let rec left ppf amount =
    let (d, r) = (Int64.(div amount 1000L), Int64.(rem amount 1000L)) in
    if d > 0L then Format.fprintf ppf "%a%03Ld" left d r
    else Format.fprintf ppf "%Ld" r
  in
  let right ppf amount =
    let triplet ppf v =
      if Compare.Int.(v mod 10 > 0) then Format.fprintf ppf "%03d" v
      else if Compare.Int.(v mod 100 > 0) then
        Format.fprintf ppf "%02d" (v / 10)
      else Format.fprintf ppf "%d" (v / 100)
    in
    let (hi, lo) = (amount / 1000, amount mod 1000) in
    if Compare.Int.(lo = 0) then Format.fprintf ppf "%a" triplet hi
    else Format.fprintf ppf "%03d%a" hi triplet lo
  in
  let (ints, decs) =
    (Int64.(div amount mult_int), Int64.(to_int (rem amount mult_int)))
  in
  left ppf ints ;
  if Compare.Int.(decs > 0) then Format.fprintf ppf ".%a" right decs

let to_string t = Format.asprintf "%a" pp t

let ( -? ) t1 t2 =
  if t2 <= t1 then ok (Int64.sub t1 t2)
  else error (Subtraction_underflow (t1, t2))

let ( +? ) t1 t2 =
  let t = Int64.add t1 t2 in
  if t < t1 then error (Addition_overflow (t1, t2)) else ok t

let ( *? ) t m =
  let open Compare.Int64 in
  let open Int64 in
  let rec step cur pow acc =
    if cur = 0L then ok acc
    else
      pow +? pow
      >>? fun npow ->
      if logand cur 1L = 1L then
        acc +? pow >>? fun nacc -> step (shift_right_logical cur 1) npow nacc
      else step (shift_right_logical cur 1) npow acc
  in
  if m < 0L then error (Negative_multiplicator (t, m))
  else step m t 0L 

let ( /? ) t d =
  if d <= 0L then error (Invalid_divisor (t, d)) else ok (Int64.div t d)

let mul_exn t m = t *? Int64.(of_int m)
let of_mutez t = if t < 0L then None else Some t

let of_mutez_exn x =
  match of_mutez x with None -> invalid_arg "Tez.of_mutez" | Some v -> v

let to_int64 t = t

let to_mutez t = t

let encoding =
  let open Data_encoding in
  Data_encoding.def
    name
    (check_size 10 (conv Z.of_int64 (Json.wrap_error Z.to_int64) n))

type tez = t
