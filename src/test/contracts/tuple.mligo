(* Various tests of tuple functionality in CameLIGO *)

let tuple_pattern (t: int * int *int) : int =
  match t with
  | (a, b, 10) -> 25
  | (_, _, _) -> 10
