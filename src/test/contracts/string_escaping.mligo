let rev (l : string list) : string list =
  let rec aux (l, r : string list * string list) : string list =
    match l with
    | [] -> r
    | x :: l -> aux (l, x :: r) in
  aux (l, ([] : string list))

let to_list (s : string) : string list =
  let n = String.length s in
  let rec aux (s, i, l : string * nat * string list) : string list =
    if i < n
    then aux (s, i + 1n, String.slice i 1n s :: l)
    else l in
  rev (aux (s, 0n, ([] : string list)))

let ss1 = to_list "\n"
let ss2 = to_list "\x53"
let ss3 = to_list "\\n"
