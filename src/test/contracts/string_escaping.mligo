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

let eq_list (s1 : string list) (s2 : string list) : bool =
  let rec aux (s1, s2 : string list * string list) : bool =
    match s1 with
    | [] ->
      (match s2 with
       | [] -> true
       | y :: s2 -> false)
    | x :: s1 ->
      (match s2 with
       | [] -> false
       | y :: s2 ->
         if x = y
         then aux (s1, s2)
         else false) in
  aux (s1, s2)

let test =
  eq_list (to_list "\n") ["\n"]
  && eq_list (to_list "\x53") ["S"]
  && eq_list (to_list "\\n") ["\\"; "n"]
  && eq_list (to_list "\\\n") ["\\"; "\n"]
  && eq_list (to_list {|\\n|}) ["\\"; "\\"; "n"]
