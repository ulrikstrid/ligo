 (* Purely functional queues based on a pair of lists *)

type 'a t = {rear: 'a list; front: 'a list}

let empty = {rear=[]; front=[]}

let enq x q = {q with rear = x::q.rear}

let concat x q = 
  let rec inner acc l1 l2 =
    match l1, l2 with 
    | [], [] -> List.rev acc
    | item :: rest, after -> inner (item :: acc) rest after
    | before, item :: rest -> inner (item :: acc) before rest
  in
  {q with rear = inner [] x.rear q.rear}

let rec deq = function
  {rear=[]; front=  []} -> None
| {rear;    front=  []} -> deq {rear=[]; front = List.rev rear}
| {rear;    front=x::f} -> Some ({rear; front=f}, x)

let rec peek = function
  {rear=[]; front=  []}      -> None
| {rear;    front=  []}      -> peek {rear=[]; front = List.rev rear}
| {rear=_;  front=x::_} as q -> Some (q,x)

let is_empty q = (q = empty)

(* let rev { rear; front } = { front = []; rear = List.rev rear @ front } *)

let rec prepend i q = 
  let rec inner item = function 
  | hd :: tl -> hd :: (inner item tl)
  | [] -> [item]
  in
  match q with 
  | { rear; front = [] } ->
    { front = []; rear = inner i rear }
  | {rear =  []; front } ->
    prepend i {front = []; rear = List.rev front}
  | {rear; front } ->
    prepend i {front = []; rear = (List.rev front) @ rear}
