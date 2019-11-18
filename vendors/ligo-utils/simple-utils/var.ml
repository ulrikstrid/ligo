type var = {
  name : string ;
  counter : int option ;
}

let pp ppf v =
  match v.counter with
  | None -> Format.fprintf ppf "%s" v.name
  | Some i -> Format.fprintf ppf "%s#%d" v.name i

module Int = X_int
module Option = X_option

module VarOrderedType = struct
  type t = var

  let equal v1 v2 =
    String.equal v1.name v2.name
    && Option.equal Int.equal v1.counter v2.counter

  let compare v1 v2 =
    let cname = String.compare v1.name v2.name in
    if Int.equal cname 0
    then Option.compare Int.compare v1.counter v2.counter
    else cname
end

include VarOrderedType

module VarSet = Set.Make(VarOrderedType)

let fresh ?name others =
  let others = VarSet.of_list others in
  let name = Option.unopt ~default:"" name in
  let counter = ref 0 in
  let current () = { name ; counter = Some !counter } in
  while (VarSet.mem (current ()) others)
  do incr counter
  done ;
  current ()

let of_name name =
  { name = name ;
    counter = None
  }

let name_of v = v.name
