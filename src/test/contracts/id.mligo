type id = string

type id_details = {
  owner: address;
  controller: address;
  profile: bytes;
}

type storage = (id, id_details) big_map * int

(* TODO: Figure out how to keep this price reasonable even as cost of tz fluctuates *)
let price = 10tz

(** Preliminary thoughts on ids:

I very much like the simplicity of http://gurno.com/adam/mne/.
5 three letter words means you have a 15 character identity, not actually more
annoying than an IP address and a lot more memorable than the raw digits. This
can be stored as a single integer which is then translated into the corresponding
series of 5 words.

I in general like the idea of having a 'skip' mechanism, but it does need to cost
something so people don't eat up the address space. 256 ^ 5 means you have a lot
of address space, but if people troll by skipping a lot that could be eaten up.
Should probably do some napkin calculations for how expensive skipping needs to
be to deter people from doing it just to chew up address space.
*)  

let buy (parameter: (bytes * address option) * storage) =
  assert (amount = price) ;
  let profile: bytes = parameter.0.0 in
  let controller: address =
    match parameter.0.1 with
    | Some addr -> addr
    | None -> sender
  in
  let new_id: id = parameter.1.1 + 1 in
  let untaken: bool =
    match Big_map.find_opt new_name parameter.1.0 with
    | Some name -> failwith("The name " ^ new_id ^ "has already been registered.") 
    | None -> true
  in
  let new_id_details: id_details = {
    owner = sender ;
    controller = controller ;
    profile = profile ;
  }
  in
  let updated_identities: (id, id_details) big_map =
    Big_map.update new_id new_id_details parameter.1.0
  in
  in ([]: instruction, (updated_identities, new_id))

let update_owner (parameter: (id * address) * storage) =
  let (id: id), (new_owner: address) = paramater.0 in
  let identities = parameter.1.0 in
  let current_id_details = Bip_map.find_opt id identities in
  let is_allowed: bool =
    if sender = current_id_details.owner
    then true
    else failwith "You are not the owner of the ID " ^ id
  in
  let updated_id_details = {
    owner = new_owner;
    controller = current_id_details.controller;
    profile = current_id_details.profile;
  }
  in
  let updated_identities = Big_map.update id updated_id_details identities in
  ([]: instruction, updated_identities)

let update_details (parameter: (id * bytes * address option) * storage) =
  let current_id_details = Big_map.find_opt parameter.0.0 parameter.1.0 in
  let is_allowed: bool =
    if
      match current_id_details with
      | Some id_details -> (sender = id_details.controller) || (sender = id_details.owner)
      | None -> failwith ("No such ID " + parameter.0.0)
    then true
    else failwith ("You are not the owner or controller of the ID " ^ parameter.0.0)
  in
  let identities: (id, id_details) big_map = parameter.1.0 in
  let (id: id), (profile: bytes), (controller: address) = parameter.0 in
  let owner: address = current_id_details.owner in
  let controller: address =
    match parameter.0.2 with
    | None -> (* Default *) current_id_details.controller
    | Some new_controller -> new_controller
  in
  let updated_id_details = {
    owner = owner;
    controller = controller;
    profile = profile;
  }
  let updated_identities = Big_map.update id updated_id_details identities in
  ([]: instruction, updated_identities)

(* Let someone skip the next identity so nobody has to take one that's undesirable *)
let skip (p: unit) = ()

let whois_id (parameter: id * storage) =
  let query: id = parameter.0 in
  let identities: (id, id_details) big_map = parameter.1.0 in
  let result: id_details =
    match Big_map.find_opt query identities with
    | Some id_details -> id_details
    | None -> failwith "This ID doesn't exist in the system."
  in () (* Not entirely sure how to return the result here *)
