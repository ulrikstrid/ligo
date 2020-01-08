function cb(const a : address; const s : unit) : list(operation) * unit is
  block {
    const c : contract(unit) = get_entrypoint("%cb", a)
  }
  with (list transaction(unit, 0mutez, c) end, s)

function cb2(const a: address; const s : unit) : list (operation) * unit is
  block {
    var c : contract(unit) := get_entrypoint("%cb2", a);
    case (get_entrypoint_opt("%cb2", a): option(contract(unit))) of
    | Some (s) -> c := s
    | None -> failwith("No entrypoint")
    end
  }
  with (list transaction(unit, 0mutez, c) end, s)
