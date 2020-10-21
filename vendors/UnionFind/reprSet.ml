include PolySet

let alias ~other_repr ~new_repr s =
  if PolySet.mem other_repr s then
    if PolySet.mem new_repr s then
      PolySet.remove other_repr s
    else
      PolySet.add new_repr (PolySet.remove other_repr s)
  else
    s
