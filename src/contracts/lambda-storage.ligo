type funtype is (int -> int)

function main (const p : int ; const s : funtype) : (list(operation) * funtype) is
  block { skip }
  with ((nil : list(operation)), s)

