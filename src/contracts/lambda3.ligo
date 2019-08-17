type some_type is int

type tf is (int -> int)

function main (const p : int ; const s : tf) : (list(operation) * tf) is
  block { skip } // skip is a do nothing instruction, needed for empty blocks
  with ((nil : list(operation)), s)

