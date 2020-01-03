type storage_ is timestamp

function main(const p : unit; const s : storage_) : list(operation) * storage_ is
  block {
    // var toto : timestamp := ("alalal00" : timestamp);
    var toto : timestamp := ("2000-01-01T10:10:10Z": timestamp) ;
  }
  with ((nil: list(operation)), toto)