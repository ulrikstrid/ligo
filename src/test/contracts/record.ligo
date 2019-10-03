// Test record type in PascaLIGO

type abc is record
  a : int ;
  b : int ;
  c : int ;
end

function modify_abc (const r : abc) : abc is
  block {
    r.b := 2048 ;
  } with r
