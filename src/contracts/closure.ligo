type r is record
  i : int ;
  j : nat ;
  k : string ;
end

function foo (const i : int) : int is
  function bar (const j : nat) : int is
    function baz (const k : string) : int is
      const r : r = record i = i + i ; j = j ; k = k end;
      block { skip }
      with r.i;
    block { skip }
    with baz("foo");
  block { skip }
  with bar(0n)

function toto (const i : int) : int is
  function tata (const j : int) : int is
    block { skip } with i + j ;
  function titi (const j : int) : int is
    block { skip } with i + j ;
  block { skip } with tata(i) + titi(i)
