function foo (const i : int) : int is
  block {
    function bar (const j : int) : int is j
  } with bar (0)
