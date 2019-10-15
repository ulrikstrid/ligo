// Test that sub blocks work in PascaLIGO

function sub_block_simple (var n : nat) : nat is
  begin 
    block { n := n + 10;
      block { n := n + 10; } } 
  end with n
