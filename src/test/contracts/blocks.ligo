// Test that sub blocks work in PascaLIGO

function sub_block_simple (var n : nat) : nat is
  begin 
    block { n := n + 10;
      block { n := n + 10; } } 
  end with n

function sub_block_triple (var n : nat) : nat is
  begin 
    block { n := n + 10;
      block { n := n + 10;
        block { n := n + 10; } } } 
  end with n

function sub_block_shadow (var n : nat ) : nat is
  begin 
    var y : int := n;
    block { 
            var n : int := 0;
      block { n := 10;
        block { n := n + 10; 
                y := n; } } }
  end with y
