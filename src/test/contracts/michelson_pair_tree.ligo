type inner_storage is michelson_pair(int,"one",nat,"two")
type storage is michelson_pair (string,"three",inner_storage,"four")

type return is list(operation) * storage

function main (const action : unit; const store : storage) : return is block {
  const foo : storage = ("foo",(1,2n)) ;
} with ((nil : list(operation)), (foo: storage))


{ parameter unit ;
  storage (pair (pair %four (nat %two) (int %one)) (string %three)) ;
  code { /* [ pair (unit @parameter)
                 (pair @storage (pair %four (nat %two) (int %one)) (string %three)) ] */
         DROP
         /* [] */ ;
         PUSH nat 2
         /* [ nat ] */ ;
         PUSH int 1
         /* [ int : nat ] */ ;
         PAIR
         /* [ pair int nat ] */ ;
         PUSH string "foo"
         /* [ string : pair int nat ] */ ;
         PAIR
         /* [ pair string (pair int nat) ] */ ;
         NIL operation
         /* [ list operation : pair string (pair int nat) ] */ ;
         PAIR
         /* [ pair (list operation) (pair string (pair int nat)) ] */ } }