let a = 1

let b = Test.set_now ("2000-01-01T10:10:10Z" : timestamp)

let i = 3

let f = fun (i : int) -> i + 40

let my_test =
  let whatever = Test.run f 2 in
  let _ = Test.log whatever in
  (*
  tries to type the body of f ("i + 2") with: 
   - env (types) = edo_types + object_env[types]`
   - env (values) = object_env[values]
  *)
  let _ = b in
  let _ = Test.log whatever in
  a
