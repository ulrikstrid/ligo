module Foo = struct
  type baker_hash = string
  let foo = {foo = 1}
  let main ((),(p:int)) = 
    let a = p in
    ([]:operation list),a
end  

let foo = Foo.foo.foo

