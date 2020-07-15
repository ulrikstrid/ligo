/* Simple test of binding multiple values */

let ((x : int), (y : int)) = (1,2);

let main = (p : unit): int => x + y;

let ((x : int), (y : int)) = (3,3);

let main_paren = (p : unit): int => x + y;

let foobar : (int, int) = (23, 42);

let ((foo : int), (bar : int)) = foobar;

let non_tuple_rhs = (p : unit) : int => foo + bar;


let o = 1
let o2 = 2
let o3 = 3
let ((x: int), ((y: int), (z: int))) = (o2, (o, o3));