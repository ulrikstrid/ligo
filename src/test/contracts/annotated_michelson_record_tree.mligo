type comb_two = {
  foo : int [@@annot:anfoo] ;
  bar : string [@@annot:anbar] ;
} [@layout:tree]

type comb_three = {
  a : int [@@annot:ana] ;
  b : string [@@annot:anb] ;
  c : nat [@@annot:anc] ;
} [@layout:tree]

type comb_five = {
  one : int [@@annot:an_One] ;
  two : string [@@annot:an_Two] ;
  three : bool [@@annot:an_Three] ;
  four : nat [@@annot:an_Four] ;
  five : int [@@annot:an_Five] ;
} [@layout:tree]

type parameter = unit
type op_list = operation list

let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
  let o = store.foo in
  let oo = { store with foo = o } in
  ([] : operation list), oo

let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
  ([] : operation list), { a = 1 ; b = "" ; c = 1n }

let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
  ([] : operation list), store