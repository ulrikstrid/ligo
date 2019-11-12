open Mini_c

(* Computes `body[x := expr]` *)
let rec subst_expression : body:expression -> x:var_name -> expr:expression -> expression =
  fun ~body ~x ~expr ->
  let self body = subst_expression ~body ~x ~expr in
  let return content = {body with content} in
  let return_id = body in
  match body.content with
  | E_variable x' ->
     if x' = x
     then expr
     else return_id
  | E_closure af -> (
    if af.binder <> x
    then (
      let body = self af.body in
      return @@ E_closure { af with body }
    ) else
      return_id
  )
  | E_let_in ((v , tv) , expr , body) -> (
    if v <> x
    then (
      let (expr',body') = Tuple.map2 self (expr,body) in
      return @@ E_let_in ((v , tv) , expr' , body')
    )
    else
      return_id
  )
  | E_iterator (s, ((name , tv) , body) , collection) -> (
    if name <> x then (
      let (exp',body') = Tuple.map2 self (collection , body) in
      return @@ E_iterator (s, ((name , tv) , body') , exp')
    ) else (
      let exp' = self collection in
      return @@ E_iterator (s, ((name , tv) , body) , exp')
    )
  )
  | E_fold (((name , tv) , body) , collection , init) -> (
    if name <> x then (
      let (body' , col' , init') = Tuple.map3 self (body , collection , init) in
      return @@ E_fold (((name , tv) , body') , col', init')
    ) else (
      let (col' , init') = Tuple.map2 self (collection , init) in
      return @@ E_fold (((name , tv) , body) , col', init')
    )
  )
  (* All that follows is boilerplate *)
  | E_literal _ | E_skip | E_make_none _
  | E_make_empty_map (_,_) | E_make_empty_list _ | E_make_empty_set _ as em -> return em
  | E_constant (name, lst) -> (
      let lst' = List.map self lst in
      return @@ E_constant (name,lst')
  )
  | E_application farg -> (
      let farg' = Tuple.map2 self farg in
      return @@ E_application farg'
  )
  | E_while eb -> (
      let eb' = Tuple.map2 self eb in
      return @@ E_while eb'
  )
  | E_if_bool cab -> (
      let cab' = Tuple.map3 self cab in
      return @@ E_if_bool cab'
  )
  | E_if_none (c, n, ((name, tv) , s)) -> (
      let (c',n',s') = Tuple.map3 self (c,n,s) in
      return @@ E_if_none (c', n', ((name, tv) , s'))
  )
  | E_if_cons (c, n, (((hd, hdtv) , (tl, tltv)) , cons)) -> (
      let (c',n',cons') = Tuple.map3 self (c,n,cons) in
      return @@ E_if_cons (c', n', (((hd, hdtv) , (tl, tltv)) , cons'))
  )
  | E_if_left (c, ((name_l, tvl) , l), ((name_r, tvr) , r)) -> (
      let (c',l',r') = Tuple.map3 self (c,l,r) in
      return @@ E_if_left (c', ((name_l, tvl) , l'), ((name_r, tvr) , r'))
  )
  | E_sequence ab -> (
      let ab' = Tuple.map2 self ab in
      return @@ E_sequence ab'
  )
  | E_assignment (s, lrl, exp) -> (
      let exp' = self exp in
      return @@ E_assignment (s, lrl, exp')
  )
