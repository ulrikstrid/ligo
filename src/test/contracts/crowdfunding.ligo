type store is
  record [
    goal     : tez;
    deadline : timestamp;
    backers  : map (address, tez);
    funded   : bool;
  ]

function back (var store : store) : list (operation) * store is
  var operations : list (operation) := list [];
  begin
    if now > store.deadline then
      failwith("Deadline passed");
    else
      case store.backers[sender] of
      [
      | None -> store.backers[sender] := amount
      | Some (s) -> skip
      ]
  end with (operations, store)

function claim (var store : store) : list (operation) * store is
  var operations : list (operation) := nil;
  const receiver : contract(unit) = get_contract(sender) ;
  begin
    if now <= store.deadline then
      failwith("Too soon.")
    else
      case store.backers[sender] of
        None ->
          failwith("Not a backer.")
      | Some (amount) ->
          if amount >= store.goal or store.funded then
            failwith("Goal reached: no refund.")
          else
            begin
              operations := list [transaction (unit, amount, receiver)];
              remove sender from map store.backers ;
            end
      end
  end with (operations, store)

function withdraw (var store : store) : list (operation) * store is
  var operations : list (operation) := list end
  const receiver : contract(unit) = get_contract(source) ;
  begin
    if sender = source then
      if now >= store.deadline then
        if amount >= store.goal then {
             store.funded := True;
             operations := list [transaction (unit, amount, receiver)];
        };
        else failwith("Below target.")
      else failwith("Too soon.");
    else skip
  end with (operations, store)
