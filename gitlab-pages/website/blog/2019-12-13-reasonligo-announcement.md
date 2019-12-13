---
title: ReasonLIGO announcement
author: Sander Spies
---

This week we added support for ReasonLIGO, a new syntax for LIGO that closely resembles [ReasonML](https://reasonml.github.io). ReasonML allows you to write simple, fast, and high quality code that looks like JavaScript.

Here's an example of a crowdfunding contract written in ReasonLIGO. 

**Disclaimer***: Do **not** use this contract for an actual crowdfunding.*
```reasonligo
type store = {
  goal: tez,
  deadline: timestamp,
  backers: big_map (address, tez),
  funded: bool
};

let back = (store: store) => {
  let now = Current.time;
  if (now > store.deadline) {
    failwith("Deadline passed");
  } 
  else {
    let opt_sender = Big_map.find_opt(sender, store.backers);
    switch (opt_sender) {
      | None => {
        let amount = Current.amount;
        let backers = Big_map.update(sender, Some(amount), store.backers);
        ([]: list(operation), {
            goal: store.goal,
            deadline: store.deadline,
            backers,
            funded: store.funded
        });
      }
      | Some (s) => ([]: list(operation), store)
    };
  } 
};

let claim = (store: store) => {
  let receiver: contract(unit) = Operation.get_contract(sender);
  let now = Current.time;
  if (now <= store.deadline) {
    failwith("Too soon.");
  }
  else {
    let opt_sender = Big_map.find_opt(sender, store.backers);
    switch(opt_sender) {
      | None => failwith("Not a backer.")
      | Some (amount) => {
        if (amount >= store.goal || store.funded) {
          failwith("Goal reached: no refund.");
        }
        else {
          let backers = Big_map.remove(sender, store.backers);
          ([]: list(operation), {
            goal: store.goal,
            deadline: store.deadline,
            backers,
            funded: store.funded
          });
        }
      }
    };
  }
};

let withdraw = (store: store) => {
  let source = Current.source;
  let receiver: contract(unit) = Operation.get_contract(source);
  let now = Current.time;
  if (sender == source) {
    if (now >= store.deadline) {
      let amount = Current.amount;
      if (amount >= store.goal) {
        ([Operation.transaction (unit, contract, amount)], 
          {
            goal: store.goal,
            deadline: store.deadline,
            backers: store.backers,
            funded: true
          }
        );
      }
      else {
        failwith("Below target.");
      };
    }
    else {
      failwith("Too soon.");
    };
  } else {
    ([]: list(operation), store);
  }
};
```

## Differences from ReasonML

With respect to ReasonML, ReasonLIGO features accessing tuple components by means of indices (natural literals).

ReasonLIGO does not support JSX, attributes, polymorphic variants, OOP, arrays, modules, exceptions, and probably more.

## Feedback

We would love to know what you think of this.

[Documentation](https://ligolang.org/)

[Report an issue](https://gitlab.com/ligolang/ligo/issues/new?issue%5Bassignee_id%5D=&issue%5Bmilestone_id%5D=)