---
id: testing-ligo-contracts
title: Testing Your LIGO Contract
---

## Introduction

In the world of smart contracts extensive testing is more or less mandatory.
Unlike most forms of modern software, it is usually not possible to "patch" a
smart contract if something goes wrong. Once put on-chain a smart contract can
only be amended by a new version. Smart contracts are often controlling significant
financial resources or assets. Because of this the consequences of implementation
flaws can be devastating. This makes the development of smart contracts very
different from most other forms of software. Developers have become used to the
expectation that they should be extreme generalists who know how to do many things
competently. This is **not** your task when writing a smart contract. Writing a
smart contract is about doing simple things flawlessly.

LIGO does not yet have the ability to host its own testing framework. To test our
contract we will have to either use a Tezos node or external test tools used as
part of developing the LIGO compiler.

## Testing with a Tezos Node

(Coming soon!)

## Testing with OCaml and the LIGO test suite

### Setting Up A LIGO Development Environment

First, clone the git repository for LIGO:

    git clone https://gitlab.com/ligolang/ligo.git
    cd ligo

Install the dependencies you'll need to build LIGO:

    scripts/install_build_environment.sh

    sudo scripts/install_native_dependencies.sh

Run the makefile to build LIGO:

    make

You can test that you have the compiler working by running: 

    _build/default/src/bin/runligo.exe changelog

### Getting Your Contract Into The Build Chain

Before you can test your contract using the LIGO test tools, you have to get it
into the path of the LIGO build system. The first step is to copy the contract you
want to test into `src/test/contracts/`. If your contract uses `#include` be sure
to copy all the parts into this directory.

Next you'll want to create a file `src/test/my_tests.ml`:

```ocaml

(* my_tests.ml *)

open Trace
open Test_helpers
open Ast_simplified

let type_file f =
  (* Change this line to use the name of your syntax, e.g. pascaligo, cameligo... *)
  let%bind simplified  = Ligo.Compile.Of_source.compile f (Syntax_name "cameligo") in
  let%bind typed,state = Ligo.Compile.Of_simplified.compile simplified in
  ok @@ (typed,state)

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        (* Change this line to use the relative path to your contract *)
        let%bind (program , state) = type_file "./contracts/vote.mligo" in
        s := Some (program , state) ;
        ok (program , state)
      )

let compile_main () =
  (* Change this line to use the relative path to your contract and your syntax name *)
  let%bind simplified      = Ligo.Compile.Of_source.compile "./contracts/vote.mligo" (Syntax_name "cameligo") in
  let%bind typed_prg,_ = Ligo.Compile.Of_simplified.compile simplified in
  let%bind mini_c_prg      = Ligo.Compile.Of_typed.compile typed_prg in
  (* Possibly change this line to use the entrypoint name of your contract *)
  let%bind michelson_prg   = Ligo.Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg "main" in
  let%bind (_contract: Tezos_utils.Michelson.michelson) =
    (* fails if the given entry point is not a valid contract *)
    Ligo.Compile.Of_michelson.build_contract michelson_prg in
  ok ()

(* Change this line to use a name you'll recognize for your test suite *)
let main = test_suite "My Contract Name" [
  ]
```

Then modify `src/test/test.ml` to include the main function of your test file:

```ocaml
let () =
  Printexc.record_backtrace true ;
  run_test @@ test_suite "LIGO" [
    Integration_tests.main ;
    Transpiler_tests.main ;
    Typer_tests.main ;
    Coase_tests.main ;
    Vote_tests.main ;
    Multisig_tests.main ;
    Multisig_v2_tests.main ;
    Replaceable_id_tests.main ;
    Time_lock_tests.main ;
    My_tests.main ;
  ] ;
  ()
```

You are now ready to begin testing your contract.

### Testing Your Contract

In order to demonstrate testing a contract, we'll need an example. Lets try
testing a simple access control module:

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo
(* access_control.ligo *)

type storage is record [
  owner: address;
  controller: address;
]

type auth is
| At_least_controller of address
| At_least_owner of address

function at_least_controller (const parameter: address; const storage: storage) : bool is
  begin
    var result: bool := False ;
    if (parameter = storage.owner) or (parameter = storage.controller)
    then result := True
    else result := False
  end with result

function at_least_owner (const parameter: address; const storage: storage) : bool is
  begin
    var result: bool := False ;
    if parameter = storage.owner
    then result := True
    else result := False
  end with result

function main (const parameter: auth; const storage: storage) : (list(operation) * bool) is
  begin
    var result : bool := False ;
    case parameter of
    | At_least_controller(addr) -> result := at_least_controller(addr, storage)
    | At_least_owner(addr) -> result := at_least_owner(addr, storage)
    end
  end with ((nil: list(operation)), result)
```

<!--CameLIGO-->

```cameligo
(* access_control.mligo *)

type storage = {
  owner: address;
  controller: address;
}

let at_least_controller (parameter, storage: address * storage) : bool =
  if (parameter = storage.owner || parameter = storage.controller)
  then true
  else false

let at_least_owner (parameter, storage: address * storage) : bool =
  if parameter = storage.owner
  then true
  else false

let main (parameter, storage: auth * storage) : bool =
  match parameter with
  | At_least_controller addr -> at_least_controller (addr, storage)
  | At_least_owner addr -> at_least_owner (addr, storage)

```

<!--ReasonLIGO-->

```reasonligo
type storage = {
  owner: address ,
  controller: address ,
};

type auth =
  | At_least_controller(address)
  | At_least_owner(address);


let at_least_controller = (ps: (address, storage)): bool =>
  let parameter, storage = ps;
  if (parameter == storage.owner || parameter == storage.controller) {
    true;
  }
  else {
    false;
  };

let at_least_owner = (ps: (address, storage)): bool =>
  let parameter, storage = ps;
  if (parameter == storage.owner) {
    true;
  }
  else {
    false;
  };

let main = (ps: (auth, storage)): (list(operation), bool) =>
  let parameter, storage = ps;
  let result: bool = 
    switch (parameter) {
    | At_least_controller(addr) => at_least_controller((addr, storage))
    | At_least_owner(addr) => at_least_owner((addr, storage))
    };
  (([]: list(operation)), result);
```

<!--END_DOCUSAURUS_CODE_TABS-->

To thoroughly test a contract like this we aim for full 
[code coverage](https://en.wikipedia.org/wiki/Code_coverage). Full code coverage does
not necessarily mean we've tested every case, but it's the bare minimum requirement
for us to have even *possibly* tested every case. So we have to consider every case
for these entrypoints.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo
function at_least_controller (const parameter: address; const storage: storage) : bool is
  begin
    var result: bool := False ;
    if (parameter = storage.owner) or (parameter = storage.controller)
    then result := True
    else result := False
  end with result
```

<!--CameLIGO-->

```cameligo
let at_least_controller (parameter, storage: address * storage) : bool =
  if (parameter = storage.owner || parameter = storage.controller)
  then true
  else false
```

<!--ReasonLIGO-->

```reasonligo
let at_least_controller = (ps: (address, storage)): bool =>
  let parameter, storage = ps;
  if (parameter == storage.owner || parameter == storage.controller) {
    true;
  }
  else {
    false;
  };
```

<!--END_DOCUSAURUS_CODE_TABS-->

The cases we have for this entrypoint are:

- We're neither the owner or the controller
- We're the owner
- We're the controller

Because this test requires us to change our identity between cases, we need to
manipulate the Tezos dummy environment to set up our tests.

```ocaml
(* my_tests.ml *)

...

let (us_addr , us_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt
```

This lets us extract a stable identity to use for ourselves between test cases.
Then in the tests we create a context:

```ocaml
(* my_tests.ml *)

...

let options = Proto_alpha_utils.Memory_proto_alpha.make_options
  ~payer:first_contract
  ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
```

This context varies between tests, but the extracted identity above should only
need to appear once per suite. It's passed to assertions like `expect_eq` as the
`~options` flag.

We can write a test for each case like so:

```ocaml
(* my_tests.ml *)

...

(* Test this returns false when we're not owner or controller *)
let at_least_controller_not () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let controller_addr = addr 4 in
  let storage = e_ez_record [("owner", e_address owner_addr) ;
                             ("controller", e_address controller_addr)]
  in
  let%bind () = expect_eq program "at_least_controller"
    (e_pair (e_address us_addr) storage)
    (e_bool false)
  in ok ()

(* Test this returns true when we're controller *)
let at_least_controller_c () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let controller_addr = us_addr in
  let storage = e_ez_record [("owner", e_address owner_addr) ;
                             ("controller", e_address controller_addr)]
  in
  let%bind () = expect_eq program "at_least_controller"
    (e_pair (e_address us_addr) storage)
    (e_bool true)
  in ok ()

(* Test this returns true when we're owner *)
let at_least_controller_owner () =
  let%bind program, _ = get_program () in
  let owner_addr = us_addr in
  let controller_addr = addr 4 in
  let storage = e_ez_record [("owner", e_address owner_addr) ;
                             ("controller", e_address controller_addr)]
  in
  let%bind () = expect_eq program "at_least_controller"
    (e_pair (e_address us_addr) storage)
    (e_bool true)
  in ok ()
```

Once we're ready to try running our tests, entries have to be added to the test
file's main function:

```ocaml
(* my_tests.ml *)

...

(* Change this line to use a name you'll recognize for your test suite *)
let main = test_suite "My Contract Name" [
    test "at_least_controller false when not" at_least_controller_not ;
    test "at_least_controller true when controller" at_least_controller_c ;
    test "at_least_controller true when owner" at_least_controller_owner ;
  ]
```

Then run the makefile at the top of the LIGO source tree, and you should see
pass/fail for your tests:

```
[OK]            Access Control                 0   at_least_controller false when not.
[OK]            Access Control                 1   at_least_controller true when controller.
[OK]            Access Control                 2   at_least_controller true when owner.
```
