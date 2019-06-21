Set Implicit Arguments.

Tactic Notation "dbg''" tactic1(show_dashes_and_name) :=
  idtac "";
  repeat(
  try(match reverse goal with
      | [ H : ?t |- _ ] => idtac H ":" t; fail
      | [ H := ?v : ?t |- _ ] => idtac H ":=" v ":" t; fail
      | [ |- ?G ] => show_dashes_and_name; idtac G; idtac ""
      end)).

Inductive displayName := DisplayName (e : nat).
Notation "x" := (let x := 0 in DisplayName x)
                (at level 200, only printing).

Tactic Notation "dbg" uconstr(invalid_name) :=
  dbg'' (idtac "------" invalid_name).
Tactic Notation "dbg" ident(invalid_name) :=
  dbg'' (idtac "------" invalid_name).
Tactic Notation "dbg" :=
  dbg'' (idtac "------").

Inductive do_dbg :=
| Do_dbg
| Do_dbg_named (nm : displayName).

Ltac dbgn :=
  lazymatch goal with
  | H := ?h |- _ =>
    lazymatch h with
    | (let nm := 0 in DisplayName nm) =>
      clear H;
      dbg nm
    | _ => fail 99999 "dbgn: internal error"
    end
  | _ => fail 99999 "dbgn: internal error"
  end.

Ltac program_dbg :=
  lazymatch goal with
  | |- (let x : do_dbg := Do_dbg in ?G) =>
    let H := fresh "H" in
    intro H;
    clear H;
    dbg
  | |- (let x : do_dbg := Do_dbg_named (let nm := 0 in DisplayName nm) in ?G) =>
    let H := fresh "H" in
    intro H;
    clear H;
    dbg nm
  | _ => idtac
  end.

Require Import Coq.Program.Tactics.
Show Obligation Tactic.
Obligation Tactic := program_dbg; program_simpl.

Notation "<?>" := ltac:(dbg; refine(_)) (only parsing).

Notation "<? invalid_name ?>" :=
  (let _ := (let invalid_name := 0 in DisplayName invalid_name) in ltac:(dbgn; refine(_)))
  (only parsing).

Notation "<??>" := (let x := Do_dbg in _) (only parsing).
Notation "y" := (let x := Do_dbg in y) (only printing).

Notation "<?? name ??>" := (let _ := Do_dbg_named (let name := 0 in DisplayName name) in _) (only parsing).
Notation "y" := (let x := Do_dbg_named _ in y) (only printing).
