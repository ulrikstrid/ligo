(* Driving the preprocessor for ReasonLIGO *)

module Comments       = Preproc_reasonligo.Comments
module File           = Preproc_reasonligo.File
module PreprocMainGen = Preproc_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Comments) (File)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
