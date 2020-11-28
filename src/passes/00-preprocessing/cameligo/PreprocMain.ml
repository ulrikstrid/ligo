(* Driving the preprocessor for CameLIGO *)

module Comments       = Preproc_cameligo.Comments
module File           = Preproc_cameligo.File
module PreprocMainGen = Preproc_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Comments) (File)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
