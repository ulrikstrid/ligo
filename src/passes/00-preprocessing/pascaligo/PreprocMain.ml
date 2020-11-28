(* Driving the preprocessor for PascaLIGO *)

module Comments       = Preproc_pascaligo.Comments
module File           = Preproc_pascaligo.File
module PreprocMainGen = Preproc_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Comments) (File)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
