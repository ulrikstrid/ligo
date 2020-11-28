(* Interfacing the PascaLIGO preprocessor. *)

module Common   = Preproc_shared.Common
module File     = Preproc_pascaligo.File
module Comments = Preproc_pascaligo.Comments
module Preproc  = Common.MakePreproc (File) (Comments)

let preprocess = Preproc.preprocess_file
let preprocess_string = Preproc.preprocess_string
