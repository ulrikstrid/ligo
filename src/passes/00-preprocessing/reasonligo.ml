(* Interfacing the ReasonLIGO preprocessor. *)

module Common   = Preproc_shared.Common
module File     = Preproc_reasonligo.File
module Comments = Preproc_reasonligo.Comments
module Preproc  = Common.MakePreproc (File) (Comments)

let preprocess = Preproc.preprocess_file
let preprocess_string = Preproc.preprocess_string
