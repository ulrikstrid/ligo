(* Interfacing the CameLIGO preprocessor. *)

module Common   = Preproc_shared.Common
module File     = Preproc_cameligo.File
module Comments = Preproc_cameligo.Comments
module Preproc  = Common.MakePreproc (File) (Comments)

let preprocess = Preproc.preprocess_file
let preprocess_string = Preproc.preprocess_string
