(* Interfacing the PascaLIGO preprocessor. *)

module Trace = Simple_utils.Trace

type success = Preprocessor.API.success
type error   = Preprocessing_shared.Errors.preproc_error
type result  = (success, error) Trace.result

type file_path = string
type dirs      = file_path list (* For #include and #import *)

module Common     = Preprocessing_shared.Common
module File       = Preprocessing_pascaligo.File
module Comments   = Preprocessing_pascaligo.Comments
module Preprocess = Common.MakePreproc (File) (Comments)

let preprocess_file   = Preprocess.from_file
let preprocess_string = Preprocess.from_string
