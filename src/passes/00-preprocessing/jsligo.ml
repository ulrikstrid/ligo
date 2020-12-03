(* Interfacing the JsLIGO preprocessor. *)

(* Vendors dependencies *)

module Trace = Simple_utils.Trace
type success = Preprocessor.API.success

(* Internal dependencies *)

module File       = Preprocessing_jsligo.File
module Comments   = Preprocessing_jsligo.Comments
module Common     = Preprocessing_shared.Common
module Preprocess = Common.MakePreproc (File) (Comments)

type error   = Preprocessing_shared.Errors.t

(* Preprocessing *)

type result    = (success, error) Trace.result
type file_path = string
type dirs      = file_path list (* For #include and #import *)

let preprocess_file   = Preprocess.from_file
let preprocess_string = Preprocess.from_string
