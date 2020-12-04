(* Interfacing the CameLIGO preprocessor *)

module Trace = Simple_utils.Trace

type success = Preprocessor.API.success
type errors  = Preprocessing_shared.Errors.t
type result  = (success, errors) Trace.result

type file_path = string
type dirs      = file_path list (* For #include and #import *)

val preprocess_file   : dirs -> file_path -> result
val preprocess_string : dirs -> file_path -> result
