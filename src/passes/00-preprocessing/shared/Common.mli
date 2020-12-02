(* PREPROCESSING *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Directories and files *)

type file_path = string
type dirs = file_path list (* #include and #import *)

(* Results *)

type success = Preprocessor.API.success
type error   = Errors.preproc_error
type result  = (success, error) Trace.result

module MakePreproc (File : File.S) (Comments : Comments.S) :
  sig
    val from_file    : dirs -> file_path  -> result
    val from_string  : dirs -> string     -> result
    val from_channel : dirs -> in_channel -> result
  end

(* For further passes *)

module type FILE =
  sig
    include File.S
    val input : file_path option
    val dirs  : dirs
  end
