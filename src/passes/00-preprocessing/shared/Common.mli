(* PREPROCESSING *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Directories and files *)

type file_path = string
type dirs = file_path list (* #include and #import *)

module Make (File : File.S) (Comments : Comments.S) :
  sig
    (* Results *)

    type success = Preprocessor.API.success
    type result  = (success, Errors.t) Trace.result

    (* Preprocessing various sources *)

    val from_file    : dirs -> file_path  -> result
    val from_string  : dirs -> string     -> result
    val from_channel : dirs -> in_channel -> result

    (* Aliases *)

    val preprocess_file    : dirs -> file_path  -> result
    val preprocess_string  : dirs -> string     -> result
    val preprocess_channel : dirs -> in_channel -> result
  end

(* For further passes *)

module type FILE =
  sig
    include File.S
    val input : file_path option
    val dirs  : dirs
  end
