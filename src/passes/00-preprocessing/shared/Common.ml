(* Interfacing the preprocessor. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* CONFIGURATION *)

type file_path = string

module type FILE =
  sig
    val input     : file_path option
    val extension : string            (* No option here *)
    val dirs      : file_path list    (* #include *)
  end

module Config (File : FILE) (Comments : Comments.S) =
  struct
    (* Stubs for the libraries CLIs *)

    module Preproc_CLI : Preprocessor.CLI.S =
      struct
        include Comments

        let input     = File.input
        let extension = Some File.extension
        let dirs      = File.dirs
        let show_pp   = false
        let offsets   = true

        type status = [
          `Done
        | `Version      of string
        | `Help         of Buffer.t
        | `CLI          of Buffer.t
        | `SyntaxError  of string
        | `FileNotFound of string
        ]

        let status = `Done
      end

    (* Configurations for the preprocessor based on the
       librairies CLIs. *)

    let preproc =
      object
        method block   = Preproc_CLI.block
        method line    = Preproc_CLI.line
        method input   = Preproc_CLI.input
        method offsets = Preproc_CLI.offsets
        method dirs    = Preproc_CLI.dirs
      end
  end

(* PREPROCESSING *)

type dirs = file_path list (* For #include directives *)

(* Results *)

type success = Preprocessor.API.success
type error   = Errors.preproc_error
type result  = (success, error) Trace.result

let fail msg = Trace.fail @@ Errors.generic msg
module MakePreproc (File : File.S) (Comments : Comments.S) =
  struct
    (* Preprocessing a contract in a file *)

    let from_file dirs file_path =
      let module File =
        struct
          let input     = Some file_path
          let extension = File.extension
          let dirs      = dirs
        end in
      let module Config = Config (File) (Comments) in
      let preprocessed =
        Preprocessor.API.from_file Config.preproc file_path
      in match preprocessed with
           Stdlib.Error (_, msg) -> fail msg
         | Ok (buffer, deps) ->
             let string = Buffer.contents buffer in
             if Config.Preproc_CLI.show_pp then
               Printf.printf "%s\n%!" string;
             Trace.ok (buffer, deps)

    (* Preprocessing from a string *)

    let from_string dirs string =
      let module File =
        struct
          let input     = None
          let extension = File.extension
          let dirs      = dirs
        end in
      let module Config = Config (File) (Comments) in
      let preprocessed =
        Preprocessor.API.from_string Config.preproc string
      in match preprocessed with
           Stdlib.Error (_, msg) -> fail msg
         | Ok (buffer, deps) ->
             let string = Buffer.contents buffer in
             if Config.Preproc_CLI.show_pp then
               Printf.printf "%s\n%!" string;
             Trace.ok (buffer, deps)

    (* Preprocessing from a channel *)

    let preprocess_channel dirs channel =
      let module File =
        struct
          let input     = None
          let extension = File.extension
          let dirs      = dirs
        end in
      let module Config = Config (File) (Comments) in
      let preprocessed =
        Preprocessor.API.from_channel Config.preproc channel
      in match preprocessed with
           Stdlib.Error (_, msg) -> fail msg
         | Ok (buffer, deps) ->
             let string = Buffer.contents buffer in
             if Config.Preproc_CLI.show_pp then
               Printf.printf "%s\n%!" string;
             Trace.ok (buffer, deps)
  end
