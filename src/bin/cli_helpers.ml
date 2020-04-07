open Cmdliner
(* open Trace *)
open Main.Display
(* open Main.Errors_format *)

(* let error_suggest: string = "\n If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'\n" *)

(* let toplevel : type o .  display_format: o display_format -> displayable -> unit Term.ret =
  fun ~display_format disp ->
  let str = match display_format with
    | Human_readable -> convert ~display_format:(Human_readable) disp
    | Dev -> convert ~display_format:(Dev) disp
    | Json -> Yojson.Basic.to_string @@ to_json disp
  in
  print_string str ;
  `Ok () *)
  (* match value with
  | Ok _ -> `Ok ()
  | Error _ -> `Error (false, "error") *)

let toplevel : display_format:planque -> displayable -> unit Term.ret =
  fun ~display_format disp ->
  let str = display_format.tt { run =
    fun (type a) (cons : a display_format) ->
        match cons with
        | Human_readable -> convert ~display_format:(Human_readable) disp
        | Dev -> convert ~display_format:(Dev) disp
        | Json -> Yojson.Basic.to_string @@ to_json disp
    } in
  print_string str ;
  `Ok ()