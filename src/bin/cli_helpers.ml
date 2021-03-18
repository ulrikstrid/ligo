open Cmdliner
open Main.Display

let return_good v = `Ok v
let return_bad v = (
  if v.[String.length v - 1] = '\n' then
    Format.eprintf "%s" v
  else
    Format.eprintf "%s\n" v;
  Format.pp_print_flush Format.err_formatter ();
  `Error (false, "")
  )

let toplevel : ?warn:bool -> ?output_file:string option -> display_format:ex_display_format -> displayable -> ('value, _) Trace.result -> unit Term.ret =
  fun ?(warn=false) ?(output_file=None) ~display_format disp value ->
    let (Ex_display_format t) = display_format in
    let as_str : string =
      match t with
      | Human_readable -> convert ~display_format:t disp ;
      | Dev -> convert ~display_format:t disp ;
      | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp
    in
    match Trace.to_stdlib_result value with
    | Ok (_, a) ->
      let fmt : Format.formatter = match output_file with
        | Some file_path -> Format.formatter_of_out_channel @@ open_out file_path
        | None -> Format.std_formatter
      in
      if not (List.is_empty a) && warn then
        (match t with
         | Human_readable | Dev as t ->
            Format.eprintf "%a\n" (Simple_utils.PP_helpers.list_sep (Main_errors.Formatter.error_format.pp ~display_format:t) (Simple_utils.PP_helpers.tag "")) a;
            Format.pp_print_flush Format.err_formatter ()
         | Json -> let _json = a |> List.map Main_errors.Formatter.error_format.to_json in
                   let s = Yojson.Safe.pretty_to_string @@ `List _json in
                   Format.eprintf "%s\n" s;
                   Format.pp_print_flush Format.err_formatter ());
      return_good @@ (Format.fprintf fmt "%s\n" as_str;
                      Format.pp_print_flush fmt ())
    | Error _ -> return_bad as_str

let return_result : ?warn:bool -> ?output_file:string option -> display_format:ex_display_format -> 'value format -> ('value, Main_errors.all) Trace.result -> unit Term.ret =
  fun ?(warn=false) ?(output_file=None) ~display_format value_format value ->
    let format = bind_format value_format Main_errors.Formatter.error_format in
    toplevel ~warn ~output_file ~display_format (Displayable {value ; format}) value
