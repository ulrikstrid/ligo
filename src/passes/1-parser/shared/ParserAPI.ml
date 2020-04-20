(* Generic parser for LIGO *)

module Region = Simple_utils.Region

type options = <
  offsets : bool;
  mode    : [`Byte | `Point];
  cmd     : EvalOpt.command
>

module type IO =
  sig
    val options : options
  end

module type PARSER =
  sig
    (* The type of tokens, abstract syntax trees and expressions *)

    type token
    type ast
    type expr

    (* This exception is raised by the monolithic API functions. *)

    exception Error

    (* The monolithic API. *)

    val interactive_expr :
      (Lexing.lexbuf -> token) -> Lexing.lexbuf -> expr

    val contract :
      (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ast

    module MenhirInterpreter :
      sig
        (* The incremental API. *)

        include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
                with type token = token
    end

    (* The entry point(s) to the incremental API. *)

    module Incremental :
      sig
        val interactive_expr :
          Lexing.position -> expr MenhirInterpreter.checkpoint

        val contract :
          Lexing.position -> ast MenhirInterpreter.checkpoint
      end
  end

(* Main functor *)

module Make (IO: IO)
            (Lexer: Lexer.S)
            (Parser: PARSER with type token = Lexer.Token.token)
            (ParErr: sig val message : int -> string end) =
  struct
    module I = Parser.MenhirInterpreter
    module S = MenhirLib.General (* Streams *)

    (* The call [stack checkpoint] extracts the parser's stack out of
       a checkpoint. *)

    let stack = function
      I.HandlingError env -> I.stack env
    |                   _ -> assert false

    (* The call [state checkpoint] extracts the number of the current
       state out of a parser checkpoint. *)

    let state checkpoint : int =
      match Lazy.force (stack checkpoint) with
        S.Nil -> 0 (* WARNING: Hack. The first state should be 0. *)
      | S.Cons (I.Element (s,_,_,_),_) -> I.number s

    (* The parser has successfully produced a semantic value. *)

    let success v = v

    (* The parser has suspended itself because of a syntax error. Stop. *)

    type message = string
    type valid   = Parser.token
    type invalid = Parser.token
    type error   = message * valid option * invalid

    exception Point of error

    let format_error ?(offsets=true) mode (msg, valid_opt, invalid) =
      let invalid_region = Lexer.Token.to_region invalid in
      let header =
        "Parse error " ^ invalid_region#to_string ~offsets mode in
      let trailer =
        match valid_opt with
          None ->
            if Lexer.Token.is_eof invalid then ""
            else let invalid_lexeme = Lexer.Token.to_lexeme invalid in
                 Printf.sprintf ", at \"%s\"" invalid_lexeme
        | Some valid ->
            let valid_lexeme = Lexer.Token.to_lexeme valid in
            if Lexer.Token.is_eof invalid then
              Printf.sprintf ", after \"%s\"" valid_lexeme
            else
              let invalid_lexeme = Lexer.Token.to_lexeme invalid in
              Printf.sprintf " at \"%s\", after \"%s\""
                             invalid_lexeme valid_lexeme in
      let header = header ^ trailer in
      let msg =
        header ^ (if msg = "" then ".\n" else ":\n" ^ msg)
      in Region.{value=msg; region=invalid_region}

    let extract text (pos1, pos2) : string =
      let ofs1 = pos1.Lexing.pos_cnum + 1
      and ofs2 = pos2.Lexing.pos_cnum + 1 in
      let len = ofs2 - ofs1 in
      try
        String.sub text ofs1 len
      with Invalid_argument _ ->        
        "??"

    let fragment text get_win checkpoint msg = 
      let i = int_of_string (Str.matched_group 1 msg) in
      if i = 0 then (
        let invalid = match get_win () with
          Lexer.Nil -> assert false
        | Lexer.One invalid ->
          invalid
        | Lexer.Two (invalid, _) ->
          invalid
        in 
        let invalid_region = Lexer.Token.to_region invalid in
        extract text (invalid_region#start#byte, invalid_region#stop#byte)
      )
      else (
        match checkpoint with 
        | I.HandlingError env -> ( 
          let e = I.get (i - 1) env in
          match e with 
          | Some (I.Element (_,_, pos1, pos2)) ->
            extract text (pos1, pos2)
          | _ -> msg
        )
        | _ -> msg
      )
      

    let augment text get_win checkpoint msg = 
      Str.global_substitute
        (Str.regexp "\\$\\([0-9]+\\)")
        (fragment text get_win checkpoint)
        msg

    let failure lexbuf get_win checkpoint =
      let text = Bytes.to_string lexbuf in
      let message = 
        ParErr.message (state checkpoint) 
        |> augment text get_win checkpoint
      in
      let message = if message = "<YOUR SYNTAX ERROR MESSAGE HERE>\n" then
        (string_of_int (state checkpoint)) ^ ": <syntax error>"
      else
        message
      in
      match get_win () with
        Lexer.Nil -> assert false
      | Lexer.One invalid ->
          raise (Point (message, None, invalid))
      | Lexer.Two (invalid, valid) ->
          raise (Point (message, Some valid, invalid))

    (* The monolithic API of Menhir *)

    let mono_contract = Parser.contract

    let mono_expr = Parser.interactive_expr

    (* Incremental API of Menhir *)

    module Incr = Parser.Incremental

    module Log = LexerLog.Make (Lexer)
    let log    = Log.output_token
                   ~offsets:IO.options#offsets
                   IO.options#mode IO.options#cmd stdout

    let incr_contract Lexer.{read; buffer; get_win; close; _} =
      let supplier  = I.lexer_lexbuf_to_supplier (read ~log) buffer
      and failure   = failure buffer.Lexing.lex_buffer get_win in
      let parser    = Incr.contract buffer.Lexing.lex_curr_p in
      let ast       = I.loop_handle success failure supplier parser
      in flush_all (); close (); ast

    let incr_expr Lexer.{read; buffer; get_win; close; _} =
      let supplier   = I.lexer_lexbuf_to_supplier (read ~log) buffer
      and failure    = failure buffer.Lexing.lex_buffer get_win in
      let parser     = Incr.interactive_expr buffer.Lexing.lex_curr_p in
      let expr       = I.loop_handle success failure supplier parser
      in flush_all (); close (); expr
  end
