open Cmdliner
open Trace

val toplevel : display_format : 'a Main.Display.display_format -> string result -> unit Term.ret
