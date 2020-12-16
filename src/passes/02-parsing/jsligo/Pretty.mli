(* A pretty printer for JsLIGO *)

type cst        = Cst.Jsligo.t

val print           : cst -> PPrint.document
