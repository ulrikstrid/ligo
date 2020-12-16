[@@@warning "-42"]
[@@@coverage exclude_file]

type state = <
  offsets  : bool;
  mode     : [`Point | `Byte];
  buffer   : Buffer.t;
  pad_path : string;
  pad_node : string;
  pad      : int -> int -> state
>

let mk_state ~offsets ~mode ~buffer =
  object
    method offsets  = offsets;
    method mode     = mode;
    method buffer   = buffer
    val pad_path    = ""
    method pad_path = pad_path
    val pad_node    = ""
    method pad_node = pad_node

    (** The method [pad] updates the current padding, which is
        comprised of two components: the padding to reach the new node
        (space before reaching a subtree, then a vertical bar for it)
        and the padding for the new node itself (Is it the last child
        of its parent?).
     *)
    method pad arity rank =
      {< pad_path =
           pad_node ^ (if rank = arity-1 then "`-- " else "|-- ");
         pad_node =
           pad_node ^ (if rank = arity-1 then "    " else "|   ")
      >}
  end

let print_tokens = failwith "Printer.print_tokens: TODO"

let pp_cst = failwith "Printer.pp_cst"
