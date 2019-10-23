type section = int * string

type block = {
  line    : int;
  file    : string;
  section : section option;
  (* labels  :
    (string * ([`Eq | `Neq | `Le | `Lt | `Ge | `Gt] * string) option) list; *)
  header  : string option;
  contents: string list;
  (* value   : value; *)
}

type md = 
| Text of string
| Block of block
| Section of section
