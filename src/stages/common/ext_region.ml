module Region = Simple_utils.Region

type t = {
  t_region: Region.t;
  markup: markup list
}

and 'a reg = { region: t; value: 'a }

and markup = 
  BlockCom of string reg * comment_position
| LineCom of string reg * comment_position

and comment_position = 
  Before
| After
| Inline

let make (region: Region.t) markup =
  {
    t_region = region;
    markup
  }
let ghost = 
  make (Region.make ~start:Pos.ghost ~stop:Pos.ghost) []

let wrap_ghost value = 
  {value ; region = ghost}

let cover r1 r2 =
  make (Region.cover r1.t_region r2.t_region) []

