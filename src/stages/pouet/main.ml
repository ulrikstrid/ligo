open Myerror

let _ =
  let computations : (Bar.Types.expression, _) result =
    let%bind x = Foo.f_err () in
    Bar.f_ok x in

  (* let toto =
      Display.convert ~display_format:(Display.Human_readable) (Display.Displayable {
       value = computations ;
       format = (bind_format error_format Bar.Types.foo_expression_format);
      }) in *)

  let toto =
      Yojson.Basic.to_string @@ Display.convert ~display_format:(Display.Json) (Display.Displayable {
       value = computations ;
       format = (bind_format error_format Bar.Types.foo_expression_format);
      }) in



  print_string(
    toto
  )