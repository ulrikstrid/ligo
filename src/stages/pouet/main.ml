open Myerror

let _ =
  let computations : (Bar.Types.expression, _) result =
    let%bind x = Foo.f_ok () in
    Bar.f_serr x in

  let toto =
      Yojson.Basic.to_string @@ Display.convert ~display_format:(Display.Json) (Display.Displayable {
       value = computations ;
       format = (bind_format error_format Bar.Types.foo_expression_format);
      }) in

  print_string(
    toto
  )