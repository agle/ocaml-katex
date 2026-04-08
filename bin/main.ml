let () = Printexc.record_backtrace true

let l =
  {|
\f\relax{x} = \int_{-\infty}^\infty \f\hat\xi\,e^{2 \pi i \xi x}     \,d\xi
  |}

let () =
  let ctx = Katex.create () in
  let r = Katex.eval_katex ctx l ~display:false in
  match r with
  | Ok e -> print_endline e
  | Error e -> print_endline ("error: " ^ e)
