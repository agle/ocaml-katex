let () = Printexc.record_backtrace true

let () =
  let ctx = Katex.create () in
  let r = Katex.eval_katex ctx "\\frac 1 2" ~display:true in
  match r with
  | Ok e -> print_endline e
  | Error e -> print_endline ("error: " ^ e)
