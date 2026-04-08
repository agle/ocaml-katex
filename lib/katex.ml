open Quickjs_eval.Quickjs

open struct
  let blob = [%blob "vendor/katex.js"]
end

let create () =
  let rt = JS.js_NewRuntime () in
  let ctx = JS.js_NewContext rt in
  ignore @@ eval_str ctx blob "katex.min.js";
  ctx

let eval_katex ctx ?(display = false) tex =
  let b = if display then "true" else "false" in
  let e = new_string ctx tex in
  let f =
    JS.js_json_stringify ctx e (JS.js_value_null ()) (JS.js_value_null ())
  in
  let s = JS.js_to_cstring ctx f in
  let tex_escaped = Option.get s in
  let s =
    Printf.sprintf
      "katex.renderToString(%s, {displayMode: %s, throwOnError: false})"
      tex_escaped b
  in
  print_endline s;
  try eval_str_ret_str ctx s |> Option.to_result ~none:"Nothing returned"
  with QuickJSEerror m -> Error m

let cmarkit_map_katex ?(ignore_errors = true) (doc : Cmarkit.Doc.t) =
  let ctx = create () in
  let inline _mapper (b : Cmarkit.Inline.t) =
    match b with
    | Cmarkit.Inline.Ext_math_span (m, meta) -> (
        let display = Cmarkit.Inline.Math_span.display m in
        let tex = Cmarkit.Inline.Math_span.tex m in
        let html = eval_katex ctx tex ~display in
        match html with
        | Ok html ->
            let h = Cmarkit.Block_line.tight_list_of_string html in
            `Map (Some (Cmarkit.Inline.Raw_html (h, meta)))
        | Error e ->
            if ignore_errors then `Map (Some b)
            else failwith ("Katex render error:" ^ e))
    | _ -> `Default
  in
  let block _mapper (b : Cmarkit.Block.t) =
    match b with
    | Cmarkit.Block.Ext_math_block (node, meta) -> (
        let code =
          Cmarkit.Block.Code_block.code node
          |> List.map Cmarkit.Block_line.to_string
          |> String.concat "\n"
        in
        let tex = eval_katex ctx code ~display:true in
        match tex with
        | Ok html ->
            let h = Cmarkit.Block_line.list_of_string html in
            `Map (Some (Cmarkit.Block.Html_block (h, meta)))
        | Error e ->
            if ignore_errors then `Map (Some b)
            else failwith ("Katex render error:" ^ e))
    | _ -> `Default
  in
  let mapper = Cmarkit.Mapper.make ~block ~inline () in
  Cmarkit.Mapper.map_doc mapper doc
