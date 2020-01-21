open Ppxlib

module E = struct
  open Ast_pattern

  let tags =
    Attribute.declare
      "tags"
      Attribute.Context.pattern
      (single_expr_payload (
         pexp_tuple (many (estring __))
         |||  map (estring __) ~f:(fun f x -> f [x])))
      (fun x -> x)

  let list_of_option = function
    | None -> []
    | Some x -> x

  let opt_name () =
         map (pstring __) ~f:(fun f x -> f (Some x))
     ||| map ppat_any     ~f:(fun f   -> f None)

  let opt_name_and_expr expr =
    pstr ((
      pstr_value nonrecursive (
        (value_binding
           ~pat:(
             map
               (Attribute.pattern tags (opt_name ()))
               ~f:(fun f attributes name_opt ->
                 f ~name:name_opt ~tags:(list_of_option attributes)))
           ~expr)
        ^:: nil)
      ) ^:: nil)

  let expand_osh_prog ~loc ~path program args =
    ignore (path);
    let pname = Ast_builder.Default.estring ~loc program in
    let pargs = Ast_builder.Default.(elist ~loc (List.map (estring ~loc) args)) in
    [%expr Osh.Proc.create_sys_process ~program:[%e pname] ~args:[%e pargs]]

  let osh =
    Context_free.Rule.extension @@
      Extension.declare "osh"
        Extension.Context.expression
        Ast_pattern.(single_expr_payload
                       (pexp_tuple ((estring __)^::(elist (estring __))^::nil)))
        expand_osh_prog

  let all = [ osh ]
end

let () = Driver.register_transformation "osh_transformation" ~rules:E.all
