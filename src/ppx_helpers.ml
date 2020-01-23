open Ppxlib

module E = struct
  open Ast_pattern

  let tags =
    Attribute.declare
      "osh_tags"
      Attribute.Context.pattern
      (single_expr_payload (
         pexp_tuple (many (estring __))
         |||  map (estring __) ~f:(fun f x -> f [x])))
      (fun x -> x)


  let payload_pattern () =
    Ast_pattern.(
      map
        (single_expr_payload (pexp_loc __ (pexp_constant (pconst_string __ __))))
        ~f:(fun f loc s tag -> f (Some (loc, s, tag)))
      ||| map (pstr nil) ~f:(fun f -> f None))


  let uncaught_exn =
    Attribute.declare_with_name_loc
      "@expect.uncaught_exn"
      Attribute.Context.value_binding
      (map1' (payload_pattern ()) ~f:(fun loc x -> loc, x))
      (fun ~name_loc:_ (loc, _) ->
        loc, ())


  (* This patterns matches on the script name, which is optional *)
  let script_name () =
    map (ppat_var __) ~f:(fun f x -> f (Some x))
    ||| map ppat_any ~f:(fun f -> f None)


  let script_pattern () =
    pstr
      (pstr_value
         nonrecursive
         (Attribute.pattern
            uncaught_exn
            (value_binding
               ~pat:
               (map
                  (Attribute.pattern tags (script_name ()))
                  ~f:(fun f attributes name_opt ->
                    f
                      ~name:name_opt
                      ~tags:
                      (match attributes with
                       | None -> []
                       | Some x -> x)))
               ~expr:__)
          ^:: nil)
       ^:: nil)


  let rec code_transform ~loc code =
    let mk_prog_name p ~loc =
      match p.pexp_desc with
      | Pexp_ident { txt = Lident t; _ } ->
         Ast_builder.Default.estring ~loc t
      | _ -> Location.raise_errorf ~loc "Need a label here"
    in
    match code with
    (* If it's a `let` expression *)
    | [%expr let [%p? p] = [%e? exp1] in [%e? exp2]] ->
       [%expr
        let [%p p] = [%e code_transform ~loc exp1] in
        [%e code_transform ~loc exp2]
       ]
    (* If it's a daisy chain *)
    | [%expr (    ([%e? pn1] [%e? pargs1])
              ||> ([%e? pn2] [%e? pargs2])
              ||> [%e? cont])] ->
       let prog_name1 = mk_prog_name pn1 ~loc in
       let prog_name2 = mk_prog_name pn2 ~loc in
       [%expr
           create_sys_process ~program:[%e prog_name1] ~args:[%e pargs1]
       ||> create_sys_process ~program:[%e prog_name2] ~args:[%e pargs2]
       ||> [%e code_transform ~loc cont]
       ]
    (* If we're at the end of a chain *)
    | [%expr (    ([%e? pn1] [%e? pargs1])
              ||> ([%e? pn2] [%e? pargs2]))] ->
       let prog_name1 = mk_prog_name pn1 ~loc in
       let prog_name2 = mk_prog_name pn2 ~loc in
       [%expr
           create_sys_process ~program:[%e prog_name1] ~args:[%e pargs1]
       ||> create_sys_process ~program:[%e prog_name2] ~args:[%e pargs2]]
    (* If we just have a single program *)
    | [%expr ([%e? pn] [%e? prog_args])] ->
       let prog_name = mk_prog_name pn ~loc
       in
       [%expr
           create_sys_process ~program:[%e prog_name] ~args:[%e prog_args]
       ]
    | e -> e


  let expand_osh_script ~loc ~path:_ uncaught_exn ~name ~tags code =
    ignore (uncaught_exn, tags);
    match name with
    | Some n ->
       let nvar  = Ast_builder.Default.pvar ~loc n in
       [%str
        let [%p nvar] =
          let open Osh.Proc in
          [%e code_transform ~loc code]
       ]
    | None ->
       [%str let _ =
          let open Osh.Proc in
          [%e code_transform ~loc code]
       ]


  let expand_osh_prog ~loc ~path:_ program =
    let pname = Ast_builder.Default.estring ~loc program in
    [%expr
        fun args -> Osh.Proc.create_sys_process
                      ~program:[%e pname]
                      ~args:args
    ]


  let osh_script =
    Extension.declare_inline "osh_script"
      Extension.Context.structure_item
      (script_pattern ())
      expand_osh_script


  let osh =
    Extension.declare "osh"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (estring __))
      expand_osh_prog

  let all_rules = List.map Context_free.Rule.extension [ osh; osh_script ]
end

let () =
  Driver.register_transformation "osh_transformation"
    ~rules:E.all_rules
    ~enclose_impl:(fun loc ->
      match loc with
      | None -> [], []
      | Some _ -> [], []
    )
