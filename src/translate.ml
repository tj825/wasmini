(* TODO *)
let translate_module (ast : Wasm.Ast.module_) : Syntax.module_ =
  match ast.it with
  | { types;
      globals;
      tables;
      memories;
      funcs;
      start;
      elems;
      datas;
      imports;
      exports } ->

     (* TODO now *)
     ignore types;
     ignore globals;
     ignore memories;
     ignore funcs;
     ignore imports;
     List.fold_left
       (fun _x (y : Wasm.Ast.export) ->
         (match ((y.it).edesc).it with
          | FuncExport _ -> failwith "TODO func export"
          | TableExport _ -> failwith "TODO table export"
          | MemoryExport _ -> failwith "TODO memory export"
          | GlobalExport _ -> failwith "TODO global export"))
       () exports;

     (* TODO later *)
     if List.length tables > 0 then failwith "TODO tables";
     if Option.is_some start then failwith "TODO start";
     if List.length elems > 0 then failwith "TODO elems";
     if List.length datas > 0 then failwith "TODO datas";

     failwith "TODO actually do the translation"
