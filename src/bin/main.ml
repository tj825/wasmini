(* this doesn't really do anything useful yet *)

let () =
  if Array.length Sys.argv < 2
  then failwith "usage: wasmini <file.wasm>"
  else
    let ast = Wasm.Decode.decode "hmm" (Core.In_channel.read_all Sys.argv.(1)) in
    (* let ast = Wasmini.Translate.translate_module ast in *)
    Wasm.Print.module_
      Core.Out_channel.stdout
      80
      ast;
    ()
