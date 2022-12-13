module Memory : sig
  type t
  val create : int -> t
  val load : t -> int -> int64
  val store : t -> int -> int64 -> unit
  val clear : t -> unit
end = struct
  open Bigarray
  type t = (int64, int64_elt, c_layout) Array1.t
  let create size =
    Array1.create Int64 C_layout size
  let load mem idx =
    Array1.get mem idx
  let store mem idx value =
    Array1.set mem idx value
  let clear mem =
    Array1.fill mem 0L
end

module Host_interface : sig
  type t
  val get_arg : t -> int -> int64
  val put_result : t -> int64 -> unit
end = struct
  type t = {todo : unit}
  let get_arg _ _ = failwith "TODO"
  let put_result _ _ = failwith "TODO"
end

type host_func = Host_interface.t -> unit

type module_inst_wip =
  {jumps : Syntax.instr list list;
   host_funcs : host_func list}

open Syntax

module Import_map = Map.Make(Syntax.Import_name)

let init_wip (module_ : module_) (host_funcs : host_func Import_map.t) : module_inst_wip =
  let wip =
    {jumps = [];
     host_funcs = []} in
  let wip =
    List.fold_left
      (fun inst import ->
        match import.desc with
        | Memory ->
           inst
        | Func _type ->
           match Import_map.find_opt import.name host_funcs with
           | None ->
              failwith (Format.asprintf "missing import %a" Import_name.pp import.name)
           | Some host_func ->
              {inst with host_funcs = inst.host_funcs @ [host_func]})
      wip
      module_.imports in
  let wip =
    List.fold_left
      (fun _inst _func ->
        failwith "TODO jumps")
      wip
      module_.funcs in
  wip

type module_inst =
  {jumps : Syntax.instr list Array.t;
   host_funcs : host_func Array.t}

let finalize_module_inst (m : module_inst_wip) : module_inst =
  {jumps = Array.of_list m.jumps;
   host_funcs = Array.of_list m.host_funcs}

let init module_ host_funcs =
  finalize_module_inst (init_wip module_ host_funcs)
