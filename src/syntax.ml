(*
wasmini AST

TL;DR it's WASM but only i64 and no SIMD/vector instructions

Also at least these things are still TODO for now:
- tables/elems
- memories/datas
- start
- some export/import types

Finally, for now, there will also be some interpreter implementation
details recorded in the AST, see Jump_var etc
*)

type ref_type = Funcref | Externref
type value_type = I64 | Ref of ref_type

type func_type = FuncType of value_type list * value_type list (* TODO why list? *)

type testop = Eqz
type relop
  = Eq | Ne
  | Lt_s | Lt_u | Gt_s | Gt_u
  | Le_s | Le_u | Ge_s | Ge_u
type unop
  = Clz | Ctz | Popcnt
type binop
  = Add | Sub | Mul
  | Div_s | Div_u | Rem_s | Rem_u
  | And | Or | Xor
  | Shl | Shr_s | Shr_u
  | Rotl | Rotr

(* some trivial Var modules, to help keep track of the different kinds
   of variables *)
module type VAR = sig
  type t
  val to_int : t -> int
  val of_int : int -> t
  val compare : t -> t -> int
end

module Var : VAR = struct
  type t = int
  let to_int x = x
  let of_int x = x
  let compare = compare
end

module Type_var : VAR = Var
module Label_var : VAR = Var
module Func_var : VAR = Var
module Local_var : VAR = Var
module Global_var : VAR = Var

(* some WIP interpreter implementation details: *)

module Jump_var : VAR = Var

type break_info =
  {label : Label_var.t;
   jump : Jump_var.t}

type local_info =
  {local : Local_var.t;
   offset : int}

type global_info =
  {global : Global_var.t;
   address : int}



type instr =
  | Unreachable
  | Nop
  | Drop
  (* | Select of *) (* TODO why `value_type list option`? *)
  | Block of Type_var.t * instr list * Jump_var.t
  | Loop of Type_var.t * Jump_var.t * instr list
  | If of Type_var.t * instr list * instr list
  | Br of break_info
  | Br_if of break_info
  | Br_table of break_info list * break_info
  | Return
  | Call of Func_var.t * Jump_var.t
  (* | Call_indirect of *)
  | Local_get of local_info
  | Local_set of local_info
  | Local_tee of local_info
  | Global_get of global_info
  | Global_set of global_info
  (* TODO refs and tables *)
  (* | Table_get *)
  (* | Table_set *)
  (* | Table_size *)
  (* | Table_grow *)
  (* | Table_fill *)
  (* | Table_copy *)
  (* | Table_init *)
  (* | Elem_drop *)
  | Load
  | Store
  | Memory_size
  | Memory_grow
  | Memory_fill
  | Memory_copy
  (* TODO? *)
  (* | Memory_init *)
  (* | Data_drop of *)
  (* TODO refs and tables *)
  (* | Ref_null of ref_type *)
  (* | Ref_func of Func_var.t *)
  (* | Ref_is_null *)
  | Const of int64
  | Test of testop
  | Compare of relop
  | Unary of unop
  | Binary of binop
  (* no Convert :D *)
  (* no SIMD :D *)

type func =
  {type_ : Type_var.t;
   (* TODO is this the params concatenated with the other locals? *)
   locals : value_type list;
   body : instr list}

(* WASM note:
   "const" instrs are: t.const, ref.null, ref.func, global.get (immut) 
   and const in globals/elems/datas can only refer to imported globals.
   ...probably should make a separate ADT?  *)
type const = instr list

type global =
  {mut : bool;
   type_ : value_type;
   init : const}

type export_desc =
  | Func of Type_var.t
  (* | Table of *)
  (* | Memory of *)
  (* | Global of *)

type export =
  {name : string;
   desc : export_desc}

type import_desc =
  | Func of Type_var.t
  (* | Table of *)
  | Memory (* of memory_limits *)
  (* | Global of *)

type import_name =
  {module_name : string;
   item_name : string}

module Import_name = struct
  type t = import_name

  let compare x y : int =
    let c = compare x.module_name y.module_name in
    if c <> 0
    then c
    else
      compare x.item_name y.item_name

  let pp ppf {module_name; item_name} =
    Format.fprintf ppf "%s.%s" module_name item_name
end

type import =
  {name : import_name;
   desc : import_desc}

type module_ =
  {types : func_type list;
   funcs : func list;
   globals : global list;
   exports : export list;
   imports : import list}
