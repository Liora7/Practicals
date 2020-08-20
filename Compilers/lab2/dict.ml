(* lab2/dict.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

(* Environments are implemented using a library module that
   represents mappings by balanced binary trees. *)

type ident = string

type ptype =
    Integer
  | Boolean
  | Array of int * ptype
  | Void

(* |def| -- definitions in environment *)
type def =
  { d_tag: ident;               (* Name *)
    d_type: ptype;              (* Type *)
    d_lab: string }             (* Global label *)

module IdMap = Map.Make(struct type t = ident  let compare = compare end)

type environment = Env of def IdMap.t

let can f x = try f x; true with Not_found -> false

(* |define| -- add a definition *)
let define d (Env e) =
  if can (IdMap.find d.d_tag) e then raise Exit;
  Env (IdMap.add d.d_tag d e)

(* |lookup| -- find definition of an identifier *)
let lookup x (Env e) = IdMap.find x e

(* |init_env| -- empty environment *)
let init_env = Env IdMap.empty

(* \type_size\ -- calculate size occupied by value of given type *)
let rec type_size t =
  match t with
    Integer -> 4
  | Boolean -> 1
  | Array (size, t2) -> size * type_size t2
  | _ -> 0

(* |is_array| -- return true for array types *)
let is_array t =
  match t with
     Array _ -> true
    | _ -> false

(* |base_type| -- return base type of array, or raise exception *)
let base_type t =
  match t with
      Array (_, t2) -> t2
    | _ -> failwith "Not array type"
