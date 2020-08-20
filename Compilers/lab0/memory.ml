(* lab1/memory.ml *)
(* Copyright (c) 2017 J. M. Spivey *)

let vars = ref [];;

let store vr vl =
    vars := (vr, vl)::!vars;;

let rec find vr = function
    [] -> raise Not_found
  | (n, v)::ts -> if n=vr then v else find vr ts;;

let recall vr = find vr !vars;;
