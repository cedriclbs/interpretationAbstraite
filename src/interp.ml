open Geo

(* Code de la Section 4 du projet. *)

type instruction =
  Move of transformation
| Repeat of int * program
| Either of program * program 
and program = instruction list

(* Fonction qui vérifie si un programme est déterministe *)
let rec is_deterministic (prog : program) : bool =
  match prog with
  | [] -> true
  | Move _ :: rest -> is_deterministic rest
  | Repeat (_, p) :: rest -> is_deterministic p && is_deterministic rest
  | Either (_, _) :: _ -> false

(* Fonction qui déplie les instructions Repeat d'un programme *)  
let rec unfold_repeat (prog : program) : program =
  match prog with
  | [] -> []
  | Move p :: rest -> Move p :: unfold_repeat rest
  | Repeat (n, p) :: rest ->
    let rec repeat_aux n p =
      if n <= 0 then []
      else p @ repeat_aux (n - 1) p
    in
    repeat_aux n (unfold_repeat p) @ unfold_repeat rest
  | Either (_, _) :: _ -> failwith "Le programme n'est pas déterministe"

(* Fonction qui génère la liste des positions visitées par un robot *)
let run_det (prog : program) (p : point) : point list =
  let rec aux current_pos acc prog =
    match prog with
    | [] -> acc
    | Move t :: rest ->
        let new_pos = transform t current_pos in
        aux new_pos (new_pos :: acc) rest
    | Repeat (_,_) :: _ | Either (_ ,_) :: _ -> failwith "Le programme n'est pas déterministe"
  in
 List.rev (aux p [p] (unfold_repeat prog))

(* Fonction qui vérifie si le robot termine son trajet dans la cible à la fin de l'exécution du programme *)
let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  match List.rev (run_det prog p) with
  | [] -> false 
  | final_point :: _ -> in_rectangle target final_point
  
let run (prog : program) (p : point) : point list =
  failwith "À compléter"

let all_choices (prog : program) : program list =
  failwith "À compléter"

let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  failwith "À compléter"
