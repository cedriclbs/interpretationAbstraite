open Geo
open Interp

(* Code de la Section 5 du projet. *)


(* Fonction renvoyant un point choisi aléatoirement à l’intérieur de ce rectangle*)
let sample (rect : rectangle) : point =
  { x = Random.float (rect.x_max -. rect.x_min) +. rect.x_min;
    y = Random.float (rect.y_max -. rect.y_min) +. rect.y_min
  }
  


(* Fonction renvoyant l’image d’un rectangle par la transformation donnée en argument*)
let transform_rect (t : transformation) (r : rectangle) : rectangle =
  match t with 
  | Translate v ->
    { x_min = r.x_min +. v.x;
      x_max = r.x_max +. v.x;
      y_min = r.y_min +. v.y;
      y_max = r.y_max +. v.y;
    }
  | Rotate (c, alpha) -> 
    let corners = List.map (rotate c alpha) (corners r) in 
    rectangle_of_list corners




(* Fonction qui retourne la liste des rectangles correspondant aux positions successives visitées suite au programme*)
let run_rect (prog : program) (r : rectangle) : rectangle list =
  let rec rrAux (progAux : program) (rAux : rectangle) (rListAux : rectangle list) : rectangle list =
    
    match progAux with
    | [] -> rListAux

    | Move x::rest ->
        let new_rect = transform_rect x rAux in
        rrAux rest new_rect (new_rect :: rListAux)
    
    | Repeat (n, progRep)::rest ->
        let temp = (rrAux progRep rAux rListAux) in 
        if n < 2 then
          rrAux rest (List.hd temp) temp
        else 
          rrAux (Repeat (n - 1, progRep) :: rest) (List.hd temp) temp
    
    | Either (sp1, sp2)::rest ->
    let temp =
      [rectangle_of_list
        (List.flatten
            [corners (List.hd (rrAux sp1 rAux rListAux));
            corners (List.hd (rrAux sp2 rAux rListAux))])]
    in
    rrAux rest (List.hd temp) temp
  in
  List.rev (rrAux prog r [r])
    



(* Fonction booléene qui renvoie true si r est contenu dans t, false sinon. *)
let inclusion (r : rectangle) (t : rectangle) : bool =
  r.x_min >= t.x_min && r.x_max <= t.x_max && r.y_min >= t.y_min && r.y_max <= t.y_max
  



(* Fonction booléene qui renvoie true si le robot termine son trajet dans la zone cible dans toutes les exécutions du programme*)
let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
  let adap = run_rect prog r in
  List.for_all (fun aux -> inclusion aux target) adap




(* Fonction qui renvoie la liste des états successifs du robot pour une des exécutions possibles du programme donné en partant de l'état initial. *)
let run_polymorphe (transformer : transformation -> 'a -> 'a) (prog : program) (init : 'a) : 'a list =
  let rec aux (current_state : 'a) (acc : 'a list) (prog : program) : 'a list =
    match prog with
    | [] -> List.rev acc
    | Move t :: rest ->
        let new_state = transformer t current_state in
        aux new_state (new_state :: acc) rest
    | Repeat (n, p) :: rest ->
        let rec repeat_aux n current_state acc =
          if n <= 0 then acc
          else
            let new_states = aux current_state [] p in
            match new_states with
            | [] -> acc  
            | _ -> repeat_aux (n - 1) (List.hd (List.rev new_states)) (List.rev_append new_states acc)
        in
        repeat_aux n current_state acc |> fun acc_new -> aux current_state acc_new rest
    | Either (p1, p2) :: rest ->
        let choice = Random.bool () in
        let new_prog = if choice then unfold_repeat p1 @ rest else unfold_repeat p2 @ rest in
        aux current_state acc new_prog
  in
  aux init [init] (unfold_repeat prog)




(* Fonction qui renvoye la sur-approximation des états atteignables par un robot *)
let rec over_approximate (prog : program) (init_rect : rectangle) : rectangle =
  let rec aux prog current_rect =
    match prog with
    | [] -> current_rect
    | Move t :: rest ->
        let new_rect = transform_rect t current_rect in
        aux rest new_rect
    | Repeat (n, sub_prog) :: rest ->
        let rec repeat_rect n rect =
          if n <= 0 then rect
          else
            let repeated_rect = aux sub_prog rect in
            repeat_rect (n - 1) repeated_rect
        in
        let repeated_final_rect = repeat_rect n current_rect in
        aux rest repeated_final_rect
    | Either (p1, p2) :: rest ->
        let rect1 = aux p1 current_rect in
        let rect2 = aux p2 current_rect in
        let combined_rect = rectangle_of_list (corners rect1 @ corners rect2) in
        aux rest combined_rect
  in
  aux prog init_rect

let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"
