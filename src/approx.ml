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

  



  
let run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
  failwith "À compléter"

let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  failwith "À compléter"

let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"
