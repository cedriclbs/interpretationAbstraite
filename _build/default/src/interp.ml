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
  | Either (p1, p2) :: rest -> 
    (unfold_repeat p1) @ (unfold_repeat p2) @ (unfold_repeat rest)






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
  





(* Fonction simulant une exécution possible d’un programme quelconque à partir d’un point de départ donné. *)
let rec run (prog : program) (p : point) : point list =
  let rec aux current_pos acc prog =
    match prog with
    | [] -> List.rev acc
    | Move t :: rest ->
        let new_pos = transform t current_pos in
        aux new_pos (new_pos :: acc) rest
    | Repeat (n, p) :: rest ->
      let rec repeat_aux n current_pos acc =
        if n <= 0 then acc
        else
          let new_pos = run p current_pos in
          match new_pos with
          | [] -> acc
          | new_pos :: _ ->
              let next_acc = new_pos :: acc in
              repeat_aux (n - 1) new_pos next_acc
      in
      let pos_repetes = repeat_aux n current_pos [] in
      aux current_pos (List.rev_append pos_repetes acc) rest
      
    | Either (p1, p2) :: rest ->
        let choice = Random.bool () in
        if choice then
          aux current_pos acc (unfold_repeat p1 @ rest)
        else
          aux current_pos acc (unfold_repeat p2 @ rest)
  in
  aux p [p] (unfold_repeat prog)
  

    

(* Fonction calculant, à partir d’un programme quelconque, la liste de programmes déterministes comme expliqué ci-dessus.*)
let rec all_choices (prog : program) : program list =
  match prog with 
  | [] -> [[]]

  | Move t :: rest -> 
      let rest_choices = all_choices rest in
      List.map (fun r ->  Move t :: r) rest_choices

  | Repeat (n, p) :: rest -> 
      let rest_ch = all_choices rest in
      let current_ch = all_choices (List.init n (fun _ -> p) |> List.concat) in 
      List.concat_map (fun choice ->
        List.map (fun cat -> cat @ choice) current_ch
      ) rest_ch
      
  | Either (p1, p2) :: rest -> 
    let p1_choices = all_choices p1 in
    let p2_choices = all_choices p2 in 
    let rest_choices = all_choices rest in 
    List.concat_map (fun choice -> 
        List.map (fun cat -> cat @ choice) (p1_choices @ p2_choices)
      ) rest_choices




(* Fonction vérifiant si toute exécution possibles du programme se terminent dans la zone cible*)
let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  let choices = all_choices prog in
  List.for_all (fun aux -> 
    let point_pos = run aux p in 
    let final_pos = List.hd (List.rev point_pos) in 
    in_rectangle r final_pos
  ) choices
  
