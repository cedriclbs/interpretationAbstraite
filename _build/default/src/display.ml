open Graphics;;

(* Initialise la fenêtre graphique et ses paramètres *)
let initialize_graphics () =
  open_graph "1000x800";
  set_window_title "Interpréteur Géométrique OCaml"

(* Dessine les axes avec des marques *)
let draw_axes () =
  let mid_x = size_x () / 2 in
  let mid_y = size_y () / 2 in
  set_color black;
  moveto mid_x 0;
  lineto mid_x (size_y ());
  moveto 0 mid_y;
  lineto (size_x ()) mid_y

(* Dessine un rectangle avec une couleur spécifique *)
let draw_rectangle () =
  set_color (rgb 255 200 200);  (* Couleur rose *)
  fill_rect (size_x () / 2 + 50) (size_y () / 2 + 50) 100 100

(* Boucle principale pour la gestion des événements *)
let rec event_loop () =
  try
    while true do
      let s = wait_next_event [Key_pressed] in
      if s.keypressed then
        match s.key with
        | 'q' -> raise Exit  (* Quitter le programme *)
        | _ -> ()
    done
  with Exit -> close_graph ()

(* Fonction principale *)
let main () =
  initialize_graphics ();
  draw_axes ();
  draw_rectangle ();
  event_loop ()

let () = main ()
