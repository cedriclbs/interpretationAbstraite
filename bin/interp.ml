open Pf5;;
open Graphics;;

exception Quit;;

let listr = ref None;;
let listp = ref None;;

let point (x: float) (y: float) : Geo.point = {x = x; y = y};;

let length () =
  match !Config.mode with
  | "abs" -> List.length (Option.get !listr)
  | _ -> List.length (Option.get !listp)
;;

let maj (n: int) = 
  let maj_coord (n: int) =
    match !Config.mode with
    | "abs" -> Config.rectangle_coord := List.nth (Option.get !listr) n
    | _ -> Config.point_coord := List.nth (Option.get  !listp) n
  in maj_coord n;
  Display.draw n (length ());
;;

let initialize () = 
  match !Config.mode with
  | "abs" -> listr := Some (Argument.execution ());
  | _ -> listp := Some (List.map (fun (r: Geo.rectangle) -> (point r.x_min r.y_min)) (Argument.execution ()));
  ;
;;

let rec loop (n: int) =
  Display.force_size () ;
  synchronize ();
  maj n;
  let eve = wait_next_event [Mouse_motion;Key_pressed]
  in
  if eve.keypressed
  then 
    match eve.key with
      | 'n'  -> if n <= 0 then loop 0 else loop (n-1)
      | 'b'  -> if n >= (length () - 1) then loop n else loop (n+1)
      | '-'  -> if !Display.scale <= 10. then Display.scale := 1. else Display.scale := (!Display.scale -. 10.); loop n
      | '+'  -> if !Display.scale < 10. then Display.scale := 10. else Display.scale := (!Display.scale +. 10.); loop n
      | 'q'  -> raise Quit
      | _    -> loop n
  else begin
    loop n
  end
;;

let () =
  let message = "Argument manquant !" in
  Arg.parse Options.options Argument.parsing_argument message ;

  Display.window ();
  try
    initialize (); 
    loop 0
  with | Quit -> close_graph () | Graphics.Graphic_failure _ -> close_graph ()
;;