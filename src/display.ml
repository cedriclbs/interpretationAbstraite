open Graphics
open Config
open Geo

let scale = ref 50.
;;

let rgbcolor (r, g, b) = rgb r g b
;;

let window () =
    let (w, h) = !wsize in
    let win_spec = Printf.sprintf " %dx%d" w h in
    open_graph win_spec;
    set_window_title "Robot Interpreter"
;;

let draw_point () =
    let (r, v, b) = !Config.pcolor in
    set_color (rgbcolor (r, v, b));
    let p = !Config.point_coord in
    let (w, h) = !woffset in
    fill_circle (w + (int_of_float (p.x *. !scale))) (h + (int_of_float (p.y *. !scale))) 3;
;;

let draw_grid () =
    set_color black;
    
    let (w, h) = !wsize in

    moveto 0 (h / 2);
    lineto w (h / 2);
    for i = 1 to ((h / 2) / (int_of_float !scale)) do
        moveto (w / 2 + 5) (h / 2 + i * (int_of_float !scale));
        lineto (w / 2 - 5) (h / 2 + i * (int_of_float !scale));
        moveto (w / 2 + 10) (h / 2 - 7 + i * (int_of_float !scale));
        draw_string (string_of_int i);
        moveto (w / 2 + 5) (h / 2 - i * (int_of_float !scale));
        lineto (w / 2 - 5) (h / 2 - i * (int_of_float !scale));
        moveto (w / 2 + 10) (h / 2 - 7 - i * (int_of_float !scale));
        draw_string (string_of_int (-i));
    done;

    moveto (w / 2) 0;
    lineto (w / 2) h;
    for i = 1 to ((w / 2) / (int_of_float !scale)) do
        moveto (w / 2 + i * (int_of_float !scale)) (h / 2 + 5);
        lineto (w / 2 + i * (int_of_float !scale)) (h / 2 - 5);
        moveto (w / 2 - 3 + i * (int_of_float !scale)) (h / 2 + 10);
        draw_string (string_of_int i);
        moveto (w / 2 - i * (int_of_float !scale)) (h / 2 + 5);
        lineto (w / 2 - i * (int_of_float !scale)) (h / 2 - 5);
        moveto (w / 2 - 7 - i * (int_of_float !scale)) (h / 2 + 10);
        draw_string (string_of_int (-i));
    done;
;;  

let draw_rectangle () =
    let (r, v, b) = !rcolor in
    set_color (rgbcolor (r, v, b));
    let re = !Config.rectangle_coord in
    let (w, h) = !woffset in
    let (x, y, x', y') = 
    (w + (int_of_float (re.x_min *. !scale)), h + (int_of_float (re.y_min *. !scale)), 
     w + (int_of_float (re.x_max *. !scale)), h + (int_of_float (re.y_max *. !scale))) 
    in
    fill_rect x y (x' - x) (y' - y);
    let (r, v, b) = !Config.pcolor in
    set_color (rgbcolor (r, v, b));
    draw_rect x y (x' - x) (y' - y);
;;

let draw_background () =
    let (r, v, b) = !bcolor in
    set_color (rgbcolor (r, v, b));
    let (w, h) = !wsize in
    fill_rect 0 0 w h
;;

let draw_command () =
    let (r, v, b) = !fcolor in
    set_color (rgbcolor (r, v, b));
    let (w, h) = !wsize in
    moveto (w - 110) 100;
    draw_string " Commands : ";
    moveto (w - 110) 70;
    draw_string "N : Next step";
    moveto (w - 110) 55;
    draw_string "B : Previous step";
    moveto (w - 110) 40;
    draw_string "+ : Zoom in";
    moveto (w - 110) 25;
    draw_string "- : Zoom out";
    moveto (w - 110) 10;
    draw_string "Q : Quit";
;;

let draw n len =
    draw_background ();

    if !Config.mode = "abs" then
        draw_rectangle ()
    else
        draw_point ();

    draw_grid ();
    draw_command ();
;;

let force_size () =
  let current_size = ( size_x (), size_y () ) in
  let ( h , w ) = !wsize in
  if current_size <> ( h , w ) then resize_window h w
;;
