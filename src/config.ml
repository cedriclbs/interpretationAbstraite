open Geo

let aux = ref None
let rcolor = ref (250, 150, 150)
let pcolor = ref (255, 0, 0)
let bcolor = ref (200, 180, 150)
let fcolor = ref (0, 0, 0)
let wsize = ref (800, 600)
let woffset = ref (let (w, h) = !wsize in (w / 2, h / 2))
let mode = ref "default"

let rectangle_coord = ref {x_min = -1.; y_min = -1.; x_max = 1.; y_max = 1.}
let point_coord = ref {x = 0. ; y = 0.}

let prog_det = ref None

let limit_color r g b =
  let limit x =
    if x < 0 then raise (Invalid_argument "La valeur de la couleur doit être entre 0 et 255")
    else if x > 255 then raise (Invalid_argument "La valeur de la couleur doit être entre 0 et 255")
    else x
  in (limit r, limit g, limit b)
;;

let update_offset () =
  woffset := let (w, h) = !wsize in (w / 2, h / 2)
;;

let config_rgb s =
  match List.map ( fun i -> int_of_string i ) ( String.split_on_char ',' s ) with
  | [r; g; b] -> limit_color r g b
  | _ -> raise ( Invalid_argument "Format RGB invalide (doit être r,v,b)" )
;;

let config_abs s =
  match List.map ( fun i -> int_of_string i ) (  String.split_on_char ',' s ) with
  | [x_min ; y_min ; x_max ; y_max ] ->
    if  x_min <= 0 && x_max >= 0 && y_min <= 0 && y_max >= 0
    then 
      {x_min = float_of_int x_min; y_min = float_of_int y_min; 
       x_max = float_of_int x_max; y_max = float_of_int y_max}
    else raise  ( Invalid_argument " La valeur est incorrect ! " )
  | _ -> raise ( Invalid_argument "L'abs est incorrect !" )
;;

let size s =
  try
     match List.map ( fun i -> int_of_string i ) ( String.split_on_char 'x' s )  with
     | [h; w] -> (h, w)
     | _ -> raise ( Invalid_argument "Taille invalide !" )
  with
  | Failure _ -> raise (Invalid_argument "Taille invalide !" )
;;
