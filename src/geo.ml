
(* Code de la Section 3 du projet. *)

type coord2D = {
    x : float;
    y : float
  }
type point = coord2D
type vector = coord2D
type angle = float

let translate (v : vector) (p : point) : point =
    {x = p.x +. v.x ; y = p.y +. v.y}

let rad_of_deg (a : angle) : angle =
  a *. (Float.pi/.180.0)

let deg_of_rad (a : angle) : angle =
  a *. (180.0/.Float.pi)

let rotate (c : point) (alpha : angle) (p : point) : point =
  let alpha_rad = rad_of_deg alpha in
  let fact1 = p.x -. c.x in
  let fact2 = p.y -. c.y in
  let resCos = cos alpha_rad in 
  let resSin = sin alpha_rad in
  {x = c.x +. fact1 *. resCos -. fact2 *. resSin; 
  y = c.y +. fact1 *. resSin +. fact2 *. resCos }
  



type transformation =
  Translate of vector
| Rotate of point * angle

let transform (t : transformation) (p : point) : point =
  match t with
  | Translate v -> translate v p
  | Rotate (pointAct, alpha) -> rotate pointAct alpha p 
  


type rectangle = {
    x_min : float;
    x_max : float;
    y_min : float;
    y_max : float
  }

let in_rectangle (r : rectangle) (p : point) : bool =
  p.x >= r.x_min && p.x <= r.x_max && p.y >= r.y_min && p.y <= r.y_max



let corners (r :rectangle) : point list =
   [
    {x = r.x_min ; y = r.y_min};
    {x = r.x_min ; y = r.y_max};
    {x = r.x_max; y = r.y_min};
    {x = r.x_max; y = r.y_max}
   ]
  


  
let rectangle_of_list (pl : point list) : rectangle = 
  failwith "À compléter"
