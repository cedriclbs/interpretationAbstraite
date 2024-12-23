open Geo
open Interp
open Approx
open Config

let vector x y = { x ; y }

let program_1 = [Repeat (5, [Either ([Move (Rotate (vector 0. 0., 90.))], [Move (Translate (vector 0. 1.))])])]

let program_2  = [Either ([Move (Translate (vector 1. 2.))], [Move (Rotate (vector 0. 0., 90.))])]
 
let program_3 = 
  let t1 = Move (Translate (vector 0. 1.)) in
  let t2 = Move (Translate (vector 1. 0.)) in
  [Repeat (10, [Either ([t1; t2], [t2; t1])])]

let execute program =
  prog_det := Some (program);
  let rectangle =
    match !mode with
    | "abs" -> !rectangle_coord
    | _ -> let new_rectangle = [!point_coord; !point_coord; !point_coord; !point_coord] in rectangle_of_list new_rectangle
  in run_rect (Option.get !prog_det) rectangle
;;