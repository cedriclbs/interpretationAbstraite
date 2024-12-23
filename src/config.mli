val aux : string option ref
val rcolor : ( int * int * int )  ref
val pcolor : ( int * int * int ) ref
val bcolor : ( int * int * int ) ref
val fcolor : ( int * int * int ) ref
val wsize : ( int * int ) ref
val woffset : ( int * int ) ref
val mode : string ref

val rectangle_coord : Geo.rectangle ref
val point_coord : Geo.point ref

val prog_det : Interp.program option ref

val config_rgb : string -> ( int * int * int )
val config_abs : string -> Geo.rectangle
val size : string -> ( int * int )
val update_offset : unit -> unit