open Config

let options = [
  ("-abs", Arg.String (fun s -> rectangle_coord := config_abs s ; mode := "abs" ), "Pour visualiser un rectangle approximative");
  ("-cr", Arg.Unit (fun () -> mode := "cr"), "Pour visualiser un point");
  ("-bc", Arg.String (fun s -> bcolor :=  config_rgb s), "La couleur de l'arrière-plan");
  ("-fc", Arg.String (fun s -> fcolor :=  config_rgb s), "La couleur de l'avant-plan");
  ("-rc", Arg.String (fun s -> rcolor :=  config_rgb s), "La couleur du rectangle");
  ("-pc", Arg.String (fun s -> pcolor :=  config_rgb s), "La couleur du point");
  ("-size", Arg.String (fun s -> wsize := size s ;  update_offset () ), "Taille de la fenêtre")
]