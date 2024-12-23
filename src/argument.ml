open Config
open Programs

let parsing_argument arg =
    match arg with
    | "1" | "2" | "3" -> Config.aux := Some arg
    | _ -> Config.aux := None ; raise ( Invalid_argument "Il faut choisir entre 1 et 3." )

let execution () =
    match !Config.aux with
    | Some s ->
         (match s with
               | "1" ->  (Programs.execute Programs.program_1)
               | "2" ->  (Programs.execute Programs.program_2)
               | "3" ->  (Programs.execute Programs.program_3)
               | _ -> raise (Invalid_argument "Impossible."))
    | None -> raise ( Invalid_argument "Aucun argument fourni." )