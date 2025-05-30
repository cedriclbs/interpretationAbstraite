(* Example from the documentation, this code is in public domain. *)

(* Implementation, we check the dest argument and print the args *)

let cp verbose recurse force srcs dest =
  let many = List.length srcs > 1 in
  if many && (not (Sys.file_exists dest) || not (Sys.is_directory dest))
  then `Error (false, dest ^ ": not a directory") else
  `Ok (Printf.printf
         "verbose = %B\nrecurse = %B\nforce = %B\nsrcs = %s\ndest = %s\n"
         verbose recurse force (String.concat ", " srcs) dest)

(* Command line interface *)

open Cmdliner

let verbose =
  let doc = "Print file names as they are copied." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let recurse =
  let doc = "Copy directories recursively." in
  Arg.(value & flag & info ["r"; "R"; "recursive"] ~doc)

let force =
  let doc = "If a destination file cannot be opened, remove it and try again."in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let srcs =
  let doc = "Source file(s) to copy." in
  Arg.(non_empty & pos_left ~rev:true 0 file [] & info [] ~docv:"SOURCE" ~doc)

let dest =
  let doc = "Destination of the copy. Must be a directory if there is more \
             than one $(i,SOURCE)." in
  let docv = "DEST" in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv ~doc)

let cmd =
  let doc = "Copy files" in
  let man_xrefs =
    [ `Tool "mv"; `Tool "scp"; `Page ("umask", 2); `Page ("symlink", 7) ]
  in
  let man =
    [ `S Manpage.s_bugs;
      `P "Email them to <bugs@example.org>."; ]
  in
  let info = Cmd.info "cp" ~version:"v1.3.0" ~doc ~man ~man_xrefs in
  Cmd.v info Term.(ret (const cp $ verbose $ recurse $ force $ srcs $ dest))


let main () = exit (Cmd.eval cmd)
let () = main ()
