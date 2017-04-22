open Core.Std

let draw_table filename = Printf.printf "Filename: %s\n" filename

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"Render input data as a table"
    ~readme:(fun () -> "Render input data as a table")
    spec
    (fun filename () ->  draw_table filename)

let () = Command.run command