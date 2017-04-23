open Core.Std

let borders = [| 
  [|"┏";"┳";"┓"|]; 
  [|"┣";"╋";"┫"|]; 
  [|"┗";"┻";"┛"|];
  [|"┃";"┃";"┃"|]
|]

let hline = "━"
let vline = "┃"


type column_hint = {
  label: string;
  width: int;
}

let draw_n n ch =
  for i = 1 to n do
    print_string ch
  done

let draw_row index cell_fn data =
  let border = borders.(index) in
  let left = border.(0) in
  let center = border.(1) in
  let right = border.(2) in
  print_string left;
  let rec loop = function
    | [cell] -> cell_fn cell; loop []
    | cell::rest -> cell_fn cell; print_string center; loop rest
    | [] -> print_string right in
  loop data;
  print_string "\n"

let string_format s n = 
  let len = String.length s in
  if len > n then
    String.slice s 0 (len - n + 1)
  else
    s ^ (String.init (n - len) ~f:(fun _ -> ' '))

let draw_data_row column_hints row =
  let data = List.zip_exn column_hints row in
  let cell_fn ({width; _}, text) = print_string (string_format text width) in
  draw_row 3 cell_fn data

let draw_separator index column_hints =
  let cell_fn {width; _} = draw_n width hline in
  draw_row index cell_fn column_hints

let draw_table column_hints rows =
  draw_separator 0 column_hints;
  draw_data_row column_hints (List.map ~f:(fun {label; _} -> label) column_hints);
  draw_separator 1 column_hints;
  List.iter rows ~f:(fun row -> draw_data_row column_hints row);
  draw_separator 2 column_hints

let tabulate filename = 
  let lines = In_channel.read_lines filename in
  let lines = List.map ~f:(fun line -> String.split ~on:'\t' line) lines in
  let header::rows = lines in
  let columns = List.zip_exn header (List.transpose_exn rows) in 
  let column_hints = List.map columns ~f:(fun (label, column) -> 
    let width = Option.value_exn (List.max_elt (List.map ~f:String.length (label::column))
        ~cmp:(fun a b -> a - b)) in
    {label; width}
  ) in
  draw_table column_hints rows


let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary: "Render input data as a table"
    ~readme:(fun () -> "Render input data as a table")
    spec
    (fun filename () ->  tabulate filename)

let () = Command.run command