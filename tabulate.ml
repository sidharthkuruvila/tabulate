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
  for _ = 1 to n do
    print_string ch
  done

let draw_row border cell_fn data =
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
  draw_row borders.(3) cell_fn data

let draw_separator index column_hints =
  let cell_fn {width; _} = draw_n width hline in
  draw_row index cell_fn column_hints

let draw_table show_header column_hints rows =
  draw_separator borders.(0) column_hints;
  if show_header then begin
    draw_data_row column_hints (List.map ~f:(fun {label; _} -> label) column_hints);
    draw_separator borders.(1) column_hints
  end;
  List.iter rows ~f:(fun row -> draw_data_row column_hints row);
  draw_separator borders.(2) column_hints


let extract_header has_header header lines = 
  match header with 
    | Some header -> Result.Ok (header, lines)
    | None -> match lines with
      | header::rest ->  
        let header_labels = String.split ~on:'\t' header in
        if has_header then 
          Result.Ok (header_labels, rest) 
        else 
          let column_id_ints = List.range 0 (List.length header_labels) in
          let column_ids = List.map column_id_ints ~f:(fun n -> "column_" ^ (string_of_int n)) in
          Result.Ok (column_ids, lines)
      | [] -> Result.Error "Unable to parse header row"


let tabulate_lines header columns show_header has_header lines = 
  let open Result.Monad_infix in
  let header_result = extract_header has_header header lines in
  let prepare_table_result = header_result >>= fun (header, lines) ->
  let rows = List.map ~f:(fun line -> String.split ~on:'\t' line) lines in 
  List.transpose rows |> Result.of_option ~error:"Column counts not the same for all rows" >>= fun transposed ->
  List.zip header transposed |> Result.of_option ~error:"Header sized does not match column count" >>| fun columns -> 
  let column_hints = List.map columns ~f:(fun (label, column) -> 
    let width = Option.value_exn (List.max_elt (List.map ~f:String.length (label::column))
      ~cmp:(fun a b -> a - b)) in
    {label; width}
  ) in
  (column_hints, rows) in
  match prepare_table_result with 
    | Result.Ok (column_hints, rows) -> draw_table show_header column_hints rows
    | Result.Error msg -> Printf.eprintf "Failed to generate table: %s\n" msg


let tabulate header columns show_header has_header filename =
  let tabulate_lines = tabulate_lines header columns show_header has_header in 
  match filename with
  | Some filename -> 
    In_channel.with_file filename ~f:(fun chan -> 
      let lines = In_channel.input_lines chan in
      tabulate_lines lines)
  | None -> tabulate_lines (In_channel.input_lines In_channel.stdin)
  

let column_names =
  Command.Spec.Arg_type.create
      (fun str -> String.split ~on:',' str)  

let spec =
  let open Command.Spec in
  empty
  +> flag "-header" (optional column_names)
    ~doc:"LIST Column labels for csv/tsv file"
  +> flag "-columns" (optional column_names)
    ~doc:"LIST Comma separated list of column names/ids to dislplay"
  +> flag "-hide-header" no_arg 
    ~doc:" Hide the header"
  +> flag "-no-header-row" no_arg
    ~doc:" Don't use the first row as the header row, use numeric id's instead unless -header is provided"
  +> anon (maybe ("filename" %: string))

let command =
  Command.basic
    ~summary: "Render input data as a table"
    ~readme:(fun () -> "Render input data as a table")
    spec
    (fun header columns hide_header no_header_row filename () ->  tabulate header columns 
      (not hide_header) 
      ((not no_header_row) || Option.is_some header) filename)

let () = Command.run command