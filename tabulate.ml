open Core.Std

module File_types = struct
  type file_type = 
    | CSV
    | TSV

  let from_string = function
    | "csv" -> Some CSV
    | "tsv" -> Some TSV
    | _ -> None
    
end

module Buffer = struct
  type buffer = {
    buffer: string list array;
    mutable next: int;
    mutable last: int;
  }

  let init size = 
    {
      buffer = Array.create ~len:(size+1) [];
      next = 0;
      last = 0
    }

  let add buffer s =
    let len = Array.length buffer.buffer in
    if (buffer.next + 1) % len = buffer.last then begin
      buffer.last <- (buffer.last + 1) % len
    end;
    buffer.buffer.(buffer.next) <- s;
    buffer.next <- (buffer.next + 1) % len

  let length buffer =
    (buffer.next - buffer.last) % (Array.length buffer.buffer)

  let get buffer idx =
    let len = Array.length buffer.buffer in
    assert (idx < len && idx < length buffer);
    let next = buffer.next in
    buffer.buffer.((next - idx - 1) % len)

end


let borders = [| 
  [|"┏";"┳";"┓"|]; 
  [|"┣";"╋";"┫"|]; 
  [|"┗";"┻";"┛"|];
  [|"┃";"┃";"┃"|]
|]

let hline = "━"

type column_hint = {
  label: string;
  width: int;
}

let draw_n n ch =
  let chl = String.length ch in
  let s = String.create (n * chl) in
  for i = 0 to n - 1 do
    String.blit ~src:ch ~src_pos:0 ~dst:s ~dst_pos:(chl*i) ~len:chl
  done;
  s

let draw_row border cell_fn data =
  let left = border.(0) in
  let center = border.(1) in
  let right = border.(2) in
  let line = left ^ (String.concat ~sep:center (List.map data ~f:cell_fn)) ^ right in
  line

let string_format s n = 
  let len = String.length s in
  if len > n then
    String.slice s 0 (len - n + 1)
  else
    s ^ (String.init (n - len) ~f:(fun _ -> ' '))

let draw_data_row column_hints row =
  let data = List.zip_exn column_hints row in
  let cell_fn ({width; _}, text) = string_format text width in
  draw_row borders.(3) cell_fn data

let draw_separator index column_hints =
  let cell_fn {width; _} = draw_n width hline in
  draw_row index cell_fn column_hints

let draw_table ~show_header ~column_hints ~rows =
  List.concat [
    [draw_separator borders.(0) column_hints];
    if show_header then
      [
        draw_data_row column_hints (List.map ~f:(fun {label; _} -> label) column_hints);
        draw_separator borders.(1) column_hints
      ]
    else 
      [];
    List.map rows ~f:(fun row -> draw_data_row column_hints row);
    [draw_separator borders.(2) column_hints]
  ]

let read_line chan = 
  try
    Some (Csv.next chan)
  with End_of_file -> None
  | _ -> failwith "Unknown"
 
let extract_header ~has_header ~header ~chan = 
  let header = Csv.Rows.header chan in
  if header = [] then
    match read_line chan with
    | Some header_labels ->
          let column_id_ints = List.range 0 (List.length header_labels) in
          let column_ids = List.map column_id_ints ~f:(fun n -> "column_" ^ (string_of_int n)) in
          Result.Ok (column_ids, Some header_labels)
      | None -> Result.Error "Unable to parse header row"
  else
    Result.Ok (header, None)

let tabulate_lines  ~columns ~show_header ~has_header ~header ~buffer ~count= 
  let open Result.Monad_infix in
  let rows = List.init count ~f:(Buffer.get buffer) in 
  let prepare_table_result = 
    List.transpose rows |> Result.of_option ~error:"Column counts not the same for all rows" >>= fun transposed ->
    List.zip header transposed |> Result.of_option ~error:"Header size does not match column count" >>| fun columns -> 
    let column_hints = List.map columns ~f:(fun (label, column) -> 
      let width = Option.value_exn (List.max_elt (List.map ~f:String.length (label::column))
        ~cmp:(fun a b -> a - b)) in
      {label; width}
    ) in
    (column_hints, rows) in
  Result.map prepare_table_result ~f:(fun (column_hints, rows) ->  draw_table ~show_header ~column_hints ~rows)

let read_n_lines buffer chan n =
  let rec loop n = 
    if n > 0 then
      match read_line chan with
        | Some line -> Buffer.add buffer line; loop n
        | None -> () in
  loop n

let tabulate ~header ~columns ~buffer_size ~show_header ~has_header ~format ~filename =
  let csv_channel ~chan = 
    Csv.of_channel ?header ~has_header ~separator:'\t' chan in
  let buffer = Buffer.init buffer_size in
  let tabulate_lines = tabulate_lines ~columns ~show_header ~has_header in 
  let render lines = 
    List.iter lines ~f:(fun line ->
      print_endline line
    ) in
  let tabulate_chan chan = 
    let header_result = extract_header ~has_header ~header ~chan in
    let screen_lines = Result.bind header_result (fun (header, first_line) ->
      Option.iter first_line ~f:(Buffer.add buffer);
      read_n_lines buffer chan buffer_size;
      tabulate_lines ~header ~buffer ~count:(Buffer.length buffer)) in
    match screen_lines with
    | Result.Ok lines -> render lines
    | Result.Error msg -> Printf.eprintf "Failed to generate table: %s\n" msg in
  match filename with
  | Some filename -> 
    In_channel.with_file filename ~f:(fun chan -> tabulate_chan (csv_channel ~chan))
  | None -> 
    let chan = csv_channel In_channel.stdin in
    let col_count = Terminal_size.get_columns () in
    let row_count = Terminal_size.get_rows () in
    if Option.is_none col_count || Option.is_none row_count then
      tabulate_chan chan
    else
      let row_count = Option.value_exn row_count in
      let col_count = Option.value_exn col_count in
      let header_result = extract_header ~has_header ~header ~chan in
      Result.iter header_result ~f:(fun (header, first_line) ->
        match first_line with
          | Some first_line -> Buffer.add buffer first_line
          | None -> Option.iter (read_line chan) ~f:(Buffer.add buffer);
        let count = min row_count (Buffer.length buffer) in
        let screen_lines = tabulate_lines ~header ~buffer ~count in
        let rec loop screen_lines =
          let screen_lines_count = List.length screen_lines in
          Option.iter (read_line chan) ~f:(fun line ->
            Buffer.add buffer line;
            let count = min row_count (Buffer.length buffer) in
            let updated_screen_lines = tabulate_lines ~header ~buffer ~count in
            match updated_screen_lines with
            | Result.Ok lines -> 
                Printf.printf "\x1B[%d;0f" (row_count - screen_lines_count);
                render lines; 
                loop lines
            | Result.Error msg -> Printf.printf "Failed to generate table: %s\n" msg) in
        match screen_lines with
        | Result.Ok lines -> render lines; loop lines
        | Result.Error msg -> Printf.printf "Failed to generate table: %s\n" msg)

let column_names =
  Command.Spec.Arg_type.create
      (fun str -> String.split ~on:',' str)  

let file_type =
  Command.Spec.Arg_type.create
      (fun str -> Option.value_exn (File_types.from_string str))

let spec =
  let open Command.Spec in
  empty
  +> flag "-header" (optional column_names)
    ~doc:"LIST Column labels for csv/tsv file"
  +> flag "-columns" (optional column_names)
    ~doc:"LIST Comma separated list of column names/ids to dislplay"
  +> flag "-buffer-size" (optional_with_default 1000 int)
    ~doc:"INT The number of lines the program can store at a time"
  +> flag "-hide-header" no_arg
    ~doc:" Hide the header"
  +> flag "-no-header-row" no_arg
    ~doc:" Don't use the first row as the header row, use numeric id's instead unless -header is provided"
  +> flag "-format" (optional_with_default File_types.TSV file_type)
    ~doc: "The file format of the input file, defaults to tsv"
  +> anon (maybe ("filename" %: string))

let command =
  Command.basic
    ~summary: "Render input data as a table"
    ~readme:(fun () -> "Render input data as a table")
    spec
    (fun header columns buffer_size hide_header no_header_row format filename () ->  tabulate ~header ~columns ~buffer_size
      ~show_header:(not hide_header) 
      ~has_header:((not no_header_row) || Option.is_some header) ~format ~filename)

let () = Command.run command