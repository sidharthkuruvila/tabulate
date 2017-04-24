open Core.Std


module Buffer = struct
  type buffer = {
    buffer: string array;
    mutable next: int;
    mutable last: int;
  }

  let init size = 
    {
      buffer = Array.create ~len:(size+1) "";
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
let vline = "┃"


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


let extract_header ~has_header ~header ~chan = 
  match header with 
    | Some header -> Result.Ok (header, None)
    | None -> match In_channel.input_line chan with
      | Some header ->  
        let header_labels = String.split ~on:'\t' header in
        if has_header then 
          Result.Ok (header_labels, None) 
        else 
          let column_id_ints = List.range 0 (List.length header_labels) in
          let column_ids = List.map column_id_ints ~f:(fun n -> "column_" ^ (string_of_int n)) in
          Result.Ok (column_ids, Some header)
      | None -> Result.Error "Unable to parse header row"


let tabulate_lines  ~columns ~show_header ~has_header ~header ~buffer = 
  let open Result.Monad_infix in
  let rows = List.init (Buffer.length buffer) ~f:(fun n -> 
    let line = Buffer.get buffer n in
    String.split ~on:'\t' line) in 
  let prepare_table_result = 
    List.transpose rows |> Result.of_option ~error:"Column counts not the same for all rows" >>= fun transposed ->
    List.zip header transposed |> Result.of_option ~error:"Header sized does not match column count" >>| fun columns -> 
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
      match In_channel.input_line chan with
        | Some line -> Buffer.add buffer line; loop n
        | None -> () in
  loop n

let tabulate ~header ~columns ~buffer_size ~show_header ~has_header ~filename =
  let buffer = Buffer.init buffer_size in
  let tabulate_lines = tabulate_lines ~columns ~show_header ~has_header in 
  let print_screen_lines lines_result =
    match lines_result with 
      | Result.Ok lines ->
        List.iter lines ~f:(fun line ->
          print_endline line
        )
      | Result.Error msg -> Printf.eprintf "Failed to generate table: %s\n" msg in
  let tabulate_chan chan = 
    let header_result = extract_header ~has_header ~header ~chan in
    let screen_lines = Result.bind header_result (fun (header, first_line) ->
      Option.iter first_line ~f:(Buffer.add buffer);
      read_n_lines buffer chan buffer_size;
      tabulate_lines ~header ~buffer) in
    print_screen_lines screen_lines in
  match filename with
  | Some filename -> 
    In_channel.with_file filename ~f:tabulate_chan
  | None -> 
    let col_count = Terminal_size.get_columns () in
    let row_count = Terminal_size.get_rows () in
    if Option.is_none col_count || Option.is_none row_count then
      tabulate_chan In_channel.stdin
    else
      let row_count = Option.value_exn row_count in
      let col_count = Option.value_exn col_count in
      print_string (Printf.sprintf "To implement: cols = %d rows = %d\n" col_count row_count) 
    

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
  +> flag "-buffer-size" (optional_with_default 1000 int)
    ~doc:"The number of lines the program can store at a time"
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
    (fun header columns buffer_size hide_header no_header_row filename () ->  tabulate ~header ~columns ~buffer_size
      ~show_header:(not hide_header) 
      ~has_header:((not no_header_row) || Option.is_some header) ~filename)

let () = Command.run command