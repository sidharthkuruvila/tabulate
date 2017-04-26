open Core.Std


module Csv_util = struct

  let csv_channel ~csv_header ~csv_has_header ~csv_separator ~chan = 
      Csv.of_channel ?header:csv_header ~has_header:csv_has_header ~separator:csv_separator chan

  let read_line chan = 
    try
      Some (Csv.next chan)
    with End_of_file -> None
    | _ -> failwith "Unknown"
   
  let extract_header ~chan = 
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
end


module Console = struct
  let get_terminal_size _ =
    let col_count = Terminal_size.get_columns () in
    let row_count = Terminal_size.get_rows () in
    Option.both row_count col_count

  let move_cursor_to row col = 
    Printf.printf "\x1B[%d;%df" row col

  let print_line line = 
    print_endline line

  let print_string line = 
    print_string line;
    Out_channel.flush Out_channel.stdout

  let rec render = function
    | line :: [] -> print_string line
    | line :: rest -> print_line line; render rest
    | [] -> ()
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



module Table_hints = struct

  type column_hint = {
    label: string;
    header_width: int;
    data_width: int;
    width: int

  }

  let caclulate ~header ~buffer ~count = 
    let open Result.Monad_infix in
    let rows = List.init count ~f:(Buffer.get buffer) in 
    List.transpose rows |> Result.of_option ~error:"Column counts not the same for all rows" >>= fun transposed ->
    List.zip header transposed |> Result.of_option ~error:"Header size does not match column count" >>| fun columns -> 
    let column_hints = List.map columns ~f:(fun (label, column) ->
      let header_width = String.length label in 
      let data_width = Option.value_exn (List.max_elt (List.map ~f:String.length (column))
        ~cmp:(fun a b -> a - b)) in
      let width = max header_width data_width in
      {label; width; header_width; data_width}
    ) in
    column_hints

end

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
  let cell_fn ({Table_hints.width; _}, text) = string_format text width in
  draw_row borders.(3) cell_fn data

let draw_separator index column_hints =
  let cell_fn {Table_hints.width; _} = draw_n width hline in
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

let tabulate_lines ~table_hints ~show_header ~buffer ~count= 
  let open Result.Monad_infix in
  let rows = List.init count ~f:(Buffer.get buffer) in 
  table_hints >>| fun column_hints ->  
  draw_table ~show_header ~column_hints ~rows

let read_n_lines buffer chan n =
  let rec loop n = 
    if n > 0 then
      match Csv_util.read_line chan with
        | Some line -> Buffer.add buffer line; loop n
        | None -> () in
  loop n

let tabulate_loop ~chan ~show_header ~row_count ~buffer =
  let open Result.Monad_infix in
  let mk_screen_lines ~row_count ~header ~buffer = 
    let count = min row_count (Buffer.length buffer) in
    let table_hints = Table_hints.caclulate ~header ~buffer ~count in
    tabulate_lines  ~show_header ~table_hints ~buffer ~count in
  let header_result = Csv_util.extract_header ~chan in
  let add_first_line first_line = 
    match first_line with
      | Some first_line -> Buffer.add buffer first_line
      | None -> Option.iter (Csv_util.read_line chan) ~f:(Buffer.add buffer) in
  header_result >>= fun (header, first_line) -> 
  add_first_line first_line;
  let rec loop screen_lines = 
    let screen_lines_count = min (List.length screen_lines) row_count in
    let screen_lines_trunc = List.slice screen_lines 0 screen_lines_count in
    Console.render screen_lines_trunc;
    match Csv_util.read_line chan with
    | None -> Result.Ok ()
    | Some line -> 
      Buffer.add buffer line;
      mk_screen_lines ~row_count ~header ~buffer >>= fun lines -> 
      Console.move_cursor_to (row_count - screen_lines_count + 1) 0;
      loop lines in
  mk_screen_lines ~row_count ~header ~buffer >>= loop

let tabulate ~buffer_size ~show_header ~csv_has_header ~csv_header ~csv_separator ~filename =
  let csv_channel ~chan = Csv_util.csv_channel ~csv_header ~csv_has_header ~csv_separator ~chan in
  let buffer = Buffer.init buffer_size in
  let tabulate_lines = tabulate_lines ~show_header in 
  
  let tabulate_chan chan = 
    let header_result = Csv_util.extract_header ~chan in
    let screen_lines = Result.bind header_result (fun (header, first_line) ->
      Option.iter first_line ~f:(Buffer.add buffer);
      read_n_lines buffer chan buffer_size;
      let count = Buffer.length buffer in
      let table_hints = Table_hints.caclulate ~header ~buffer ~count in
      tabulate_lines ~table_hints ~buffer ~count) in
    match screen_lines with
    | Result.Ok lines -> Console.render lines
    | Result.Error msg -> Printf.eprintf "Failed to generate table: %s\n" msg in
  match filename with
  | Some filename -> 
    In_channel.with_file filename ~f:(fun chan -> tabulate_chan (csv_channel ~chan))
  | None -> 
    let chan = csv_channel ~chan:In_channel.stdin in
    match Console.get_terminal_size () with
    | None -> tabulate_chan chan
    | Some (row_count, _) -> 
      match tabulate_loop ~chan ~show_header ~row_count ~buffer with 
      | Result.Ok _ -> ()
      | Result.Error msg -> Printf.eprintf "Failed to generate table: %s\n" msg

let column_names =
  Command.Spec.Arg_type.create
      (fun str -> String.split ~on:',' str)  

let spec =
  let open Command.Spec in
  empty
  (*+> flag "-columns" (optional column_names)
    ~doc:"LIST Comma separated list of column names/ids to dislplay"*)
  +> flag "-buffer-size" (optional_with_default 1000 int)
    ~doc:"INT The number of lines the program can store at a time"
  +> flag "-hide-header" no_arg
    ~doc:" Hide the header"
  +> flag "-csv-no-header-row" no_arg
    ~doc:" Don't use the first row as the header row, use numeric id's instead unless -header is provided"
  +> flag "-csv-header" (optional column_names)
    ~doc:"LIST Column labels for csv/tsv file"
  +> flag "-csv-separator" (optional_with_default '\t' char)
    ~doc:" The column separater used by the csv parser, this defaults to the tab character"
  +> anon (maybe ("filename" %: string))

let command =
  Command.basic
    ~summary: "Render input data as a table"
    ~readme:(fun () -> "Render input data as a table")
    spec
    (fun buffer_size hide_header no_header_row csv_header csv_separator filename () ->  tabulate ~buffer_size
      ~show_header:(not hide_header) 
      ~csv_has_header:((not no_header_row) || Option.is_some csv_header) 
      ~csv_header ~csv_separator ~filename)

let () = Command.run command