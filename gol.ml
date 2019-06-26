let tick_time = 0.1

let adjacent_cells i j =
  [ (i - 1, j - 1);
    (i - 1, j);
    (i - 1, j + 1);
    (i, j - 1);
    (i, j + 1);
    (i + 1, j - 1);
    (i + 1, j);
    (i + 1, j + 1) ]

let valid i j n = i >= 0 && i < n && j >= 0 && j < n

let display n board =
  for i = 0 to n - 1 do
    print_string "[ ";
    for j = 0 to n - 1 do
      print_char (if board.(i).(j) then 'X' else ' ')
    done;
    print_endline " ]"
  done;
  print_newline()

let count n board count_board =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do count_board.(i).(j) <- 0 done
  done;
  let update_count i j =
    adjacent_cells i j
    |> List.filter (fun (x, y) -> valid x y n)
    |> List.iter (fun (x, y) -> count_board.(x).(y) <- count_board.(x).(y) + 1)
  in
  let check i j = if valid i j n && board.(i).(j) then update_count i j in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do check i j done
  done

let update n count_board board =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let c = count_board.(i).(j) in
      board.(i).(j) <-
        match board.(i).(j) with
        | true -> c = 2 || c = 3
        | false -> c = 3
    done
  done

let rec loop n board count_board =
  display n board;
  count n board count_board;
  update n count_board board;
  Unix.sleepf tick_time;
  loop n board count_board

let parse_file filename =
  let ic = open_in filename in
  let n = int_of_string (input_line ic) in
  let rec f acc n =
    if n = 0 then List.rev acc else
    let line =
      String.to_seq (input_line ic)
      |> Seq.map (fun c -> int_of_string (String.make 1 c))
      |> Seq.map (fun i -> if i = 1 then true else false)
      |> List.of_seq
      |> Array.of_list
    in
    f (line :: acc) (n - 1)
  in
  (n, Array.of_list (f [] n))

let main () =
  if Array.length Sys.argv <> 2 then begin
    Printf.printf "Usage: %s input_file" Sys.argv.(0);
    exit 1
  end;
  let (n, board) = parse_file Sys.argv.(1) in
  let count_board = Array.make_matrix n n 0 in
  loop n board count_board

let () = main ()