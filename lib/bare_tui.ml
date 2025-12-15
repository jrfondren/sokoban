open Effect.Deep

let rec read_move () =
  let open Game in
  print_string "wasd, e/h, q? ";
  match read_line () with
  | "w" -> Move (0, -1)
  | "a" -> Move (-1, 0)
  | "s" -> Move (0, 1)
  | "d" -> Move (1, 0)
  | "e" -> Easier
  | "h" -> Harder
  | "q" -> Quit
  | _ -> read_move ()

let play () =
  let moves = ref 0 in
  let g =
    try Grid.levels.(0) |> Grid.copy |> Game.play with
    | effect Game.Draw g, k ->
      print_newline ();
      Array.iter
        (fun row ->
          print_bytes row;
          print_newline ())
        g;
      continue k ()
    | effect Game.GetMove (), k ->
      incr moves;
      let move = read_move () in
      (match move with
      | Easier | Harder -> moves := 0
      | _ -> ());
      continue k move
  in
  if Grid.unfinished g = 0 then
    Printf.printf "You won in %d moves!\n" !moves
  else
    ()
