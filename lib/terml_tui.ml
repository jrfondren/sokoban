open Terml

let moves = ref 0

let rec read_move chan =
  let open Events in
  let open Game in
  match Event.sync (Event.receive chan) with
  | Events.Key {code; _} -> (
    match code with
    | Up -> Move (0, -1)
    | Left -> Move (-1, 0)
    | Down -> Move (0, 1)
    | Right -> Move (1, 0)
    | Char "e" ->
      moves := 0;
      Easier
    | Char "h" ->
      moves := 0;
      Harder
    | Char "q" -> Quit
    | _ -> read_move chan)

let redraw g =
  let q = Queue.create () in
  let cmd c = Queue.add c q in
  cmd @@ Command.Terminal Terminal.(ClearScreen All);
  cmd @@ Command.Cursor Cursor.(MoveTo (1, 1));
  cmd
  @@ Command.Print
       (Printf.sprintf
          "Move %d (e/h for easier/harder level, arrow keys to move)" !moves);
  Array.iteri
    (fun i row ->
      cmd @@ Command.Cursor Cursor.(MoveTo (i + 2, 1));
      Array.iter
        (fun tile -> cmd @@ Command.Print (Grid.string_of_tile tile))
        row)
    g;
  Command.execute (Queue.to_seq q |> List.of_seq)

let play () =
  let open Effect.Deep in
  moves := 0;
  let chan = Events.poll () in
  let g =
    try Grid.levels.(0) |> Grid.copy |> Game.play with
    | effect Game.Draw g, k ->
      redraw g;
      continue k ()
    | effect Game.GetMove (), k ->
      incr moves;
      continue k (read_move chan)
  in
  if Grid.unfinished g = 0 then
    Printf.sprintf "You won in %d moves!" !moves
  else
    "Maybe another time?"

let play () =
  let term = Terminal.enable_raw_mode () in
  Command.execute [Command.Terminal Terminal.EnterAlternateScreen];
  let msg =
    Fun.protect
      ~finally:(fun () ->
        Command.execute [Command.Terminal Terminal.LeaveAlternateScreen];
        Terminal.disable_raw_mode term)
      play
  in
  print_endline msg
