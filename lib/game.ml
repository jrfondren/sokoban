open Effect

type move =
  | Quit
  | Harder
  | Easier
  | Move of (int * int)

type _ Effect.t += Draw : Grid.t -> unit t
type _ Effect.t += GetMove : unit -> move t

let rec play g =
  let open Grid in
  if unfinished g = 0 then
    g
  else (
    perform (Draw g);
    match perform (GetMove ()) with
    | Quit -> g
    | Harder -> play (copy levels.(1))
    | Easier -> play (copy levels.(0))
    | Move dir ->
      let at = where g in
      look g ~at ~dir |> move |> place g ~at ~dir;
      play g)
