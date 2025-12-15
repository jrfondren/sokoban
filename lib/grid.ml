exception BadTile of char
(** invalid ASCII representation of a game *)

exception AtNowhere
(** player character does not exist in the game *)

(** game tiles *)
type tile =
  | Nil  (** empty space, permitting travel *)
  | At  (** you, the player character *)
  | Box  (** a pushable box *)
  | Goal  (** marking where boxes must go *)
  | AtGoal  (** the player standing on a goal *)
  | BoxGoal  (** a box on a goal *)
  | Wall  (** a wall *)

(** ASCII for traditional representation of Sokoban games, but as strings so
    that unicode can also be used *)
let string_of_tile = function
  | Nil -> " "
  | At -> "@"
  | Box -> "$"
  | Goal -> "."
  | AtGoal -> "&"
  | BoxGoal -> "*"
  | Wall -> "#"

let tile_of_char = function
  | ' ' -> Nil
  | '@' -> At
  | '$' -> Box
  | '.' -> Goal
  | '&' -> AtGoal
  | '*' -> BoxGoal
  | '#' -> Wall
  | c -> raise (BadTile c)

let draw fmt =
  Array.iter (fun row ->
      Array.to_seq row
      |> Seq.map string_of_tile
      |> Seq.iter (Printf.fprintf fmt "%s");
      Printf.fprintf fmt "\n")

(** Given (current_pos, facing, after_facing), return the tuple that these tiles
    should be transformed to with a step forward *)
let move = function
  | At, Nil, x -> (Nil, At, x)
  | At, Goal, x -> (Nil, AtGoal, x)
  | AtGoal, Nil, x -> (Goal, At, x)
  | AtGoal, Goal, x -> (Goal, AtGoal, x)
  | At, Box, Nil -> (Nil, At, Box)
  | At, Box, Goal -> (Nil, At, BoxGoal)
  | AtGoal, Box, Nil -> (Goal, At, Box)
  | AtGoal, Box, Goal -> (Goal, At, BoxGoal)
  | At, BoxGoal, Nil -> (Nil, AtGoal, Box)
  | At, BoxGoal, Goal -> (Nil, AtGoal, BoxGoal)
  | AtGoal, BoxGoal, Nil -> (Goal, AtGoal, Box)
  | AtGoal, BoxGoal, Goal -> (Goal, AtGoal, BoxGoal)
  | unchanged -> unchanged

(** return count of boxes not in goals *)
let unfinished =
  Array.fold_left
    (fun n ->
      Array.fold_left
        (fun n -> function
          | Box -> n + 1
          | _ -> n)
        n)
    0

(** x,y coordinate of the player, or raises AtNowhere. Zero-based coordinates
    with the top left of the grid as origin *)
let where g =
  let exception Return of (int * int) in
  try
    for y = 0 to Array.length g - 1 do
      for x = 0 to Array.length g.(y) - 1 do
        match g.(y).(x) with
        | At | AtGoal -> raise (Return (x, y))
        | _ -> ()
      done
    done;
    raise AtNowhere
  with Return at -> at

(** (at, facing, facing+1) *)
let look g ~at:(x, y) ~dir:(dx, dy) =
  let peek x y = try g.(y).(x) with Invalid_argument _ -> Wall in
  (peek x y, peek (x + dx) (y + dy), peek (x + (2 * dx)) (y + (2 * dy)))

(** update grid given (at, facing, facing+1) tiles *)
let place g ~at:(x, y) ~dir:(dx, dy) (a, b, c) =
  let put x y o = try g.(y).(x) <- o with Invalid_argument _ -> () in
  put x y a;
  put (x + dx) (y + dy) b;
  put (x + (2 * dx)) (y + (2 * dy)) c

(** return a mutable copy of a level, to play it *)
let copy g = Iarray.to_seq g |> Seq.map Iarray.to_array |> Array.of_seq

let levels : tile iarray iarray array =
  [|
    {|
 #####
 #.. #
###  #
# $  #
# $ ##
#@  #
#####|};
    {|
        #####
        #   #
        #$  #
      ###  $##
      #  $ $ #
    ### # ## #   ######
    #   # ## #####  ..#
    # $  $          ..#
    ##### ### #@##  ..#
        #     #########
        #######|};
  |]
  |> Array.map (fun m ->
      String.split_on_char '\n' m
      |> List.tl
      |> List.to_seq
      |> Seq.map (fun row ->
          String.to_seq row |> Seq.map tile_of_char |> Iarray.of_seq)
      |> Iarray.of_seq)

type t = tile array array
