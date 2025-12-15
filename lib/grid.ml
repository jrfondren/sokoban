(** Given (current_pos, facing, after_facing), return the tuple that these tiles
    should be transformed to with a step forward *)
let move = function
  | '@', ' ', x -> (' ', '@', x)
  | '@', '.', x -> (' ', '&', x)
  | '&', ' ', x -> ('.', '@', x)
  | '&', '.', x -> ('.', '&', x)
  | '@', '$', ' ' -> (' ', '@', '$')
  | '@', '$', '.' -> (' ', '@', '*')
  | '&', '$', ' ' -> ('.', '@', '$')
  | '&', '$', '.' -> ('.', '@', '*')
  | '@', '*', ' ' -> (' ', '&', '$')
  | '@', '*', '.' -> (' ', '&', '*')
  | '&', '*', ' ' -> ('.', '&', '$')
  | '&', '*', '.' -> ('.', '&', '*')
  | unchanged -> unchanged

(** return count of boxes not in goals *)
let unfinished =
  Array.fold_left
    (fun n ->
      Bytes.fold_left
        (fun n -> function
          | '$' -> n + 1
          | _ -> n)
        n)
    0

(** x,y coordinate of the player, or raises Not_found. Zero-based coordinates
    with the top left of the grid as origin *)
let where g =
  let exception Return of (int * int) in
  try
    for y = 0 to Array.length g - 1 do
      for x = 0 to Bytes.length g.(y) - 1 do
        match Bytes.get g.(y) x with
        | '@' | '&' -> raise_notrace (Return (x, y))
        | _ -> ()
      done
    done;
    raise Not_found
  with Return at -> at

(** (at, facing, facing+1) *)
let look g ~at:(x, y) ~dir:(dx, dy) =
  let peek x y = try Bytes.get g.(y) x with Invalid_argument _ -> '#' in
  (peek x y, peek (x + dx) (y + dy), peek (x + (2 * dx)) (y + (2 * dy)))

(** update grid given (at, facing, facing+1) tiles *)
let place g ~at:(x, y) ~dir:(dx, dy) (a, b, c) =
  let put x y o = try Bytes.set g.(y) x o with Invalid_argument _ -> () in
  put x y a;
  put (x + dx) (y + dy) b;
  put (x + (2 * dx)) (y + (2 * dy)) c

(** return a mutable copy of a level, to play it *)
let copy = Array.map Bytes.copy

let levels =
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
      |> Seq.map Bytes.of_string
      |> Array.of_seq)

type t = bytes array
