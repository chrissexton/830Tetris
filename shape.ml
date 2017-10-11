open Printf

(** representation of a tetrimino
  * shape * orientation * x * y
* *)

type tetromino = (int array array * int * (int * int) * int)

let s, z, t, i, l, l' = (0, 1, 2 ,3, 4, 5)
let north, south, east, west = (0, 1, 2, 3)
let left, right, down, flip = (0, 1, 2, 3)

let small_shapes = [|
  [| (* o *)
    [|1;1;0;0; 1;1;0;0; 0;0;0;0; 0;0;0;0;|];
  |];
  [| (* s *)
    [|0;1;0;0; 1;0;0;0; 0;0;0;0; 0;0;0;0;|];
    [|1;0;0;0; 0;1;0;0; 0;0;0;0; 0;0;0;0;|];
  |];
  [| (* j *)
    [|1;1;0;0; 0;1;0;0; 0;0;0;0; 0;0;0;0;|];
    [|1;1;0;0; 1;0;0;0; 0;0;0;0; 0;0;0;0;|];
    [|1;0;0;0; 1;1;0;0; 0;0;0;0; 0;0;0;0;|];
    [|0;1;0;0; 1;1;0;0; 0;0;0;0; 0;0;0;0;|];
  |];
  [| (* l *)
    [|1;0;0;0; 1;0;0;0; 0;0;0;0; 0;0;0;0;|];
    [|1;1;0;0; 0;0;0;0; 0;0;0;0; 0;0;0;0;|];
  |];
  [| (* . *)
    [|1;0;0;0; 0;0;0;0; 0;0;0;0; 0;0;0;0;|];
  |]
|]

let full_shapes = [|
  [| (* s *)
    [|0;1;0;0; 1;1;0;0; 1;0;0;0; 0;0;0;0;|];
    [|1;1;0;0; 0;1;1;0; 0;0;0;0; 0;0;0;0;|];
    [|0;1;0;0; 1;1;0;0; 1;0;0;0; 0;0;0;0;|];
    [|1;1;0;0; 0;1;1;0; 0;0;0;0; 0;0;0;0;|];
  |];
  [| (* z *)
    [|1;0;0;0; 1;1;0;0; 0;1;0;0; 0;0;0;0;|];
    [|0;1;1;0; 1;1;0;0; 0;0;0;0; 0;0;0;0;|];
    [|1;0;0;0; 1;1;0;0; 0;1;0;0; 0;0;0;0;|];
    [|0;1;1;0; 1;1;0;0; 0;0;0;0; 0;0;0;0;|];
  |];
  [| (* t *)
    [|1;1;1;0; 0;1;0;0; 0;0;0;0; 0;0;0;0;|];
    [|0;1;0;0; 0;1;1;0; 0;1;0;0; 0;0;0;0;|];
    [|0;1;0;0; 1;1;1;0; 0;0;0;0; 0;0;0;0;|];
    [|0;1;0;0; 1;1;0;0; 0;1;0;0; 0;0;0;0;|];
  |];
  [| (* i *)
    [|1;0;0;0; 1;0;0;0; 1;0;0;0; 1;0;0;0;|];
    [|1;1;1;1; 0;0;0;0; 0;0;0;0; 0;0;0;0;|];
    [|1;0;0;0; 1;0;0;0; 1;0;0;0; 1;0;0;0;|];
    [|1;1;1;1; 0;0;0;0; 0;0;0;0; 0;0;0;0;|];
  |];
  [| (* l *)
    [|1;1;0;0; 1;0;0;0; 1;0;0;0; 0;0;0;0;|];
    [|1;0;0;0; 1;1;1;0; 0;0;0;0; 0;0;0;0;|];
    [|0;1;0;0; 0;1;0;0; 1;1;0;0; 0;0;0;0;|];
    [|1;1;1;0; 0;0;1;0; 0;0;0;0; 0;0;0;0;|];
  |];
  [| (* j *)
    [|1;1;0;0; 0;1;0;0; 0;1;0;0; 0;0;0;0;|];
    [|1;1;1;0; 1;0;0;0; 0;0;0;0; 0;0;0;0;|];
    [|1;0;0;0; 1;0;0;0; 1;1;0;0; 0;0;0;0;|];
    [|0;0;1;0; 1;1;1;0; 0;0;0;0; 0;0;0;0;|];
  |];
  [| (* o *)
    [|1;1;0;0; 1;1;0;0; 0;0;0;0; 0;0;0;0;|];
    [|1;1;0;0; 1;1;0;0; 0;0;0;0; 0;0;0;0;|];
    [|1;1;0;0; 1;1;0;0; 0;0;0;0; 0;0;0;0;|];
    [|1;1;0;0; 1;1;0;0; 0;0;0;0; 0;0;0;0;|];
  |];
|]

let starting_col = (Config.cols - 2) / 2

let shapes = ref (
  if Config.small_shapes
  then small_shapes
  else full_shapes)

let num_shapes = ref (Array.length !shapes)

let get_shape shape =
  !shapes.(shape)

let new_tetromino direction : int * tetromino =
  Random.self_init ();
  let shape = Random.int !num_shapes in
  let color = shape in
    (shape, (get_shape(shape), direction, (starting_col, 0), color))

let fall (shape, direction, (col, row), color) =
  (shape, direction, (col, row+1), color)

let move (shape, direction, (col, row), color) action : tetromino =
  let get_new_shape action =
    if action = 0
    then ((direction + 1) mod (Array.length shape), col, row)
    else if action = 1
    then (direction, col, row + 1)
    else if action = 2
    then (direction, col - 1, row)
    else if action = 3
    then (direction, col + 1, row)
    else (direction, col, row)
  in
  let direction, col, row = get_new_shape action in
    (shape, direction, (col, row), color)

let width_of (shape, direction, (col, row), color) = 
  let w = ref 0 in
    for j = 0 to 3 do
      for i = 0 to 3 do
        if shape.(direction).(i + j * 4) = 1
        then w := i+1
      done
    done;
    !w

let possible_actions shape =
  let rec make_direction flips max_n dir =
    if max_n = 0
    then []
    else begin
      let new_act = (flips @ (Util.make_list dir max_n) @ [4]) in
       new_act :: (make_direction flips (max_n-1) dir)
    end
  in
  let actions = ref [[4]] in
  let s, _, _, _ = shape in
    for flip = 0 to (Array.length s)-1 do
      let flip_a = Util.make_list 0 flip in
      let piece = List.fold_left (fun s a -> move s a) shape flip_a in
      let piece_w = width_of piece in
      let lefts = starting_col + 1 - piece_w in
      let rights = (Config.cols-1) - lefts - piece_w in
      let lefts = make_direction flip_a lefts 2 in
      let rights = make_direction flip_a rights 3 in
        actions := lefts @ rights @ !actions
    done;
    let acts = Util.number_list !actions in
      acts
