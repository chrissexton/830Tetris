(** representation of the tetris pit *)
open Printf

let background = Config.background
let foreground = Config.foreground
let cols = Config.cols
let rows = Config.rows

let line_count = ref 0

let new_falling falling shape =
  let s, f = (Shape.new_tetromino 0) in
    falling := f;
    shape := s

let new_empty_block _ =
  (** return RGB=background empty block *)
  background

(* pit size is 12 * 21 or 10x20 + border space *)
let make_pit () =
  let new_row i =
    let new_block = ref background in
      if i = rows-1
      then new_block := foreground;
      let row = Array.make cols !new_block in
        row.(0) <- foreground;
        row.(Array.length row-1) <- foreground;
        row
  in
  let rows = Array.init rows new_row in
    rows

let copy_pit pit =
  let copy_row i =
    Array.copy pit.(i)
  in
  let rows = Array.init rows copy_row in
    rows

let get_block pit x y =
  (!pit).(y).(x)

let is_filled pit x y =
  let block = get_block pit x y in
    block != background

let block_to_str pit x y =
  if is_filled pit x y
  then "##"
  else "__"

let print_pit chan pit =
  for y = 0 to rows-1 do
    for x = 0 to cols-1 do
      fprintf chan "%s" (block_to_str pit x y)
    done;
    fprintf chan "\n"
  done;
  fprintf chan "\n%!"

let get_tops pit =
  let max_r = ref rows in
  let rec max_row r c =
    if !pit.(r).(c) != background
    then r
    else max_row (r+1) c
  in
  let find_max c _ =
    if c = 0 || c = cols-1
    then rows
    else
      let r = max_row 1 c in
        if r < !max_r
        then max_r := r;
        r
  in
  let tops = Array.make cols 0 in
  let tops = Array.mapi find_max tops in
  let tops = Array.sub tops 1 (cols-2) in
    (!max_r, tops)
    
let top_two_lines pit shape =
  let max_r, tops = get_tops pit in
  let filter_top height =
    if height = max_r
    then 1
    else 0
  in
  let filter_second idx height =
    (** a is the top row for colmn idx+1 (take out the side wall) *)
    if max_r > rows-2
    then 0
    else
      let cell = !pit.(max_r+1).(idx+1) in
      if cell = background
      then 0
      else 1
  in
  let add_bit x bit =
    (x lsl 1) lor bit
  in
  let bits = Array.map filter_top tops in
  let bits = Array.append bits (Array.mapi filter_second tops) in
    ((Array.fold_left add_bit 0 bits) lsl 3) + !shape

let check_loss pit =
  fst (get_tops pit) = 1

let collision pit (shape, direction, (col, row), color) =
  let check_cell i cell =
    let bx = i mod 4 in
    let by = i / 4 in
    let px = bx + col in
    let py = row + by in
      if px >= 0 && px < cols && py >= 0 && py < rows
      then (!pit).(py).(px) != background && cell = 1
      else false
  in
  let results = Array.mapi check_cell shape.(direction) in
    Array.fold_left (fun x y -> x || y) false results

let at_rest pit (shape, direction, (col, row), color) =
  (** check only the case that we hit a block below ourselves *)
  let check_cell i cell =
    let bx = i mod 4 in
    let by = i / 4 in
    let px = bx + col in
    let py = row + by in
      if px > 0 && px < cols-1 && py > 0 && py < rows
      then (!pit).(py).(px) != background && cell = 1
      else false
  in
  let results = Array.mapi check_cell shape.(direction) in
    Array.fold_left (fun x y -> x || y) false results

let shift_down pit line =
  let shift_block i j c =
    try
      (!pit).(i).(j) <- (!pit).(i-1).(j)
    with Invalid_argument n ->
      fprintf stderr "invalid set %s: %i %i %i\n%!" n i j (i-1)
  in
  for i = line downto 1 do
      Array.iteri (shift_block i) (!pit).(i)
  done

let check_lines pit =
  let is_line i =
    Array.fold_left (fun j c -> j && c != background) true !pit.(i)
  in
    for i = 0 to rows-2 do
      if is_line i
      then begin
        line_count := !line_count + 1;
        shift_down pit i
      end
    done

let add_to_pit pit falling current_shape (shape, direction, (col, row), color) =
  let solidify i cell =
    let bx = i mod 4 in
    let by = i / 4 in
    let px = bx + col in
    let py = row + by in
      if cell = 1
      then (!pit).(py).(px) <- Config.tetromino_colors.(color)
  in
  Array.iteri solidify shape.(direction);
  new_falling falling current_shape;
  check_lines pit;
  if check_loss pit
  then raise Util.Loss

let fall pit falling shape =
  let fallen = Shape.fall !falling in
    (* check bottom of shape upwards for a collision *)
  let collide = collision pit fallen in
    if not collide
    then begin
      falling := fallen;
      false
    end
    else if collide && at_rest pit fallen
    then begin
      add_to_pit pit falling shape !falling;
      true
    end
    else false

exception Fallen
let move_shape pit falling falling_shape action =
  let drop () =
    try
      while true do
        if fall pit falling falling_shape
        then raise Fallen
      done
    with Fallen -> ()
  in
    if action = 4
    then drop ()
    else begin
      let shape = !falling in
      let new_shape = Shape.move shape action in
        if not (collision pit new_shape)
        then falling :=  new_shape
    end

let calc_reward before after =
  let max_r, _ = before in
  let r_after, _ = after in
  let reward = (r_after - max_r) * 1 in
    if reward > 0
    then reward * 10
    else if reward = 0
    then 1
    else reward

let execute_action (pit, falling, falling_shape) action_list=
  (** queue up a list of move commands, return ??? *)
  let pit = ref pit in
  let falling = ref falling in
  let falling_shape = ref falling_shape in
  let before = get_tops pit in
    List.iter (move_shape pit falling falling_shape) action_list;
    let after = get_tops pit in
    let reward = calc_reward before after in
      ((!pit, !falling, !falling_shape), reward)

let evaluate pit pit' =
  (* copy the state and don't modify *)
  let max_r_before, tops_before = get_tops pit in
  let max_r, tops = get_tops pit' in
  let num_holes = 
    let holes = ref 0 in
      for c = 1 to cols-2 do
        for r = 0 to rows-1 do
          if r > tops.(c-1) && !pit'.(r).(c) = background
          then holes := !holes + 1
        done
      done;
      0 - !holes
  in
  let lines = max_r - max_r_before in
  let average_r =
    let sum = Array.fold_left (+) 0 tops in
      sum
  in
    10 * num_holes + 1000 * lines + average_r + 10 + max_r
