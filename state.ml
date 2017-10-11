let falling = ref (snd (Shape.new_tetromino 0))
let shape = ref 0
let pit = ref (Pit.make_pit ())

let get_state () =
  (Pit.copy_pit !pit, !falling, !shape)

let new_state () =
  (Pit.make_pit (), snd (Shape.new_tetromino 0), 0)

let set_state (new_pit, new_falling, new_shape) =
  pit := new_pit;
  falling := new_falling;
  shape := new_shape

let reset_pit () =
  pit := Pit.make_pit ();
  Pit.line_count := 0

