open Printf

let pause = ref false
let app_done = ref false

let pause_unpause tick redraw () =
  match !pause with
      true ->
        ignore(GMain.Timeout.add 500 tick);
        ignore(GMain.Timeout.add 100 redraw);
        pause := false
    | false ->
        pause := true

let locale =
  if not !Sys.interactive
  then GtkMain.Main.init ()
  else ""

let width = Config.width
let height = Config.height
let tile_width = width / Pit.cols
let tile_height = height / Pit.rows

let clicked msg () =
  print_endline msg;
  flush stdout

let destroy () = GMain.Main.quit ()

let delete_event ev =
  destroy ();
  true

let signals = ref []

let add_signal name id =
  signals := (name, id) :: !signals

let translate col row =
  let x = row * Config.height / Config.rows in
  let y = col * Config.width / Config.cols in
    (x, y)

let fill_square (drawing : GDraw.drawable) row col color =
  let x, y = translate row col in
    drawing#set_foreground (`NAME color);
    drawing#rectangle ~filled:true ~x:x ~y:y
      ~width:tile_width ~height:tile_height ()

let draw_tetromino (drawing : GDraw.drawable) (shape : Shape.tetromino) =
  let shape, direction, (col, row), color = !State.falling in
  let shape = shape.(direction) in
  let draw_tetromino i b =
    let row = row + (i/4) in
    let col = col + (i mod 4) in
      if b = 1
      then fill_square drawing row col Config.tetromino_colors.(color)
  in
    Array.mapi draw_tetromino shape

let keypress key =
  let direction =
    match GdkEvent.Key.keyval key with
        65362 -> (* up *)
          0
      | 65364 -> (* down *)
          1
      | 65361 -> (* left *)
          2
      | 65363 -> (* right *)
          3
      | 65505 -> (* shift/drop *)
          4
      | _ -> -1 (* not interesting *)
  in
    if direction > -1
    then Pit.move_shape State.pit State.falling State.shape direction;
    false

let redraw (drawing : GDraw.drawable) =
  (* setup drawing area *)
  drawing#set_foreground (`NAME Config.background);
  drawing#rectangle ~filled:true ~x:0 ~y:0 ~width:width ~height:height ();
  drawing#set_foreground (`NAME Config.background);
  drawing#rectangle ~filled:false ~x:0 ~y:0 ~width:width ~height:height ();
  drawing#set_foreground (`NAME Config.foreground);

  (* draw the pit *)
  let pit = !State.pit in
  let draw_loc row col data =
    if data != Pit.background
    then fill_square drawing row col data
  in
  let draw_row row cols =
    Array.iteri (draw_loc row) cols
  in
    Array.iteri draw_row pit;

    (* draw the falling piece *)
    ignore(draw_tetromino drawing !State.falling);
    true

let quit () =
  app_done := true

let main tick_callback =
  let window = GWindow.window ~border_width:10 () in
    add_signal "WinDelete" (window#event#connect#delete ~callback:delete_event);
    add_signal "WinDestroy" (window#connect#destroy ~callback:destroy);
    let bigbox = GPack.vbox ~packing:window#add () in
    let box1 = GPack.hbox ~packing:bigbox#add () in
    let box2 = GPack.hbox ~packing:bigbox#add () in
    let image_area = GMisc.drawing_area
                       ~width: width
                       ~height: height
                       ~packing:box1#add
                       ()
    in
    let drawing1 =
      image_area#misc#realize ();
      new GDraw.drawable (image_area#misc#window)
    in
    let current_drawing = ref drawing1 in
    let button = GButton.button ~label:"Pause" ~packing:box2#pack () in
    let button2 = GButton.button ~label:"Quit" ~packing:box2#pack () in
    let expose_event _ =
      redraw !current_drawing
    in
    let tick () =
      tick_callback ();
      ignore(redraw !current_drawing);
      (* set current_drawing to image area *)
      (* set image area drawing to current drawing *)
      (not !pause)
    in
    let idle () =
      ignore(redraw !current_drawing);
      (not !pause)
    in
      add_signal "draw" (image_area#event#connect#expose
                           ~callback:expose_event);
      add_signal "Button1" (button#connect#clicked
                              ~callback:(pause_unpause tick idle));
      add_signal "Button2" (button2#connect#clicked ~callback:quit);
      add_signal "keypress" (window#event#connect#key_press ~callback:keypress);
      window#show ();
      ignore(GMain.Timeout.add 500 tick);
      (*ignore(GMain.Idle.add idle); no double buffering, this is bad*)
      ignore(GMain.Timeout.add 100 idle);
      GtkMain.Main.main ()
