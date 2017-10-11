open Printf
open Pit

let debug = ref false
let current_state = ref 0
let learning = ref false
let interactive = ref true
let game_number = ref 1
let game_count = ref 1000
let game_reward = ref 0
let before_pit = ref (Pit.copy_pit !State.pit)

let loss (appin, appout) =
  fprintf stdout "Game %i lines: %i reward: %i\n%!" !game_number !Pit.line_count !game_reward;
  game_reward := 0;
  game_number := !game_number + 1;
  State.reset_pit ();
  if !game_number > !game_count
  then begin
    if Config.gui
    then Gui.app_done := true
    else begin
      fprintf appout "goodbye!\n";
      Unix.sleep 1; (* allow slave to clean up *)
      exit 0;
    end
  end;
  if !learning
  then fprintf appout "-------- new trial ----------\n";
  true

let execute_cycle (appin, appout) () =
  if !Gui.app_done
  then begin
    if !interactive || (Config.gui && not !learning)
    then GMain.Main.quit ()
  end;
  try
    if !learning
    then begin
    (* -------- learning control ----------- *)
    let ttl = Pit.top_two_lines State.pit State.shape in
        current_state := ttl;
        let actions = Shape.possible_actions !State.falling in
          fprintf appout "state: %i\n" ttl;
          fprintf appout "%i actions:" (List.length actions);
          List.iter (fun (i, _) -> fprintf appout " %i" i) actions;
          fprintf appout "\n";
          flush appout;
          let r = input_line appin in
          let response = int_of_string r in
            if response > (List.length actions)
            then fprintf stderr "We selected a bad action\n%!";
            let action_list = List.assoc response actions in
            let new_state, reward =
              Pit.execute_action (State.get_state ()) action_list in
              State.set_state new_state;
              game_reward := !game_reward + reward;
              fprintf appout "reward: %i\n" reward;
              if !Gui.app_done
              then begin
                if not !interactive
                then fprintf appout "goodbye!\n";
                GMain.Main.quit ()
              end;
              flush appout;
              true
    end
    else if not !interactive
    then begin
      (* -------- static evaluator control ----------- *)
      let get_valuation (pit, falling, fallen) (idx, a) =
        let pit = Pit.copy_pit pit in
        let (pit', _, _), r = Pit.execute_action (pit, falling, fallen) a in
          let value = Pit.evaluate (ref pit) (ref pit') in
            (idx, value + r)
      in
      let actions = Shape.possible_actions !State.falling in
      let eval = Pit.evaluate State.pit in
      let state = State.get_state () in
      let valuations = List.map (get_valuation state) actions in
      let eval = Pit.evaluate State.pit in
      let valuations = List.fast_sort Util.tuple_rev_cmp valuations in
      let idx, value = List.hd valuations in
      let action_list = List.assoc idx actions in
      let new_state, _ =
        Pit.execute_action (State.get_state ()) action_list
      in
        State.set_state new_state;
        true
    end
    else
      (* -------- user control ----------- *)
      let rest = Pit.fall State.pit State.falling State.shape in
      if rest
      then
        let eval = Pit.evaluate before_pit State.pit in
          before_pit := Pit.copy_pit !State.pit;
          fprintf stderr "eval: %i\n%!" eval;
          flush stderr;
          true
          else
            true
  with Util.Loss ->
    loss (appin, appout)

let loop channels =
  while (execute_cycle channels ()) do
    ()
  done

let _ =
  Random.init 5;
  Pit.new_falling State.falling State.shape;
  let appin, appout =
    if Array.length Sys.argv > 1
    then begin
      if Sys.argv.(1) = "static"
      then begin
        interactive := false;
        learning := false;
        game_count := int_of_string Sys.argv.(2);
        (stdin, stdout)
      end else begin
        interactive := false;
        learning := true;
        let app = Sys.argv.(1) in
        let alg = Sys.argv.(2) in
        game_count := int_of_string Sys.argv.(3);
        Unix.open_process ("ocamlrun -b " ^ app ^ " " ^ alg ^ " 0.1");
      end
    end
    else (stdin, stdout)
  in
    if not !Sys.interactive
    then
      begin
        if !learning
        then begin
          let states = 40000 in
          fprintf appout "initialize: %i states, 40 actions, max reward 4.00\n"
            states;
          fprintf appout "-------- new trial ----------\n";
          flush appout;
          if Config.gui
          then Gui.main (execute_cycle (appin, appout))
          else loop (appin, appout)
        end
        else if not !learning && not !interactive
        then begin
          if Config.gui
          then Gui.main (execute_cycle (appin, appout))
          else loop (appin, appout)
        end else begin
          Gui.main (execute_cycle (appin, appout))
        end
      end
