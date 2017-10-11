open Printf 

type timed_func = float * float * (unit -> bool)
type data = (int * timed_func) list
type timer = int * data

let empty_timer () : timer =
  (0, [])

let add_timer (last_id, data) freq func =
  let id = last_id in
  let new_func : timed_func = (freq, Unix.gettimeofday (), func) in
  let data = (id, new_func) :: data in
  let last_id = succ last_id in
  let timer = (last_id, data) in
    (timer, id)

let mod_timer (last_id, data) id freq func =
  let _, last, _ = List.assoc id data in
  let data = (id, (freq, last, func)) :: (List.remove_assoc id data) in
    (last_id, data)

let del_timer (last_id, data) id =
  (last_id, List.remove_assoc id data)

let is_time (id, (quantum, last, func)) =
  let time = Unix.gettimeofday () in
  let truth = time -. quantum >= last in
    match truth with
        true ->
          ignore(func ());
          (id, (quantum, time, func))
      | false -> (id, (quantum, last, func))

let run_timer timer =
  let data = ref (snd !timer) in
    while true do
      data := List.map is_time !data
    done
