open Printf

exception Loss

let rec make_list value size =
  if size = 0
  then []
  else value :: make_list value (size-1)

let number_list lst =
  let rec number_list n lst =
    match lst with
        [] -> []
      | hd :: tl ->
          (n, hd) :: number_list (n+1) tl
  in
    number_list 0 lst

let print_bits b =
  Array.iter (fun x -> fprintf stderr "%i " x) b;
  fprintf stderr "\n%!"

let sprint_action_list lst =
  let string_of_action a =
    match a with
        0 -> "flip"
      | 1 -> "down"
      | 2 -> "left"
      | 3 -> "right"
      | 4 -> "drop"
      | _ -> "unknown"
  in
  let rec print_list l =
    match l with
        [] -> "\n"
      | hd :: tl ->
          sprintf "%s " (string_of_action hd) ^ (print_list tl)
  in
  let header = sprintf "%i actions:\n" (List.length lst) in
    List.fold_left (fun x (_, y) -> x ^ print_list y) header lst

let tuple_cmp (x0, v0) (x1, v1) =
  if v0 < v1
  then -1
  else
    if v0 > v1
    then 1
    else 0

let tuple_rev_cmp x0 x1 =
  0 - (tuple_cmp x0 x1)

let cmp v0 v1 =
  if v0 < v1
  then -1
  else
    if v0 > v1
    then 1
    else 0

let print_inttuple_list l =
  List.iter (fun (x,y) -> fprintf stderr "(%i, %i) " x y);
  fprintf stderr "\n%!"
