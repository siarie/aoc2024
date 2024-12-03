let calculate_diffs lst =
  let rec fn lst' acc =
    match lst' with
    | [] | [_] -> acc
    | hd::tl ->
       let diff = hd - List.hd tl in
       fn tl (acc @ [diff] )
  in
  fn lst []

let is_safe lst =
  let diffs = calculate_diffs lst in
  let is_monotonic =
    List.for_all (fun i -> i > 0) diffs || List.for_all (fun d -> d < 0) diffs
  in
  is_monotonic && List.for_all (fun a -> Int.abs a > 0 && Int.abs a <= 3) diffs

let is_safe2 lst =
  let rec check lst' prev_lst idx =
    match lst' with
    | [] -> false
    | hd::tl ->
       if is_safe (prev_lst @ tl) then true
       else
         let tolerated_list = prev_lst @ tl in
         if is_safe tolerated_list then true
         else
           if idx = List.length tolerated_list then false
           else check tl (prev_lst @ [hd]) (idx+1)
  in
  if is_safe lst then true
  else check lst [] 0

let parse lst =
  let rec fn ls acc =
    match ls with
    | [] -> acc
    | hd::tl ->
       let nums = String.split_on_char ' ' hd in
       let nums_map = List.map (fun n -> int_of_string n) nums in
       fn tl (nums_map :: acc)
  in
  fn lst []

let () =
  let _input = In_channel.with_open_text "day02/input.txt" In_channel.input_lines in
  let _parsed = parse _input in
  let result = ref 0 in
  let result2 = ref 0 in
  for i = List.length _parsed - 1 downto 0 do
    let add = if is_safe (List.nth _parsed i) then 1 else 0 in
    result := !result + add;

    let add2 = if is_safe2 (List.nth _parsed i) then 1 else 0 in
    result2 := !result2 + add2;
  done;
  Printf.printf "Total safe: %d\n" !result;
  Printf.printf "Total safe (part 2): %d\n" !result2;
