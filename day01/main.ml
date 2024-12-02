let split s =
  let parts = String.split_on_char ' ' s in
  let filter = List.filter (fun s -> s <> "") parts in
  match filter with
  | [x;y] -> (int_of_string x, int_of_string y)
  | _ -> failwith "Error: input"

let parse_location lst =
  let rec fn lst left right =
    match lst with
    | [] -> (left,right)
    | h::t ->
       let (a,b) = split h in
       fn t (a::left) (b::right)
  in
  fn lst [] []

let calculate_distance left right =
  let sorted_left = List.sort (fun a b -> a - b) left in
  let sorted_right = List.sort (fun a b -> a - b) right in
  let result = ref 0 in
  for i = 0 to List.length sorted_left - 1 do
    let distance = (List.nth sorted_left i) - (List.nth sorted_right i) in
    result := !result + Int.abs distance
  done;
  result

let () =
  let input = In_channel.with_open_text "day01/input.txt" In_channel.input_lines in
  let (left,right) = parse_location input in
  let distance = calculate_distance left right in
  print_endline (string_of_int !distance)
