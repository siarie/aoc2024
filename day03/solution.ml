let is_digit c =
  let code = Char.code c in
  code >= Char.code('0') && code <= Char.code('9')

let parse_mul str =
  let len = String.length str in
  let rec parse_int start =
    let rec aux i =
      if i >= len || not (str.[i] |> is_digit) then
        if i = start then None
        else Some (String.sub str start (i - start), i)
      else
        aux (i + 1)
    in
    aux start

  and parse_mul_expr start =
    let idx = start + 4 in 
    if idx > len || String.sub str start 4 <> "mul("
    then None
    else
      let arg1_start = start + 4 in
      match parse_int arg1_start with
      | Some (arg1, cidx) when cidx < len && str.[cidx] = ',' ->
         let r = match parse_int (cidx + 1) with
           | Some (arg2, close_idx) when close_idx < len && str.[close_idx] = ')' ->
              Some ((int_of_string arg1, int_of_string arg2), close_idx + 1)
           | _ -> None
         in r
      | _ -> None

  and aux i acc =
    if i >= len then acc
    else
      match parse_mul_expr i with
      | Some ((x,y), next_index) ->
         aux next_index (acc + (x * y))
      | None ->
         aux (i + 1) acc
  in
  aux 0 0


let parse_mul2 str =
  let len = String.length str in
  let state = ref "do" in
  let rec parse_int start =
    let rec aux i =
      if i >= len || not (str.[i] |> is_digit) then
        if i = start then None
        else Some (String.sub str start (i - start), i)
      else
        aux (i + 1)
    in
    aux start

  and parse_mul_expr start =
    let idx = start + 4 in 
    if idx >= len || String.sub str start 4 <> "mul(" then None
    else
      let arg1_start = start + 4 in
      match parse_int arg1_start with
      | Some (arg1, comma_idx) when comma_idx < len && str.[comma_idx] = ',' ->
         let r =
           match parse_int (comma_idx + 1) with
           | Some (arg2, close_idx) when close_idx < len && str.[close_idx] = ')' ->
              Some ((int_of_string arg1, int_of_string arg2), close_idx + 1)
           | _ -> None
         in r
      | _ -> None

  and parse_do_expr start =
    if (start + 4) < len && String.sub str start 4 = "do()" then (Some ("do", start + 4))
    else if (start + 7) < len && String.sub str start 7 = "don't()" then (Some ("dont", start + 7))
    else None

  and aux i acc =
    if i >= len then acc
    else
      match parse_mul_expr i with
      | Some ((x,y), next_index) ->
         if !state = "do" then aux next_index (x * y + acc)
         else aux (i + 1) acc;
      | None ->
         match parse_do_expr i with
         | Some (st, next_index) -> state := st; aux next_index acc
         | None -> aux (i + 1) acc
  in
  aux 0 0

let () =
  let input = In_channel.with_open_text "input.txt" In_channel.input_all in
  let r1 = parse_mul input in
  let r2 = parse_mul2 input in
  Printf.printf "Part 1: %d\n" r1;
  Printf.printf "Part 2: %d\n" r2;
  
