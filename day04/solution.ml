let word = "XMAS"
let word_len = String.length word

exception Break of string

type direction = Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft

let find_word arr direction x y =
  match direction with
  | Up -> String.init word_len @@ fun n -> arr.(x - n).[y]
  | UpRight -> String.init word_len @@ fun n -> arr.(x - n).[y + n]
  | Right -> String.init word_len @@ fun n -> arr.(x).[y + n]
  | DownRight -> String.init word_len @@ fun n -> arr.(x + n).[y + n]
  | Down -> String.init word_len @@ fun n -> arr.(x + n).[y]
  | DownLeft -> String.init word_len @@ fun n -> arr.(x + n).[y - n]
  | Left -> String.init word_len @@ fun n -> arr.(x).[y - n]
  | UpLeft -> String.init word_len @@ fun n -> arr.(x - n).[y - n]

let pp_found direction w =
  if w = word then begin
      Printf.printf "Found(%s). " direction;
      true
    end
  else false

let find_xmas input =
  let arr = Array.of_list input in
  let rows = Array.length arr in
  let cols = arr.(0) |> String.length in
  let total = ref 0 in
  (* let rows = 1 in *)
  for row = 0 to rows - 1 do
    let str = arr.(row) in
    let len = String.length str in

    Printf.printf "(%03d). %s -> " row str;
    for col = 0 to cols - 1 do
      (* print_int (j - word_len); print_string " . "; *)
      if col + (word_len - 1) < len then
        let find = find_word arr Right row col in
        if pp_found "H[Right]" find then total := !total + 1
        else ()
      else ();

      (* Search Horizontally [Left] *)
      if col - (word_len - 1) >= 0 then
        let find = find_word arr Left row col in
        if pp_found "H[Left]" find then total := !total + 1
        else ()
      else ();
      
      if row + (word_len - 1) < rows then
        (* Search vertically (Down)  *)
        let find = find_word arr Down row col in
        if pp_found "V[Down]" find then total := !total + 1
        else ();

        (* Search Diagonally [DownRight] *)
        if col + (word_len - 1) < len then
          let find =find_word arr DownRight row col in
          if pp_found "V[DownRight]" find then total := !total + 1
          else ()
        else ();

        (* Search Diagonally [DownLeft] *)
        if col - (word_len - 1) >= 0 then
          let find = find_word arr DownLeft row col in
          if pp_found "V[DownLeft]" find then total := !total + 1
          else ()
        else ();
      else ();

      (* Search vertically (Up)  *)
      if row - (word_len - 1) >= 0 then
        let find = find_word arr Up row col in
        if pp_found "V[Up]" find then total := !total + 1
        else ();

        (* Search Diagonally [UpRight] *)
        if col + (word_len - 1) < len then
          let find = find_word arr UpRight row col in
          if pp_found "V[UpRight]" find then total := !total + 1
          else ()
        else ();
        
        (* Search Diagonally [UpLeft] *)
        if col - (word_len - 1) >= 0 then begin
            (* print_int j;  *)
            let find = find_word arr UpLeft row col in
            if pp_found "V[UpLeft]" find then total := !total + 1
            else ()
          end
        else ();
      else ();
    done;
    print_newline ();
  done;
  Printf.printf "Total XMAS: %d\n" !total

let find_x_mas input =
  let arr = Array.of_list input in
  let rows = Array.length arr in
  let cols = String.length arr.(0) in
  for row = 0 to rows do
    ()
    (* TODO *)
  done;
  ()

let () =
  let input = In_channel.with_open_text "input.txt" In_channel.input_lines in
  find_xmas input;
    
