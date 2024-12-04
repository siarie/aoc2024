let word = "XMAS"
let word_len = String.length word

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

let is_xmas direction w =
  if w = word then begin
      (* Printf.printf "Found(%s). " direction; *)
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

    for col = 0 to cols - 1 do
      if col + (word_len - 1) < len then
        let find = find_word arr Right row col in
        if is_xmas "H[Right]" find then total := !total + 1
        else ()
      else ();

      (* Search Horizontally [Left] *)
      if col - (word_len - 1) >= 0 then
        let find = find_word arr Left row col in
        if is_xmas "H[Left]" find then total := !total + 1
        else ()
      else ();
      
      if row + (word_len - 1) < rows then
        (* Search vertically (Down)  *)
        let find = find_word arr Down row col in
        if is_xmas "V[Down]" find then total := !total + 1
        else ();

        (* Search Diagonally [DownRight] *)
        if col + (word_len - 1) < len then
          let find =find_word arr DownRight row col in
          if is_xmas "V[DownRight]" find then total := !total + 1
          else ()
        else ();

        (* Search Diagonally [DownLeft] *)
        if col - (word_len - 1) >= 0 then
          let find = find_word arr DownLeft row col in
          if is_xmas "V[DownLeft]" find then total := !total + 1
          else ()
        else ();
      else ();

      (* Search vertically (Up)  *)
      if row - (word_len - 1) >= 0 then
        let find = find_word arr Up row col in
        if is_xmas "V[Up]" find then total := !total + 1
        else ();

        (* Search Diagonally [UpRight] *)
        if col + (word_len - 1) < len then
          let find = find_word arr UpRight row col in
          if is_xmas "V[UpRight]" find then total := !total + 1
          else ()
        else ();
        
        (* Search Diagonally [UpLeft] *)
        if col - (word_len - 1) >= 0 then begin
            let find = find_word arr UpLeft row col in
            if is_xmas "V[UpLeft]" find then total := !total + 1
            else ()
          end
        else ();
      else ();
    done;
  done;
  Printf.printf "Total XMAS: %d\n" !total

let is_x_mas w = w = "MAS" || w = "SAM"

let find_x_mas input =
  let word_len = 3 in
  let arr = Array.of_list input in
  let rows = Array.length arr in
  let cols = String.length arr.(0) in
  let total = ref 0 in 
  for row = 1 to rows - 2 do
    for col = 1 to cols - 2 do
      let bslash = String.init word_len @@ fun n -> arr.(row - 1 + n).[col - 1 + n] in
      let slash = String.init word_len @@ fun n -> arr.(row - 1 + n).[col + 1 - n] in
      if is_x_mas bslash && is_x_mas slash then total := !total + 1
      else ()
    done;
  done;
  Printf.printf "Total X-MAS: %d\n" !total

let () =
  let input = In_channel.with_open_text "input.txt" In_channel.input_lines in
  find_xmas input;
  find_x_mas input;
    
