open Filler
open Board

let string =
  {|🟪🟥🟦⬛🟪🟦🟥🟥                   
    🟦🟨🟩🟨🟥⬛🟦⬛
    🟥🟩🟦🟥⬛🟩⬛🟦
    🟪🟥🟩🟦🟪🟥🟩⬛
    🟩🟦🟪⬛🟨⬛🟨🟪
    🟪⬛🟦🟩🟥🟦🟪🟦
    ⬛🟨🟩🟥🟦🟥🟩🟨 |}

let board = parse string
let () = print board
let () = print_newline ()

(* let board' = move board Color.Red Opp
let () = print board'
let () = print_int (region_size board' Opp)
let () = print_newline () *)
(* let move, score = Minimax.minimax board ~player:Us ~max_depth:4
let () = Printf.printf "Best move: %s. Score: %f\n" (Color.to_square move) score *)

(* Simulating Moves*)
let print_state label board =
  let us_sz = region_size board Us in
  let opp_sz = region_size board Opp in
  Printf.printf "%s: Us=%d Opp=%d Difference=%d\n" label us_sz opp_sz
    (us_sz - opp_sz);
  print board;
  print_newline ()

let rec simulate board player remaining_depth =
  if remaining_depth = 0 then board
  else
    let chosen_move, predicted_score =
      Minimax.minimax board ~player ~max_depth:remaining_depth
    in
    let next_board = Board.move board chosen_move player in
    Printf.printf "Player %s chooses %s; predicted score: %f\n"
      (match player with Us -> "Us" | Opp -> "Opp")
      (Color.to_square chosen_move)
      predicted_score;
    print_state "After move" next_board;
    simulate next_board (other_player player) (remaining_depth - 1)

let final_board = simulate board Us 15
let () = print_state "Final State" final_board
