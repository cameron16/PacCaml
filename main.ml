open Graphics
open Types
open Processor
open Ai
open Gui

let pre_game_state =  {
  start = true;
  pos = [];
  inaccessible_pos = [];
  point_pos = [];
  power_ups = [];
  player1_pos = (0,0);
  player2_pos = (0,0);
  enemy_positions = [];
  players = [];
  enemy_mode_p1 = Chase;
  enemy_mode_p2 = Chase
}

let rec loop_2 state =

  let change_to_command_p1 state command_string =
    match command_string with
      | 'w' -> Up
      | 's' -> Down
      | 'a' -> Left
      | 'd' -> Right
      | 'q' -> exit 0
      | _ -> loop_2 state
  in

  let change_to_command_p2 state command_string =
  match command_string with
    | 'i' -> Up
    | 'k' -> Down
    | 'j' -> Left
    | 'l' -> Right
    | 'q' -> exit 0
    | _ -> loop_2 state
  in

  (* Player 1 instructions *)
  let _ = Sys.command "clear" in
  print_endline "Enter Player 1 command: ";
  print_endline "";
  print_endline "Up    - w";
  print_endline "Down  - s";
  print_endline "Left  - a";
  print_endline "Right - d";
  print_endline "";
  print_endline "Quit  - q";
  let player1_status = wait_next_event [Key_pressed] in
  let player1_command_s = player1_status.key in
  let player1_command = change_to_command_p1 state player1_command_s in
  (* Player 2 instructions *)
  let _ = Sys.command "clear" in
  print_endline "Enter Player 2 command: ";
  print_endline "";
  print_endline "Up    - i";
  print_endline "Down  - k";
  print_endline "Left  - j";
  print_endline "Right - l";
  print_endline "";
  print_endline "Quit  - q";
  let player2_status = wait_next_event [Key_pressed] in
  let player2_command_s = player2_status.key in
  let player2_command = change_to_command_p2 state player2_command_s in
  let commands = [player1_command; player2_command] in
  let new_state = process_state state commands in
  update_gui new_state;
  loop_2 new_state

let rec loop_1 state =

  let change_to_command_p1 state command_string =
    match command_string with
      | 'w' -> Up
      | 's' -> Down
      | 'a' -> Left
      | 'd' -> Right
      | 'q' -> exit 0
      | _ -> loop_1 state
  in

  (* Player 1 instructions *)
  let _ = Sys.command "clear" in
  print_endline "Enter command: ";
  print_endline "";
  print_endline "Up    - w";
  print_endline "Down  - s";
  print_endline "Left  - a";
  print_endline "Right - d";
  print_endline "";
  print_endline "Quit  - q";
  let player1_status = wait_next_event [Key_pressed] in
  let player1_command_s = player1_status.key in
  let player1_command = change_to_command_p1 state player1_command_s in
  let player2_command = generate_p2_command state in
  let commands = [player1_command; player2_command] in
  let new_state = process_state state commands in
  update_gui new_state;
  loop_1 new_state

let rec play () =
  update_gui pre_game_state;
  let mode_status = wait_next_event [Key_pressed] in
  let mode_command = mode_status.key in
  if (mode_command = '1') then (
    update_gui init_state;
    loop_1 init_state
  )
  else if (mode_command = '2') then (
    update_gui init_state;
    loop_2 init_state
  )
  else if (mode_command = 'q') then (
    exit 0
  )
  else play ()
