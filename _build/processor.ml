open Types
open Ai

(*--------------------------------*)
(*----------INITIALIZERS----------*)
(*--------------------------------*)

let double_dist = [Double]
let kill_enemy = [Strike]
let player1 = {points = 0; active_abilities = []; orientation =  Left}
let player2 = {points = 0; active_abilities = []; orientation =  Right}

let rec remove_inaccessable all_pos inaccessible_pos point_pos =
  match all_pos with
    | [] -> List.rev point_pos
    | h::t -> if (List.mem h inaccessible_pos) then
                        remove_inaccessable t inaccessible_pos point_pos
              else
                    remove_inaccessable t inaccessible_pos (h::point_pos)
let init_pos = [
(0,19);(1,19);(2,19);(3,19);(4,19);(5,19);(6,19);(7,19);(8,19);(9,19);(10,19);(11,19);(12,19);(13,19);(14,19);(15,19);(16,19);(17,19);(18,19);(19,19);
(0,18);(1,18);(2,18);(3,18);(4,18);(5,18);(6,18);(7,18);(8,18);(9,18);(10,18);(11,18);(12,18);(13,18);(14,18);(15,18);(16,18);(17,18);(18,18);(19,18);
(0,17);(1,17);(2,17);(3,17);(4,17);(5,17);(6,17);(7,17);(8,17);(9,17);(10,17);(11,17);(12,17);(13,17);(14,17);(15,17);(16,17);(17,17);(18,17);(19,17);
(0,16);(1,16);(2,16);(3,16);(4,16);(5,16);(6,16);(7,16);(8,16);(9,16);(10,16);(11,16);(12,16);(13,16);(14,16);(15,16);(16,16);(17,16);(18,16);(19,16);
(0,15);(1,15);(2,15);(3,15);(4,15);(5,15);(6,15);(7,15);(8,15);(9,15);(10,15);(11,15);(12,15);(13,15);(14,15);(15,15);(16,15);(17,15);(18,15);(19,15);
(0,14);(1,14);(2,14);(3,14);(4,14);(5,14);(6,14);(7,14);(8,14);(9,14);(10,14);(11,14);(12,14);(13,14);(14,14);(15,14);(16,14);(17,14);(18,14);(19,14);
(0,13);(1,13);(2,13);(3,13);(4,13);(5,13);(6,13);(7,13);(8,13);(9,13);(10,13);(11,13);(12,13);(13,13);(14,13);(15,13);(16,13);(17,13);(18,13);(19,13);
(0,12);(1,12);(2,12);(3,12);(4,12);(5,12);(6,12);(7,12);(8,12);(9,12);(10,12);(11,12);(12,12);(13,12);(14,12);(15,12);(16,12);(17,12);(18,12);(19,12);
(0,11);(1,11);(2,11);(3,11);(4,11);(5,11);(6,11);(7,11);(8,11);(9,11);(10,11);(11,11);(12,11);(13,11);(14,11);(15,11);(16,11);(17,11);(18,11);(19,11);
(0,10);(1,10);(2,10);(3,10);(4,10);(5,10);(6,10);(7,10);(8,10);(9,10);(10,10);(11,10);(12,10);(13,10);(14,10);(15,10);(16,10);(17,10);(18,10);(19,10);
 (0,9); (1,9); (2,9); (3,9); (4,9); (5,9); (6,9); (7,9); (8,9); (9,9); (10,9); (11,9); (12,9); (13,9); (14,9); (15,9); (16,9); (17,9); (18,9); (19,9);
 (0,8); (1,8); (2,8); (3,8); (4,8); (5,8); (6,8); (7,8); (8,8); (9,8); (10,8); (11,8); (12,8); (13,8); (14,8); (15,8); (16,8); (17,8); (18,8); (19,8);
 (0,7); (1,7); (2,7); (3,7); (4,7); (5,7); (6,7); (7,7); (8,7); (9,7); (10,7); (11,7); (12,7); (13,7); (14,7); (15,7); (16,7); (17,7); (18,7); (19,7);
 (0,6); (1,6); (2,6); (3,6); (4,6); (5,6); (6,6); (7,6); (8,6); (9,6); (10,6); (11,6); (12,6); (13,6); (14,6); (15,6); (16,6); (17,6); (18,6); (19,6);
 (0,5); (1,5); (2,5); (3,5); (4,5); (5,5); (6,5); (7,5); (8,5); (9,5); (10,5); (11,5); (12,5); (13,5); (14,5); (15,5); (16,5); (17,5); (18,5); (19,5);
 (0,4); (1,4); (2,4); (3,4); (4,4); (5,4); (6,4); (7,4); (8,4); (9,4); (10,4); (11,4); (12,4); (13,4); (14,4); (15,4); (16,4); (17,4); (18,4); (19,4);
 (0,3); (1,3); (2,3); (3,3); (4,3); (5,3); (6,3); (7,3); (8,3); (9,3); (10,3); (11,3); (12,3); (13,3); (14,3); (15,3); (16,3); (17,3); (18,3); (19,3);
 (0,2); (1,2); (2,2); (3,2); (4,2); (5,2); (6,2); (7,2); (8,2); (9,2); (10,2); (11,2); (12,2); (13,2); (14,2); (15,2); (16,2); (17,2); (18,2); (19,2);
 (0,1); (1,1); (2,1); (3,1); (4,1); (5,1); (6,1); (7,1); (8,1); (9,1); (10,1); (11,1); (12,1); (13,1); (14,1); (15,1); (16,1); (17,1); (18,1); (19,1);
 (0,0); (1,0); (2,0); (3,0); (4,0); (5,0); (6,0); (7,0); (8,0); (9,0); (10,0); (11,0); (12,0); (13,0); (14,0); (15,0); (16,0); (17,0); (18,0); (19,0)
]

let init_inaccessible_pos = [
(0,19);(1,19);(2,19);(3,19);(4,19);(5,19);(6,19);(7,19);(8,19);(9,19);(10,19);(11,19);(12,19);(13,19);(14,19);(15,19);(16,19);(17,19);(18,19); (19,19);
(0,18);                                   (6,18);(7,18);(8,18);               (11,18);(12,18);(13,18);                                         (19,18);
(0,17);       (2,17);(3,17);(4,17);                                                                           (15,17);(16,17);(17,17);         (19,17);
(0,16);       (2,16);                     (6,16);                                             (13,16);                        (17,16);         (19,16);
(0,15);       (2,15);       (4,15);(5,15);(6,15);(7,15);(8,15);               (11,15);(12,15);(13,15);(14,15);(15,15);        (17,15);         (19,15);
(0,14);                                                                                                                                        (19,14);
(0,13);(1,13);(2,13);(3,13);(4,13);                     (8,13);(9,13);(10,13);(11,13);                         (15,13);(16,13);(17,13);(18,13);(19,13);
(0,12);                     (4,12);(5,12);              (8,12);               (11,12);                 (14,12);(15,12);                        (19,12);
(0,11);(1,11);                                                                                                                         (18,11);(19,11);
(0,10);              (3,10);       (5,10);(6,10);                                             (13,10);(14,10);        (16,10);                 (19,10);
 (0,9);        (2,9); (3,9);               (6,9);               (9,9);(10,9);                  (13,9);                 (16,9); (17,9);         (19,9);
 (0,8);        (2,8);                                                                                                          (17,8);         (19,8);
 (0,7);                      (4,7); (5,7); (6,7); (7,7);                               (12,7); (13,7); (14,7); (15,7);                         (19,7);
 (0,6); (1,6); (2,6); (3,6); (4,6);               (7,6); (8,6);                (11,6); (12,6);                 (15,6); (16,6); (17,6); (18,6); (19,6);
 (0,5);                                                                                                                                        (19,5);
 (0,4);        (2,4);        (4,4); (5,4); (6,4); (7,4); (8,4);                (11,4); (12,4); (13,4); (14,4); (15,4);         (17,4);         (19,4);
 (0,3);        (2,3);                      (6,3);                                              (13,3);                         (17,3);         (19,3);
 (0,2);        (2,2); (3,2); (4,2);                                                                            (15,2); (16,2); (17,2);         (19,2);
 (0,1);                                           (7,1); (8,1);                (11,1); (12,1);                                                 (19,1);
 (0,0); (1,0); (2,0); (3,0); (4,0); (5,0); (6,0); (7,0); (8,0); (9,0); (10,0); (11,0); (12,0); (13,0); (14,0); (15,0); (16,0); (17,0); (18,0); (19,0)
      ]

let init_state = {
  start = false;
  pos = init_pos;

  inaccessible_pos = init_inaccessible_pos;

  point_pos =
    remove_inaccessable init_pos (
          [(1,18);(18,18);(1,1);(18,1);(9,10);(10,10);
           (9,1);(10,1);(9,18);(10,18)]
        @init_inaccessible_pos) [];
  power_ups = [((1,1), double_dist);
               ((18,1), kill_enemy);
               ((18,18), double_dist);
               ((1,18), kill_enemy)];
  player1_pos = (9,10);
  player2_pos = (10,10);
  enemy_positions = [(9,1); (10,1); (9,18); (10,18)];
  players = [player1; player2];
  enemy_mode_p1 = Chase;
  enemy_mode_p2 = Chase
}

let init_p1_pos = init_state.player1_pos
let init_p2_pos = init_state.player2_pos
let init_e0_pos = List.nth (init_state.enemy_positions) 0
let init_e1_pos = List.nth (init_state.enemy_positions) 1
let init_e2_pos = List.nth (init_state.enemy_positions) 2
let init_e3_pos = List.nth (init_state.enemy_positions) 3
let init_power_up_pos = [(1,1); (18,18); (18,1); (1,18)]

(*---------------------------*)
(*----------HELPERS----------*)
(*---------------------------*)

(* removes element [el] from list [lst].
   requires: [new_list] is empty
*)
let rec remove_from_list lst el new_list =
  match lst with
    | [] -> List.rev new_list
    | h::t -> if (h = el) then remove_from_list t el new_list else
              remove_from_list t el (h::new_list
)
(*----------------------------*)
(*----------PRINTING----------*)
(*----------------------------*)

let display_stats state=
  let p1_points = player1.points in
  let p2_points = player2.points in
  print_endline "Statistics: \n";
  print_endline ("Player 1:"^" "^(string_of_int p1_points)^"\n"
                ^"Player 2:"^" "^(string_of_int p2_points));
  print_endline ("");
  ()

(* creates a board of size, board is a two dimensional char array *)
let create_window size =
  let window = Array.make size (Array.make size ' ') in
  let rec remove_refs count =
    if count=size then window
    else let () = Array.set window count (Array.make size ' ') in
    remove_refs (count+1)
  in remove_refs 0

(* draws a position according to value for a board
   '*' is wall, '@' is player1, '$' is player2, '#' is enemy
 *)
let draw_position pos board value =
  match pos with
  | (x,y) ->
          let x_array =
          Array.get board ((Array.length board) - y-1) in
          Array.set x_array x value;
          board

(* draws the inaccessible locations for a board *)
let rec draw_borders board border_locs =
  match border_locs with
  | [] -> board
  | loc::t -> draw_borders (draw_position loc board '*') t

(* draws the point locations for a board *)
let rec draw_points board point_locs =
  match point_locs with
  | [] -> board
  | loc::t -> draw_points (draw_position loc board '-') t

(* draws the point locations for a board *)
let rec draw_power_ups board power_ups =
  match power_ups with
  | [] -> board
  | (pos, power_up)::t -> if (power_up = double_dist) then
                          draw_power_ups (draw_position pos board '>') t
                          else if (power_up = kill_enemy) then
                          draw_power_ups (draw_position pos board '}') t
                          else
                          failwith "No such power_up"

(* from stack overflow, make a string out of a list of chars *)
let cl2s cl = String.concat "" (List.map (String.make 1) cl)

(* creates a board according to a state *)
let draw_state state =
  let board = create_window 20 in
  (* draw the borders *)
  let border_board = draw_borders board state.inaccessible_pos in
  (* player 1 position *)
  let p1_board = draw_position (state.player1_pos) border_board '@' in
  (* player 2 position *)
  let p2_board = draw_position (state.player2_pos) p1_board '$' in
  (* player 2 position *)
  let points_board = draw_points p2_board state.point_pos in
  (* power_ups position *)
  let power_ups_board = draw_power_ups points_board state.power_ups in
  (* enemy 1 position *)
  let enemy1_pos = List.nth state.enemy_positions 0 in
  let e1_board = draw_position enemy1_pos power_ups_board '0' in
  (* enemy 2 position *)
  let enemy2_pos = List.nth state.enemy_positions 1 in
  let e2_board = draw_position enemy2_pos e1_board '1' in
  (* enemy 3 position *)
  let enemy3_pos = List.nth state.enemy_positions 2 in
  let e3_board = draw_position enemy3_pos e2_board '2' in
  (* enemy 4 position *)
  let enemy4_pos = List.nth state.enemy_positions 3 in
  let final_board = draw_position enemy4_pos e3_board '3' in
  let rec print_board lines=
    (
      match lines with
      | [] -> ()
      | h::t-> let stringn = cl2s (Array.to_list h) in
             print_endline stringn ; print_board t
    )
  in print_board (Array.to_list final_board)

(*----------------------------------*)
(*----------PLAYER UPDATES----------*)
(*----------------------------------*)

let rec append_no_dup lst1 lst2 =
  match lst1 with
    | [] -> lst2
    | h::t -> if (List.mem h lst2) then append_no_dup t lst2
              else append_no_dup t (h::lst2)

(* returns respawn_position of player 1 *)
let respawn_p1 state =
   if(
      (not (List.mem init_p1_pos state.enemy_positions)) &&
      (init_p1_pos <> state.player2_pos)
                               ) then
       init_p1_pos
   else if(
      (not (List.mem init_p2_pos state.enemy_positions)) &&
      (init_p2_pos <> state.player2_pos)
     ) then
       init_p2_pos
   else if(
      (not (List.mem init_e0_pos state.enemy_positions)) &&
      (init_e0_pos <> state.player2_pos)
     ) then
       init_e0_pos
    else if(
      (not (List.mem init_e1_pos state.enemy_positions)) &&
      (init_e1_pos <> state.player2_pos)
            ) then
       init_e1_pos
    else if(
      (not (List.mem init_e2_pos state.enemy_positions)) &&
      (init_e2_pos <> state.player2_pos)
            ) then
      init_e2_pos
    else
      init_e3_pos

(* returns respawn_position of player 2 *)
let respawn_p2 state =
   if(
      (not (List.mem init_p2_pos state.enemy_positions)) &&
      (init_p2_pos <> state.player1_pos)
                               ) then
       init_p2_pos
   else if(
      (not (List.mem init_p1_pos state.enemy_positions)) &&
      (init_p1_pos <> state.player1_pos)
     ) then
       init_p1_pos
   else if(
      (not (List.mem init_e0_pos state.enemy_positions)) &&
      (init_e0_pos <> state.player1_pos)
     ) then
       init_e0_pos
    else if(
      (not (List.mem init_e1_pos state.enemy_positions)) &&
      (init_e1_pos <> state.player1_pos)
            ) then
       init_e1_pos
    else if(
      (not (List.mem init_e2_pos state.enemy_positions)) &&
      (init_e2_pos <> state.player1_pos)
            ) then
      init_e2_pos
    else
      init_e3_pos


(* updates player1's position to a new position *)
let update_p1_pos state pos =
  (* update point_pos *)
  let new_point_pos =
    if (List.mem pos state.point_pos) then (
      if (List.mem Double player1.active_abilities) then (
        player1.points <- player1.points + 4;
        remove_from_list state.point_pos pos []
      )
      else (
        player1.points <- player1.points + 2;
        remove_from_list state.point_pos pos []
      )
    )
    else state.point_pos
  in
  (* update pickup_pos *)
  let new_power_ups =
    if (List.mem_assoc pos state.power_ups) then (
      let abilities_list = List.assoc pos state.power_ups in
      player1.active_abilities<-(append_no_dup abilities_list player1.active_abilities);
      List.remove_assoc pos state.power_ups
    )
    else state.power_ups
  in
  (* update enemy_mode_p1 *)
  let enemy_mode = if (List.mem Strike player1.active_abilities) then Flee
                  else Chase
  in
  (* update player_pos *)
  {
    start = state.start;
    pos = state.pos;
    inaccessible_pos = state.inaccessible_pos;
    point_pos = new_point_pos;
    power_ups = new_power_ups;
    player1_pos = pos;
    player2_pos = state.player2_pos;
    players = state.players;
    enemy_positions = state.enemy_positions;
    enemy_mode_p1 = enemy_mode;
    enemy_mode_p2 = state.enemy_mode_p2
  }

(* updates player2's position to a new position *)
let update_p2_pos state pos =
  (* update point_pos *)
  let new_point_pos =
    if (List.mem pos state.point_pos) then (
      if (List.mem Double player2.active_abilities) then (
        player2.points <- player2.points + 4;
        remove_from_list state.point_pos pos []
      )
      else (
        player2.points <- player2.points + 2;
        remove_from_list state.point_pos pos []
      )
    )
    else state.point_pos
  in
  (* update pickup_pos *)
  let new_power_ups =
    if (List.mem_assoc pos state.power_ups) then (
      let abilities_list = List.assoc pos state.power_ups in
      player2.active_abilities<-(append_no_dup abilities_list player2.active_abilities);
      List.remove_assoc pos state.power_ups
    )
    else state.power_ups
  in
  (* update enemy_mode_p2 *)
  let enemy_mode = if (List.mem Strike player2.active_abilities) then Flee
                  else Chase
  in
  (* update player_pos *)
  {
    start = state.start;
    pos = state.pos;
    inaccessible_pos = state.inaccessible_pos;
    point_pos = new_point_pos;
    power_ups = new_power_ups;
    player1_pos = state.player1_pos;
    player2_pos = pos;
    players = state.players;
    enemy_positions = state.enemy_positions;
    enemy_mode_p1 = state.enemy_mode_p1;
    enemy_mode_p2 = enemy_mode
  }

(* takes a state and a location and checks in the inaccessible_pos list to see if
the new location is a valid location *)
let valid_move inaccessible_positions position =
  let condition = fun a -> if a = position then true else false in
  not (List.exists condition inaccessible_positions)

(* moves player1 according to the command given *)
let move_p1 state command =
  let x,y = state.player1_pos in
  let nx,ny =
    match command with
    | Up -> (player1.orientation<-command);(x,y+1)
    | Down -> (player1.orientation<-command);(x,y-1)
    | Left -> (player1.orientation<-command);(x-1,y)
    | Right -> (player1.orientation<-command);(x+1,y)
  in
  let inaccessible_positions =
    state.inaccessible_pos in
  if valid_move inaccessible_positions (nx, ny) then
      update_p1_pos state (nx, ny)
  else state

(* moves player two according to the command *)
let move_p2 state command =
  let x,y = state.player2_pos in
 let nx,ny =
  match command with
    | Up -> (player2.orientation<-command);(x,y+1)
    | Down -> (player2.orientation<-command);(x,y-1)
    | Left -> (player2.orientation<-command);(x-1,y)
    | Right -> (player2.orientation<-command);(x+1,y)
 in
 let inaccessible_positions =
  state.inaccessible_pos in
 if valid_move inaccessible_positions (nx, ny) then
  update_p2_pos state (nx, ny)
 else state

(*---------------------------------*)
(*----------ENEMY UPDATES----------*)
(*---------------------------------*)

(*----------Single Enemy----------*)

(* returns the id of the enemy with the position [pos] given [enemy_positions]
   requires: id is 0
*)
let rec find_enemy_id enemy_positions pos id =
  match enemy_positions with
    | [] -> failwith "No such enemy position"
    | h::t -> if (h=pos) then id
              else
              find_enemy_id t pos (id+1)
(* returns the new enemy_positions after respawning the enemy with id, [id]
   given the current state, [state]
*)
let respawn_enemy state id =
  let position = List.nth state.enemy_positions id in
  let replace = fun x ->
                if (x = position) then(
                      if(
                          (not (List.mem init_e0_pos state.enemy_positions)) &&
                          (init_e0_pos <> state.player1_pos) &&
                          (init_e0_pos <> state.player2_pos)
                        ) then
                          init_e0_pos
                      else if(
                          (not (List.mem init_e1_pos state.enemy_positions)) &&
                          (init_e1_pos <> state.player1_pos) &&
                          (init_e1_pos <> state.player2_pos)
                             ) then
                          init_e1_pos
                      else if(
                          (not (List.mem init_e2_pos state.enemy_positions)) &&
                          (init_e2_pos <> state.player1_pos) &&
                          (init_e2_pos <> state.player2_pos)
                             ) then
                          init_e2_pos
                      else if(
                          (not (List.mem init_e3_pos state.enemy_positions)) &&
                          (init_e3_pos <> state.player1_pos) &&
                          (init_e3_pos <> state.player2_pos)
                             ) then
                          init_e3_pos
                      else if(
                          (not (List.mem init_p1_pos state.enemy_positions)) &&
                          (init_p1_pos <> state.player1_pos) &&
                          (init_p1_pos <> state.player2_pos)
                             ) then
                          init_p1_pos
                      else
                          init_p2_pos
                     )
                  else
                  x
  in
 List.map replace state.enemy_positions

(* updates enemy's position to a new position according to ID *)
let update_enemy_pos id state pos =
  let enemy_positions = state.enemy_positions in
  let enemy_positions_array = Array.of_list enemy_positions in
  let new_enemy_positions = Array.set enemy_positions_array id pos;
                            Array.to_list enemy_positions_array in
  {
    start = state.start;
    pos = state.pos;
    inaccessible_pos = state.inaccessible_pos;
    point_pos = state.point_pos;
    power_ups = state.power_ups;
    player1_pos = state.player1_pos;
    player2_pos = state.player2_pos;
    players = state.players;
    enemy_positions = new_enemy_positions;
    enemy_mode_p1 = state.enemy_mode_p1;
    enemy_mode_p2 = state.enemy_mode_p2
  }

(* move enemy according to the command *)
let move_enemy id state command =
  let x,y = List.nth state.enemy_positions id in
  let nx,ny =
    match command with
    | Up -> x,y+1
    | Down -> x,y-1
    | Left -> x-1,y
    | Right ->x+1,y
  in
  let other_enemy_positions = remove_from_list state.enemy_positions (x,y) [] in
  (* walls, other enemy positions and start positions of players are off limits *)
  let inaccessible_positions =
        (init_power_up_pos@state.inaccessible_pos@other_enemy_positions)
  in
  let new_state =
    if valid_move inaccessible_positions (nx, ny) then
      update_enemy_pos id state (nx, ny)
    else state
  in
  let new_enemy_position = List.nth new_state.enemy_positions id in
  (* Kill players *)
  if ( (new_state.player1_pos = new_enemy_position) &&
        (not (List.mem Strike player1.active_abilities))
     ) then (
    (* Kill player 1 *)
    let deduct = if (player1.points-20 > 0) then player1.points-20 else 0 in
    player1.points <- deduct;
    player1.orientation <- Left;
    player1.active_abilities <- [];
    {
    start = new_state.start;
    pos = new_state.pos;
    inaccessible_pos = new_state.inaccessible_pos;
    point_pos = new_state.point_pos;
    power_ups = new_state.power_ups;
    player1_pos = respawn_p1 new_state;
    player2_pos = new_state.player2_pos;
    players = new_state.players;
    enemy_positions = new_state.enemy_positions;
    enemy_mode_p1 = new_state.enemy_mode_p1;
    enemy_mode_p2 = new_state.enemy_mode_p2
    }
  )
  else if ( (new_state.player1_pos = new_enemy_position) &&
            ((List.mem Strike player1.active_abilities))
          ) then (
    (* Player 1 Kills *)
    let enemy_id =
          find_enemy_id new_state.enemy_positions new_state.player1_pos 0 in
    {
      start = new_state.start;
      pos = new_state.pos;
      inaccessible_pos = new_state.inaccessible_pos;
      point_pos = new_state.point_pos;
      power_ups = new_state.power_ups;
      player1_pos = new_state.player1_pos;
      player2_pos = new_state.player2_pos;
      players = new_state.players;
      enemy_positions = respawn_enemy new_state enemy_id;
      enemy_mode_p1 = new_state.enemy_mode_p1;
      enemy_mode_p2 = new_state.enemy_mode_p2
    }
  )
  else if ( (new_state.player2_pos = new_enemy_position) &&
            ((List.mem Strike player2.active_abilities))
          ) then (
    (* Player 2 Kills *)
    let enemy_id =
          find_enemy_id new_state.enemy_positions new_state.player2_pos 0 in
    {
      start = new_state.start;
      pos = new_state.pos;
      inaccessible_pos = new_state.inaccessible_pos;
      point_pos = new_state.point_pos;
      power_ups = new_state.power_ups;
      player1_pos = new_state.player1_pos;
      player2_pos = new_state.player2_pos;
      players = new_state.players;
      enemy_positions = respawn_enemy new_state enemy_id;
      enemy_mode_p1 = new_state.enemy_mode_p1;
      enemy_mode_p2 = new_state.enemy_mode_p2
    }
  )
  else if ( (new_state.player2_pos = new_enemy_position) &&
            (not (List.mem Strike player2.active_abilities))
          ) then (
    (* Kill player 2 *)
    let deduct = if (player2.points-20 > 0) then player2.points-20 else 0 in
    player2.points <- deduct;
    player2.orientation <- Right;
    player2.active_abilities <- [];
    {
    start = new_state.start;
    pos = new_state.pos;
    inaccessible_pos = new_state.inaccessible_pos;
    point_pos = new_state.point_pos;
    power_ups = new_state.power_ups;
    player1_pos = new_state.player1_pos;
    player2_pos = respawn_p2 new_state;
    players = new_state.players;
    enemy_positions = new_state.enemy_positions;
    enemy_mode_p1 = new_state.enemy_mode_p1;
    enemy_mode_p2 = new_state.enemy_mode_p2
    }
  )
  else
    new_state

(*----------All Enemies----------*)

let rec process_ind_move state number_of_enemies =
  if (number_of_enemies = 0) then state else
  let enemy_id = number_of_enemies - 1 in
  let enemy_command = generate_enemy_command enemy_id state in
  let new_state = move_enemy enemy_id state enemy_command in
  process_ind_move new_state (number_of_enemies - 1)

let move_enemies state =
  let enemy_positions = state.enemy_positions in
  let number_of_enemies = List.length enemy_positions in
  process_ind_move state number_of_enemies

(*----------------------------------*)
(*----------UPDATING STATE----------*)
(*----------------------------------*)

(* takes a state, a command, and a player to update *)
let move state command player=
  match player with
  | 1 -> move_p1 state command
  | 2 -> move_p2 state command
  |_ -> failwith "not a valid player"

(* state is the current state of the game, commands is a list of two commands
index 0 is player 1, index 1 is player 2 *)
let process_state state commands =
  let p1_points = player1.points in
  let p2_points = player2.points in
  let p1_abilities = player1.active_abilities in
  let p2_abilities = player2.active_abilities in
  match commands with
  | first::second::_ ->
    let state1 = move state first 1 in
    let state2 = move state1 second 2 in
    (* if player 1 and 2 move to the same position don't move either *)
    let state3 =
      if (state2.player1_pos = state2.player2_pos) then (
        player1.points <- p1_points;
        player2.points <- p2_points;
        player1.active_abilities <- p1_abilities;
        player2.active_abilities <- p2_abilities;
        state
      )
      else
        state2 in
    (* player 1 encounters enemies, if applicable *)
    let state4 =
      if( (List.mem Strike player1.active_abilities) &&
          (List.mem state3.player1_pos state3.enemy_positions)
        )
      then (
        let enemy_id =
          find_enemy_id state3.enemy_positions state3.player1_pos 0 in
          {
            start = state3.start;
            pos = state3.pos;
            inaccessible_pos = state3.inaccessible_pos;
            point_pos = state3.point_pos;
            power_ups = state3.power_ups;
            player1_pos = state3.player1_pos;
            player2_pos = state3.player2_pos;
            players = state3.players;
            enemy_positions = respawn_enemy state3 enemy_id;
            enemy_mode_p1 = state3.enemy_mode_p1;
            enemy_mode_p2 = state3.enemy_mode_p2
          }
        )
      else if (List.mem state3.player1_pos state3.enemy_positions)
      then(
          let deduct = if (player1.points-20 > 0) then player1.points-20 else 0 in
          player1.points <- deduct;
          player1.orientation <- Left;
          player1.active_abilities <- [];
          {
            start = state3.start;
            pos = state3.pos;
            inaccessible_pos = state3.inaccessible_pos;
            point_pos = state3.point_pos;
            power_ups = state3.power_ups;
            player1_pos = respawn_p1 state3;
            player2_pos = state3.player2_pos;
            players = state3.players;
            enemy_positions = state3.enemy_positions;
            enemy_mode_p1 = state3.enemy_mode_p1;
            enemy_mode_p2 = state3.enemy_mode_p2
          }
        )
      else state3 in
    (* player 2 encounters enemies, if applicable *)
    let state5 =
      if( (List.mem Strike player2.active_abilities) &&
          (List.mem state4.player2_pos state3.enemy_positions)
        )
      then (
        let enemy_id =
          find_enemy_id state4.enemy_positions state4.player2_pos 0 in
          {
            start = state4.start;
            pos = state4.pos;
            inaccessible_pos = state4.inaccessible_pos;
            point_pos = state4.point_pos;
            power_ups = state4.power_ups;
            player1_pos = state4.player1_pos;
            player2_pos = state4.player2_pos;
            players = state4.players;
            enemy_positions = respawn_enemy state4 enemy_id;
            enemy_mode_p1 = state4.enemy_mode_p1;
            enemy_mode_p2 = state4.enemy_mode_p2
          }
        )
      else if (List.mem state4.player2_pos state4.enemy_positions)
      then(
          let deduct = if (player2.points-20 > 0) then player2.points-20 else 0 in
          player2.points <- deduct;
          player2.orientation <- Right;
          player2.active_abilities <- [];
          {
            start = state4.start;
            pos = state4.pos;
            inaccessible_pos = state4.inaccessible_pos;
            point_pos = state4.point_pos;
            power_ups = state4.power_ups;
            player1_pos = state4.player1_pos;
            player2_pos = respawn_p2 state4;
            players = state4.players;
            enemy_positions = state4.enemy_positions;
            enemy_mode_p1 = state4.enemy_mode_p1;
            enemy_mode_p2 = state4.enemy_mode_p2
          }
        )
      else state4 in
    let final_state = move_enemies state5 in
    final_state
  |_ -> failwith "not a valid command list"

let process_draw_state state commands =
  let new_state = process_state state commands in
  draw_state new_state;
  display_stats state;
  new_state
