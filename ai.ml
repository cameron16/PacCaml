open Types

type node = {pos: int * int; distance: int; path: command list}

(* finds the element in a list of positions that matches the input position *)
let contains position_list position=
  List.exists (fun a-> a=position) position_list

(* returns the node that matches the target position in a list of nodes *)
let get_target node_list target_pos=
  List.find (fun a-> a.pos=target_pos) node_list

(* gets a list of border positions from a list of border nodes *)
let border_positions border_nodes=
  let rec helper nodes out_list=(
    match nodes with
    | [] -> out_list
    | h::t -> helper t (h.pos::out_list))
  in helper border_nodes []

(* takes a state and a location and checks in the inaccessible_pos list to see if
the new location is a valid location *)
let valid_move inaccessible_positions position =
  let condition = fun a -> if a = position then true else false in
  not (List.exists condition inaccessible_positions)

(* list of positions filtered out of those that are in inacessible *)
let filter_moves poss inaccessible_pos=
  List.filter (fun a-> valid_move inaccessible_pos a) poss

(* gets the available moves at a position *)
let available_moves state position=
  let x,y=match position with
     | x,y-> x,y
  in
  let moves=[(x,y+1);(x,y-1);(x-1,y);(x+1,y)] in
  filter_moves moves state.inaccessible_pos

(* gets the Command that would take an entity from start_pos to end_pos *)
let get_move end_pos start_pos=
  let ex, ey=match end_pos with | x,y->x,y in
  let sx, sy=match start_pos with | x,y->x,y in
  if ex=sx then
    if ey=sy+1 then Up else Down
  else if ex=sx+1 then Right else Left

(* returns a list of nodes that have the same positions attribute as the input
positions list, distances of the start node+1, and path of the start path
concatenated with the move that would get to the corresponding position *)
let rec wave positions start nw=
  match positions with
  | []-> nw
  | h::t -> let move=get_move h start.pos in
            let new_node={pos=h; distance=start.distance+1;path=start.path@[move]} in
            wave t start (new_node::nw)

(* gets a new list of nodes with updated attributes from a list of border_nodes,
without revisiting a position that has already been visited *)
let rec next_wave border state visited new_wave= match border with
  | [] -> new_wave,visited
  | h::t -> let p=available_moves state h.pos in
            (* possible moves that havent been visited *)
            let vp=filter_moves p visited in
            let new_w=wave vp h [] in
            next_wave t state (vp@visited) (new_w@new_wave)


(* returns the node in the path that gets to target in the least amount of
moves *)
let rec lee_search border_nodes state visited target=
  let border_node_poss=border_positions border_nodes in
  if contains border_node_poss target then
    get_target border_nodes target
  else
    let new_wave,new_visited= next_wave border_nodes state visited [] in
    lee_search new_wave state new_visited target

(* gets a node of the best path to a target position given a start position and
a state *)
let path start target state=
  let start_node = {pos=start; distance=0; path=[]} in
  lee_search (start_node::[]) state [] target

(* returns the best move to get to a target position *)
let best start target state=
  let best_path = path start target state
  in List.hd best_path.path

(* gets the move node for an enemy, depending on whether p1 is closer of p2 *)
let generate_enemy_path id state=
  let enemy_pos=List.nth state.enemy_positions id in
  let p1_pos = state.player1_pos in
  let p2_pos = state.player2_pos in
  let p1_move = path enemy_pos p1_pos state in
  let p2_move = path enemy_pos p2_pos state in
  if p1_move.distance < p2_move.distance then
    p1_move else p2_move

(* gets a list of commands that would reach the list of moves, given a
start position*)
let moves_from_positions start moves=
  List.map (fun a-> get_move a start ) moves

(* returns a random element from a list
gotten from http://stackoverflow.com/questions/5782932/how-can-i-randomly-select-an-element-in-ocaml *)
let randomelement in_list =
    let n = Random.int (List.length in_list) in
    List.nth in_list n;;

(* finds the closest player
if the player is in flee mode, return a move that is not the optimal move to get
to the player
if the player is in chase mode, return the move that is the optimal move to get
to the player *)
let generate_enemy_command id state =
  let enemy_pos=List.nth state.enemy_positions id in
  let p1_pos = state.player1_pos in
  let p2_pos = state.player2_pos in
  let p1_move = path enemy_pos p1_pos state in
  let p2_move = path enemy_pos p2_pos state in
  if p1_move.distance < p2_move.distance then
    let best_move=List.hd p1_move.path in
    if state.enemy_mode_p1=Chase then
      best_move
    else
      let enemy_pos=List.nth state.enemy_positions id in
      let move_pos=available_moves state enemy_pos in
      let all_moves=moves_from_positions enemy_pos move_pos in
      let flee_moves=List.filter (fun a-> a != best_move) all_moves in
      if List.length flee_moves = 0 then
        best_move
      else
        randomelement flee_moves
  else
    let best_move=List.hd p2_move.path in
    if state.enemy_mode_p2=Chase then
      best_move
    else
      let enemy_pos=List.nth state.enemy_positions id in
      let move_pos=available_moves state enemy_pos in
      let all_moves=moves_from_positions enemy_pos move_pos in
      let flee_moves=List.filter (fun a-> a != best_move) all_moves in
      if List.length flee_moves = 0 then
        best_move
      else
        randomelement flee_moves

(* gets the best move nodes for all the positions in pos_list given the start
position and state *)
let get_best_moves pos_list start state =
  let rec helper pos_list start out state=
    match pos_list with
    | [] -> out
    | h::t -> helper t start ((path start h state)::out) state
  in helper pos_list start [] state

(* given a list of moves, and the current closest move, return the move that has
the smallest distance *)
let rec closest_move move_list current=
    match move_list with
    | []-> current
    | h::t-> if (h.distance<=current.distance) then
                closest_move t h
              else closest_move t current

(* given a list of positions, find the closest move from a start position *)
let get_closest_move pos_list start state =
  let all_moves=get_best_moves pos_list start state in
  let placeholder={pos=(0,0); distance=1000000; path=[]} in
  closest_move all_moves placeholder

(* returns a random valid move for p2 *)
let random_move_p2 state =
  let p2_pos=state.player2_pos in
  let move_pos=available_moves state p2_pos in
  let all_moves=moves_from_positions p2_pos move_pos in
  randomelement all_moves

(* gets the positions from a list of powerups *)
let pos_from_power_ups powerup_list=
  List.map (fun a-> match a with | position,p_type -> position) powerup_list

(* if there are powerups, go to the closest one
if there are no powerups, go to the closest point *)
let generate_p2_command state=
  let p2_pos=state.player2_pos in
  let power_ups=state.power_ups in
  (* there are powerups, so go to them *)
  if List.length power_ups >0 then
    let power_up_poss= pos_from_power_ups power_ups in
    let power_up_move=get_closest_move power_up_poss p2_pos state in
    List.hd (power_up_move.path)
  (* no powerups, so go to the closest point *)
  else
    let point_poss=state.point_pos in
    let point_move=get_closest_move point_poss p2_pos state in
    List.hd (point_move.path)
