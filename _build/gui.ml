open Graphics
open Types

let window_game_size = 800
let coordinate_size = 20
let () = open_graph " 1000x800"

let stats_size_x = 200
let stats_size_y = window_game_size

let stats_x_pos = 850

let p1_stats_pos_y = 600 (*620*)
let score_1_pos_y = 580 (*600*)

let p2_stats_pos_y = 420
let score_2_pos_y = 400

let p1_abilities_pos_y = ref score_1_pos_y

let p2_abilities_pos_y = ref score_2_pos_y

let enemy_number = ref 0

let () =Graphics.set_window_title "PacCaml"

let hot_pink = 16722885
let pink = 16630538

let gameOver = false

type prev_pacman = {mutable x_pos: int; mutable y_pos: int}
let prev_pacman1 = {x_pos=9; y_pos=10}
let prev_pacman2 = {x_pos=10; y_pos=10}

(*x0 and y0 are bottom left corners of rectangle*)
type pixel_rect = {x_left_corner: int; y_left_corner: int; x_center: int; y_center: int; w: int; h: int}


(*given the pixel_rect for a wall, draw that wall*)
let draw_wall (wall_rect:pixel_rect) : unit =
	let () = Graphics.set_color Graphics.blue in
	Graphics.fill_rect wall_rect.x_left_corner wall_rect.y_left_corner
	wall_rect.w wall_rect.h

let draw_point_circle (point_rect: pixel_rect) : unit=
	let radius = (point_rect.w/2)-(point_rect.w/4) in
	let () = Graphics.set_color white in
	Graphics.fill_circle point_rect.x_center point_rect.y_center radius

let draw_strike_key (radius: int) (x_center: int) (y_center : int)  : unit =
(* 	let () = Graphics.moveto 805 640 in
	let () = Graphics.draw_string "Strike:" in *)
	Graphics.set_color red;
	Graphics.fill_circle x_center y_center radius;
	Graphics.set_color black;
	Graphics.set_line_width 2;
	Graphics.moveto (x_center-2*(radius/3)) (y_center-(radius/3));
	Graphics.lineto (x_center+(radius/3)) (y_center+2*(radius/3));
	Graphics.moveto (x_center-(radius/3)) (y_center-2*(radius/3));
	Graphics.lineto (x_center+2*(radius/3)) (y_center+(radius/3));
	Graphics.set_line_width 1;
	Graphics.moveto (x_center-(radius/2)) (y_center+(radius/2));
	Graphics.lineto (x_center+(radius/2)) (y_center-(radius/2));
	()

let draw_double_key (radius: int) (x_center: int) (y_center : int) : unit =
(*
	Graphics.moveto 905 640;
	Graphics.draw_string "Double:"; *)
	Graphics.set_color red;
	Graphics.fill_circle x_center y_center radius;
	Graphics.set_color black;
	Graphics.set_line_width 2;
	Graphics.moveto (x_center - (radius/2)) (y_center - (radius/2));
	Graphics.lineto (x_center) (y_center);
	Graphics.lineto (x_center + (radius/2)) (y_center - (radius/2));

	Graphics.moveto (x_center - (radius/2)) (y_center - (radius/2) + 6);
	Graphics.lineto (x_center) (y_center + 6);
	Graphics.lineto (x_center + (radius/2)) (y_center - (radius/2) + 6);
	()


let draw_power_up (powerup_rect: pixel_rect) (power_ability:power_up) : unit=
	let radius = (powerup_rect.w/2)-(powerup_rect.w/4) in
	draw_strike_key radius 860 645;
	draw_double_key radius 960 645;
	Graphics.set_color red;
	Graphics.fill_circle powerup_rect.x_center powerup_rect.y_center radius;
	let head_power = List.nth power_ability 0 in
	match head_power with
	|Double -> draw_double_key radius powerup_rect.x_center powerup_rect.y_center
	|Strike -> draw_strike_key radius powerup_rect.x_center powerup_rect.y_center
	
let createChaseEnemy (enemy_rect : pixel_rect) : unit =
	let float_w = float_of_int enemy_rect.w in
	let xLeft = enemy_rect.x_left_corner + int_of_float (float_w *. (3.0/.8.0)) - 3 in (*-3*)
	let yLeft = enemy_rect.y_left_corner + (enemy_rect.h/4) - 5 in (*-5*)
	let width = (enemy_rect.w/4) +6 in (*+6*)
	let height = (enemy_rect.h/2) +6 in
	let () = Graphics.set_color black in
	let radius = (enemy_rect.w/2) - (enemy_rect.w/4) in
	let () = Graphics.fill_circle enemy_rect.x_center enemy_rect.y_center radius in
	let fourth_width =int_of_float (float_of_int (width) /. 4.0) in
	let right_eye_x = xLeft + fourth_width in
	let left_eye_x = xLeft + 3*fourth_width in
	let float_height = float_of_int height in
	let three_fourth_height = int_of_float (float_height *. (3.0/.4.0)) in
	let eye_y = yLeft + three_fourth_height in
	if (!enemy_number=1) then
		let () = Graphics.set_color green in
		let () = Graphics.fill_rect xLeft yLeft width height in
		let () = Graphics.set_color white in
		let () = Graphics.fill_circle right_eye_x eye_y 2 in
		let () = Graphics.fill_circle left_eye_x eye_y 2 in
		()
	else if (!enemy_number=2) then
		let () = Graphics.set_color magenta in
		let () = Graphics.fill_rect xLeft yLeft width height in
		let () = Graphics.set_color white in
		let () = Graphics.fill_circle right_eye_x eye_y 2 in
		let () = Graphics.fill_circle left_eye_x eye_y 2 in
		()

	else if (!enemy_number=3) then
		let () = Graphics.set_color cyan in
		let () = Graphics.fill_rect xLeft yLeft width height in
		let () = Graphics.set_color white in
		let () = Graphics.fill_circle right_eye_x eye_y 2 in
		let () = Graphics.fill_circle left_eye_x eye_y 2 in
		()
	else
		let () = Graphics.set_color red in
		let () = Graphics.fill_rect xLeft yLeft width height in
		let () = Graphics.set_color white in
		let () = Graphics.fill_circle right_eye_x eye_y 2 in
		let () = Graphics.fill_circle left_eye_x eye_y 2 in
		()

let createFleeEnemy (enemy_rect : pixel_rect) : unit =
	let float_w = float_of_int enemy_rect.w in
	let xLeft = enemy_rect.x_left_corner + int_of_float (float_w *. (3.0/.8.0)) -3 in (*-3*)
	let yLeft = enemy_rect.y_left_corner + (enemy_rect.h/4) -5 in (*-5*)
	let width = (enemy_rect.w/4) +6 in (*+6*)
	let height = (enemy_rect.h/2) +6 in (*was -10, now +6*)
	let () = Graphics.set_color black in
	let radius = (enemy_rect.w/2) - (enemy_rect.w/4) in
	let () = Graphics.fill_circle enemy_rect.x_center enemy_rect.y_center radius in
	let () = Graphics.set_color blue in
	let () = Graphics.fill_rect xLeft yLeft width height in
	let fourth_width =int_of_float (float_of_int (width) /. 4.0) in
	let right_eye_x = xLeft + fourth_width in
	let left_eye_x = xLeft + 3*fourth_width in
	let float_height = float_of_int height in
	let three_fourth_height = int_of_float (float_height *. (3.0/.4.0)) in
	let eye_y = yLeft + three_fourth_height in
	let () = Graphics.set_color white in
	let () = Graphics.fill_circle right_eye_x eye_y 3 in
	let () = Graphics.fill_circle left_eye_x eye_y 3 in
	let fourth_height = int_of_float (float_height /. 4.0) in
	let () = moveto left_eye_x (fourth_height+yLeft) in
	let () = Graphics.lineto right_eye_x (fourth_height+yLeft) in
	()

(*input is a coordinate point like (0,0) or (5,4) from processor.ml state
output is mapping of that coordinate point to a
rectangle in the gaming window which is of size
window_game_size x window_game_size
output is of type pixel_rect*)
let get_pixel_rect (coordinate_point: (int*int)) : pixel_rect =
	let sizing = window_game_size / coordinate_size in
	let x_center_rect = (Pervasives.fst coordinate_point * sizing) + (sizing/2) in
	let y_center_rect = (Pervasives.snd coordinate_point * sizing) + (sizing/2) in
	let lower_left_x = x_center_rect - (sizing / 2) in
	let lower_left_y = y_center_rect - (sizing / 2) in
	let width = sizing in
	let height = sizing in

	(* let () = Graphics.set_color black in
	let () = Graphics.fill_rect lower_left_x lower_left_y width height in *)

	{x_left_corner=lower_left_x; y_left_corner =lower_left_y;
	x_center = x_center_rect; y_center = y_center_rect; w = width; h = height}

(*given the inaccessible_positions, this function draws
walls on the maze*)
let rec updateWalls (wall_positions:(int*int) list) : unit =
	match wall_positions with
	|[] -> ()
	|h::t -> let wall_rect = get_pixel_rect h in
			let () = draw_wall wall_rect in
			updateWalls t

let rec updatePointPos (point_positions: (int*int) list) : unit =
	match point_positions with
	|[] -> ()
	|h::t -> let point_rect = get_pixel_rect h in
			let () = draw_point_circle point_rect in
			updatePointPos t

let rec updatePowerUp (powerup_positions: ((int * int) * power_up) list) : unit =
	match powerup_positions with
	|[] -> ()
	|h::t -> let powerup_rect = get_pixel_rect (Pervasives.fst h) in
			let () = draw_power_up powerup_rect (Pervasives.snd h) in
			updatePowerUp t

let clearScreen : unit =
	set_color Graphics.black;
	Graphics.fill_rect 0 0 window_game_size window_game_size;
	set_color Graphics.green;
	fill_rect window_game_size 0 stats_size_x stats_size_y;
	()

let drawPacManRight (pacRect : pixel_rect) (radius : int) : unit =
	let () = Graphics.fill_arc pacRect.x_center pacRect.y_center radius radius 45 315 in
	let eye_y_coord = pacRect.y_center + (radius/3) + 2 in
	let () = set_color black in
	let eye_radius = 2 in
	Graphics.fill_circle pacRect.x_center eye_y_coord eye_radius

	(* let () = moveto (pacRect.x_center-5) (pacRect.y_center-5) in
	let () = Graphics.draw_string "1" in ()
 *)

let drawPacManLeft (pacRect : pixel_rect) (radius : int) : unit =
	let () = Graphics.fill_arc pacRect.x_center pacRect.y_center radius radius 225 (135+360) in
	let eye_y_coord = pacRect.y_center + (radius/3) + 2 in
	let () = set_color black in
	let eye_radius = 2 in
	Graphics.fill_circle pacRect.x_center eye_y_coord eye_radius

let drawPacManUp (pacRect : pixel_rect) (radius : int) : unit =
	let () = Graphics.fill_arc pacRect.x_center pacRect.y_center radius radius 135 (45+360) in
	let eye_x_coord = pacRect.x_center + (radius/3) + 2 in
	let () = set_color black in
	let eye_radius = 2 in
	Graphics.fill_circle eye_x_coord pacRect.y_center eye_radius

let drawPacManDown (pacRect : pixel_rect) (radius : int) : unit =
	let () = Graphics.fill_arc pacRect.x_center pacRect.y_center radius radius 315 (225+360) in
	let eye_x_coord = pacRect.x_center + (radius/3) + 2 in
	let () = set_color black in
	let eye_radius = 2 in
	Graphics.fill_circle eye_x_coord pacRect.y_center eye_radius
(* 
  let coverWhiteDot (position: (int*int)) : unit = 
	let dotRect = get_pixel_rect position in
	Graphics.set_color black;
	Graphics.fill_rect dotRect.x_left_corner dotRect.y_left_corner dotRect.w dotRect.h;
	()

let coverPrevPacman (position : (int*int)) : unit =
	let pacRect = get_pixel_rect position in
	Graphics.set_color red;
	Graphics.fill_rect pacRect.x_left_corner pacRect.y_left_corner pacRect.w pacRect.h;
	()  *) 

let updatePacMan1 (position: (int*int)) (player: player list) : unit =
	let pacRect = get_pixel_rect position in
	let radius = (pacRect.w/2)-(pacRect.w/4)+5 in
	let player1 = List.nth player 0 in
	let () = Graphics.set_color yellow in
(* 
	let () = coverPrevPacman (prev_pacman1.x_pos, prev_pacman1.y_pos) in
	let () = coverWhiteDot (pacRect.x_center, pacRect.y_center) in 
	let () = prev_pacman1.x_pos <- pacRect.x_center in 
	let () = prev_pacman1.y_pos <- pacRect.y_center in

	let () = Graphics.set_color yellow in  
 *)
	match player1.orientation with
	|Up -> drawPacManUp pacRect radius
	|Down -> drawPacManDown pacRect radius
	|Left -> drawPacManLeft pacRect radius
	|Right -> drawPacManRight pacRect radius

let updatePacMan2 (position: (int*int)) (player: player list) : unit =
	let pacRect = get_pixel_rect position in
	let radius = (pacRect.w/2)-(pacRect.w/4) +5 in
	let player2 = List.nth player 1 in
	let () =Graphics.set_color hot_pink in
	match player2.orientation with
	|Up -> drawPacManUp pacRect radius
	|Down -> drawPacManDown pacRect radius
	|Left -> drawPacManLeft pacRect radius
	|Right -> drawPacManRight pacRect radius

let updateScore (my_state: state) : unit  =
	let p1 = List.nth my_state.players 0 in
	let p2 = List.nth my_state.players 1 in
	let p1_score = p1.points in
	let p2_score = p2.points in

	Graphics.set_color green;
	Graphics.fill_rect stats_x_pos score_1_pos_y 80 10;
	Graphics.fill_rect stats_x_pos score_2_pos_y 80 10;

	Graphics.set_color black;
	Graphics.moveto stats_x_pos score_1_pos_y;
	Graphics.draw_string ("Score 1: "^string_of_int p1_score);
	Graphics.moveto stats_x_pos score_2_pos_y;
	Graphics.draw_string ("Score 2: "^string_of_int p2_score);
	()

let rec helpUpdatePowerUpsP1 (abilities: ability list) (my_state:state) : unit =
	match abilities with
	|[] -> p1_abilities_pos_y := score_1_pos_y; ()
	|h::t ->
		match h with
		|Double -> p1_abilities_pos_y:= (!p1_abilities_pos_y)-20; Graphics.moveto stats_x_pos (!p1_abilities_pos_y);
								let () = Graphics.draw_string "Double" in
								let powerup_rect = get_pixel_rect my_state.player1_pos in
								let radius = (powerup_rect.w/2)-(powerup_rect.w/4) in
								let () = draw_double_key radius (stats_x_pos+55) ((!p1_abilities_pos_y)+5) in
								helpUpdatePowerUpsP1 t my_state
		|Strike -> p1_abilities_pos_y:= (!p1_abilities_pos_y)-20; Graphics.moveto stats_x_pos (!p1_abilities_pos_y);
								let () = Graphics.draw_string "Strike" in
								let powerup_rect = get_pixel_rect my_state.player1_pos in
								let radius = (powerup_rect.w/2)-(powerup_rect.w/4) in
								let () = draw_strike_key radius (stats_x_pos+55) ((!p1_abilities_pos_y)+5) in
								helpUpdatePowerUpsP1 t my_state

let rec helpUpdatePowerUpsP2 (abilities: ability list) (my_state:state) : unit =
	match abilities with
	|[] -> p2_abilities_pos_y := score_2_pos_y; ()
	|h::t ->
		match h with
		|Double -> p2_abilities_pos_y:= (!p2_abilities_pos_y)-20; Graphics.moveto stats_x_pos (!p2_abilities_pos_y);
								let () = Graphics.draw_string "Double" in
								let powerup_rect = get_pixel_rect my_state.player1_pos in
								let radius = (powerup_rect.w/2)-(powerup_rect.w/4) in
								let () = draw_double_key radius (stats_x_pos+55) ((!p2_abilities_pos_y)+5) in
								helpUpdatePowerUpsP2 t my_state
		|Strike -> p2_abilities_pos_y:= (!p2_abilities_pos_y)-20; Graphics.moveto stats_x_pos (!p2_abilities_pos_y);
								let () = Graphics.draw_string "Strike" in
								let powerup_rect = get_pixel_rect my_state.player1_pos in
								let radius = (powerup_rect.w/2)-(powerup_rect.w/4) in
								let () = draw_strike_key radius (stats_x_pos+55) ((!p2_abilities_pos_y)+5) in
								helpUpdatePowerUpsP2 t my_state


let updatePowerUpsP1 (my_state: state) : unit =
	let () = Graphics.set_color green in
	let () = Graphics.fill_rect stats_x_pos (p2_stats_pos_y+15) 80 ((score_1_pos_y-15)-(p2_stats_pos_y+15)+10) in


	let p1 = List.nth my_state.players 0 in
	let p1_abilities = p1.active_abilities in
	let () = Graphics.set_color black in
	let () = Graphics.moveto stats_x_pos (score_1_pos_y-15) in
	helpUpdatePowerUpsP1 p1_abilities my_state

let updatePowerUpsP2 (my_state: state) : unit =
	let () = Graphics.set_color green in
	let () = Graphics.fill_rect stats_x_pos (55) 80 ((score_2_pos_y-15)-5-40) in (*15 was the y starting*)

	let p2 = List.nth my_state.players 1 in
	let p2_abilities = p2.active_abilities in
	let () = Graphics.set_color black in
	let () = Graphics.moveto stats_x_pos (score_2_pos_y-15) in
	helpUpdatePowerUpsP2 p2_abilities my_state

let displayStats : unit =
	let () = set_text_size 20 in
	let () = Graphics.set_color black in
	let () = Graphics.moveto stats_x_pos p1_stats_pos_y in
	let () = Graphics.draw_string "Player 1" in
	let () = Graphics.moveto stats_x_pos p2_stats_pos_y in
	Graphics.draw_string "Player 2"

let rec enemyChaseState enemy_pos : unit =
	match enemy_pos with
	|[] -> enemy_number := 0; ()
	|h::t -> enemy_number := (!enemy_number) + 1;
		let enemy_rect = get_pixel_rect h in
		let () = createChaseEnemy enemy_rect in
		enemyChaseState t

let rec enemyFleeState enemy_pos : unit =
	match enemy_pos with
	|[] ->  ()
	|h::t -> let enemy_rect = get_pixel_rect h in
			let () = createFleeEnemy enemy_rect in
			enemyFleeState t

let updateEnemy (my_state : state) : unit =
	match (my_state.enemy_mode_p1, my_state.enemy_mode_p2) with
	|(Chase, _) -> enemyChaseState my_state.enemy_positions
	|(_, Chase) -> enemyChaseState my_state.enemy_positions
	|_ -> enemyFleeState my_state.enemy_positions

let rec doubleClearScreen all_positions : unit =

	match all_positions with
	|[] -> ()
	|h::t -> let clear_rect = get_pixel_rect h in
	let () = Graphics.set_color black in
	Graphics.fill_rect clear_rect.x_left_corner clear_rect.y_left_corner clear_rect.w clear_rect.h;
	doubleClearScreen t




let add_quit_text : unit =
	Graphics.moveto 850 2;
	Graphics.draw_string "Press 'q' to quit";
	()

let add_directions : unit =
	Graphics.moveto 805 780;
	Graphics.draw_string "Players must alternate commands";
	Graphics.moveto 805 760;
	Graphics.draw_string "Player 1 goes first";
	Graphics.moveto 805 740;
	Graphics.draw_string "P1 Controls:";
	Graphics.moveto 805 720;
	Graphics.draw_string "Up: w";
	Graphics.moveto 805 700;
	Graphics.draw_string "Down: a";
	Graphics.moveto 805 680;
	Graphics.draw_string "Left: s";
	Graphics.moveto 805 660;
	Graphics.draw_string "Right: d";
	Graphics.moveto 905 740;
	Graphics.draw_string "P2 Controls:";
	Graphics.moveto 905 720;
	Graphics.draw_string "Up: i";
	Graphics.moveto 905 700;
	Graphics.draw_string "Down: k";
	Graphics.moveto 905 680;
	Graphics.draw_string "Left: j";
	Graphics.moveto 905 660;
	Graphics.draw_string "Right: l";

	Graphics.moveto 805 640;
	Graphics.draw_string "Strike:";
	Graphics.moveto 905 640;
	Graphics.draw_string "Double:";

	Graphics.moveto 850 40;
	Graphics.draw_string "If you are stuck,";
	Graphics.moveto 850 25;
	Graphics.draw_string "refer to the console";
	()

let endGameState (my_state:state) : unit =
	let () = doubleClearScreen my_state.pos in
	let p1 = List.nth my_state.players 0 in
	let p2 = List.nth my_state.players 1 in
	Graphics.set_color black;
	Graphics.fill_rect 0 0 window_game_size window_game_size;
	Graphics.set_color white;
	Graphics.moveto 100 150;
	Graphics.draw_string "Arjan Singh";
	Graphics.moveto 100 130;
	Graphics.draw_string "Alex Pomerenk";
	Graphics.moveto 100 110;
	Graphics.draw_string "Cameron Boroumand";
	Graphics.moveto 350 700;
	Graphics.draw_string "Thanks for playing PacCaml!";
	Graphics.moveto 400 400;
	if p1.points > p2.points then
		Graphics.draw_string "Player 1 Wins"
	else if p1.points<p2.points then
		Graphics.draw_string "Player 2 Wins"
	else
		Graphics.draw_string "It's a tie"

let preGameState (my_state : state) : unit =
	let () = doubleClearScreen my_state.pos in
	Graphics.set_color black;
	Graphics.fill_rect 0 0 (window_game_size) window_game_size;
	Graphics.set_color white;
	Graphics.moveto 400 650;
	Graphics.draw_string "Welcome to PacCaml, yound lad";
	Graphics.moveto 400 500;
	Graphics.draw_string "Press 1 for 1-Player Mode";
	Graphics.moveto 400 450;
	Graphics.draw_string "Press 2 for 2-Player Mode";
	()

let update_gui (my_state:state) : unit =
	if (my_state.start = true) then
		let () = preGameState my_state in
		()
	else
		if (List.length my_state.point_pos <> 0) then
			let () = doubleClearScreen my_state.pos in

			let () = updateWalls my_state.inaccessible_pos in
			let () = updatePointPos my_state.point_pos in
			let () = updatePowerUp my_state.power_ups in
			let () = updatePacMan1 my_state.player1_pos my_state.players in
			let () = updatePacMan2 my_state.player2_pos my_state.players in
			let () = displayStats in
			let () = updateScore my_state in
			let () = updatePowerUpsP1 my_state in
			let () = updatePowerUpsP2 my_state in
			let () = updateEnemy my_state in
			()
		else
			let () = endGameState my_state in
			()

