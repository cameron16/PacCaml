(* Defines the possible commands any player or enemy can execute
*)
type command =
  |Up
  |Down
  |Left
  |Right

(* Defines the abilities a player can gain, according to the following
 * specification:
 *      Double       : The player covers two tiles in one move, in the commanded
                       direction
 *      Sheild       : The player can survive ONE collision with an enemy
 *      Invincible   : The player can survive ALL collisions with enemies for
 *                     a set number of turns
 *      Strike       : The player can "kill" the enemy
*)
type ability =
  |Double
  |Strike

type power_up = ability list

(* Defines a players state
*)
type player = {
  mutable points: int;
  mutable active_abilities: ability list;
  mutable orientation: command;
}

(* Defines the behaviour of an enemy, according to the following
 * specification:
 *      Chase        : Follow the player in order to "kill" her
 *      Flee         : Run from the player as she tries to "kill" you
*)
type enemy_mode =
  |Chase
  |Flee

(* Defines the status of the maze.
 * Note: [start] signifies whether the game has begun or not. [true] means the player(s)
 * are in the menu
*)
type state = {
  start: bool;
  pos: (int * int) list;
  inaccessible_pos: (int * int) list;
  point_pos: (int * int) list;
  power_ups: ((int * int) * power_up) list;
  player1_pos: int * int;
  player2_pos: int * int;
  players: player list;
  enemy_positions: (int * int) list;
  enemy_mode_p1: enemy_mode;
  enemy_mode_p2: enemy_mode
}