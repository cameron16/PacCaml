type ability =
  |Double
  |Strike

type power_up = ability list

type enemy_mode =
  |Chase
  |Flee

type command =
  |Up
  |Down
  |Left
  |Right

type player = {
  mutable points: int;
  mutable active_abilities: ability list;
  mutable orientation: command
}

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