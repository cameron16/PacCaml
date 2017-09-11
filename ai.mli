open Types

(* [generate_enemy_command id state] is the mose optimal move, of type [command]
 * to be made by the enemy of id, [id] given a state, [state]
*)
val generate_enemy_command: int -> state -> command

(* [generate_enemy_command id state] is the mose optimal move, of type [command]
 * to be made by player 2 given a state, [state]
*)
val generate_p2_command: state -> command