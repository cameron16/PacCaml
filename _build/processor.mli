open Types

(* [init_state] is the [state] in effect at the start of a game *)
val init_state: state

(* [process_state old_state p1_com p2_com] is a [state] resulting from moves
 * made by the player(s) and the AI
*)
val process_state: state -> command list -> state

(* [draw_state] prints the state of the board in ASCII notation
   given the state, [state]
*)
val draw_state: state -> unit

(* [process_draw_state] prints the state of the board in ASCII notation
   after processing state, [state]
*)
val process_draw_state: state -> command list -> state