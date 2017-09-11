(* Represents the possible commands the player could give.
 * Note: All these commands are valid at all times, but that does not mean
 * that the moves resulting from these commands are valid at all times
*)
(* type command =
  |Up
  |Down
  |Left
  |Right
 *)
(* [gen_command1 input] is the command corresponding to the button pressed by
 * player 1 (W, A, S or D) according to the following specification:
 *      W: Up
 *      S: Down
 *      A: Left
 *      D: Right
*)
(* val gen_command1: command *)

(* [gen_command1 input] is the command corresponding to the button pressed by
 * player 2 (arrow keys) according to the following specification:
 *      up arrow    : Up
 *      down arrow  : Down
 *      left arrow  : Left
 *      right arrow : Right
*)
(* val gen_command2: command *)