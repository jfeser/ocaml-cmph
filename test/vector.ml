open Printf
open Cmph

let () =
  let keys = [
    "aaaaaaaaaa"; "bbbbbbbbbb"; "cccccccccc"; "dddddddddd"; "eeeeeeeeee"; 
    "ffffffffff"; "gggggggggg"; "hhhhhhhhhh"; "iiiiiiiiii"; "jjjjjjjjjj"
  ] in
  let keyset = KeySet.of_list keys in
  let config = Config.create keyset in
  let hash = Hash.create config in
  List.iter (fun k -> eprintf "key:%s -- hash:%d\n" k (Hash.hash hash k)) keys
