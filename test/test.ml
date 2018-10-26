open OUnit2
open Cmph
open Util

let seeds = [0; 1; 2; 3; 4; 5]

let algos =
  Config.[`Bmz; `Bmz8; default_chd; default_chd_ph; `Fch; `Bdz; `Bdz_ph]

let check_result ctxt out =
  let ret, cmph_output = out in
  match ret with
  | Ok (kv_config, kv_packed) -> assert_equal ~ctxt kv_config kv_packed
  | Error (Cmph.Error (`Hash_new_failed msg)) ->
      logf ctxt `Warning "Cmph internal error: %s" msg
  | Error e ->
      let msg = cmph_output ^ "\n\n" ^ Printexc.to_string e in
      assert_failure msg

let packed_strings_test algo seed fn =
  let name = Printf.sprintf "%s:%d:%s" (Config.string_of_algo algo) seed fn in
  name
  >:: fun ctxt ->
  let ch = open_in fn in
  let rec read lines =
    try
      let line = input_line ch in
      read (line :: lines)
    with End_of_file -> List.rev lines
  in
  let keys = read [] in
  with_output (fun () ->
      let keyset = KeySet.of_cstrings keys in
      let config = Config.create ~seed:0 ~verbose:true ~algo keyset in
      let chash = Hash.of_config config in
      let kv_config = List.map (Hash.hash chash) keys in
      let phash = Hash.to_packed chash |> Hash.of_packed in
      let kv_packed = List.map (Hash.hash phash) keys in
      (kv_config, kv_packed) )
  |> check_result ctxt

let packed_fw_test algo seed fn =
  let name = Printf.sprintf "%s:%d:%s" (Config.string_of_algo algo) seed fn in
  name
  >:: fun ctxt ->
  let ch = open_in fn in
  let rec read keys =
    try
      let key = really_input_string ch 8 in
      read (key :: keys)
    with End_of_file -> List.rev keys
  in
  let keys = read [] in
  with_output (fun () ->
      let keyset = KeySet.of_fixed_width keys in
      let config = Config.create ~seed:0 ~verbose:true ~algo keyset in
      let chash = Hash.of_config config in
      let kv_config = List.map (Hash.hash chash) keys in
      let phash = Hash.to_packed chash |> Hash.of_packed in
      let kv_packed = List.map (Hash.hash phash) keys in
      (kv_config, kv_packed) )
  |> check_result ctxt

let product_3 l1 l2 l3 =
  List.map
    (fun x1 ->
      List.map (fun x2 -> List.map (fun x3 -> (x1, x2, x3)) l3) l2
      |> List.concat )
    l1
  |> List.concat

let suite =
  "tests"
  >::: [ ( "packed-strings"
         >:::
         (* Disable Bmz8 algorithm because it only works for key sets with < 256
            keys. *)
         let algos =
           Config.[`Bmz; default_chd; default_chd_ph; `Fch; `Bdz; `Bdz_ph]
         in
         product_3 algos seeds ["keys-long.txt"]
         |> List.map (fun (algo, seed, fn) -> packed_strings_test algo seed fn)
         )
       ; ( "packed-fixedwidth"
         >:::
         let algos =
           Config.
             [`Bmz; `Bmz8; default_chd; default_chd_ph; `Fch; `Bdz; `Bdz_ph]
         in
         product_3 algos seeds ["keys-fw.buf"; "keys-fw-1.buf"]
         |> List.map (fun (algo, seed, fn) -> packed_fw_test algo seed fn) ) ]

let () = run_test_tt_main suite
