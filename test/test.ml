open OUnit2
open Cmph

let algos =
  [`Bmz; `Bmz8; `Chm; `Brz; `Fch; `Bdz; `Bdz_ph; `Chd_ph; `Chd; `Count]

let suite = "tests" >::: [
    "packed" >::: (
      List.map (fun algo ->
          "" >:: (fun ctxt ->
              let ch = open_in "test/keys-long.txt" in
              let rec read lines =
                try
                  let line = input_line ch in
                  read (line::lines)
                with End_of_file -> List.rev lines
              in
              let keys = read [] in
              let keyset = KeySet.of_list keys in

              let config = Config.create ~algo keyset in
              let chash = Hash.of_config config in
              let kv_config = List.map (Hash.hash chash) keys in
              let phash = Hash.to_packed chash |> Hash.of_packed in
              let kv_packed = List.map (Hash.hash phash) keys in
              assert_equal ~ctxt kv_config kv_packed))
        algos)
  ]

let () = run_test_tt_main suite
