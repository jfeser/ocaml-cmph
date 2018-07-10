open OUnit2
open Cmph

let algos =
  Config.[`Bmz; `Bmz8; default_chd; default_chd_ph; `Fch; `Bdz; `Bdz_ph]

type 'a result = Ok of 'a | Error of exn

let with_output thunk =
  let old_stdout = Unix.(dup stdout) in
  let old_stderr = Unix.(dup stderr) in
  let tmp_fn = Filename.temp_file "output" "log" in
  let tmp_fd = Unix.(openfile tmp_fn [O_RDWR; O_CREAT; O_KEEPEXEC] 0o600) in
  Unix.(dup2 tmp_fd stdout) ;
  Unix.(dup2 tmp_fd stderr) ;
  let cleanup () =
    Unix.(dup2 old_stdout stdout) ;
    Unix.(dup2 old_stderr stderr) ;
    Unix.close old_stdout ;
    Unix.close old_stderr ;
    assert (Unix.(lseek tmp_fd 0 SEEK_SET) = 0) ;
    let tot_len = Unix.((fstat tmp_fd).st_size) in
    let buf = Bytes.create tot_len in
    let rec read_all ofs rem_len =
      if rem_len <= 0 then ()
      else
        let read_len = Unix.read tmp_fd buf ofs rem_len in
        let ofs = ofs + read_len in
        let rem_len = rem_len - read_len in
        read_all ofs rem_len
    in
    read_all 0 tot_len ; Bytes.to_string buf
  in
  try
    let ret = Ok (thunk ()) in
    let out = cleanup () in
    (ret, out)
  with e ->
    let out = cleanup () in
    (Error e, out)

let check_result ctxt out =
  let ret, cmph_output = out in
  match ret with
  | Ok (kv_config, kv_packed) -> assert_equal ~ctxt kv_config kv_packed
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

let rec product_3 l1 l2 l3 =
  List.map
    (fun x1 ->
      List.map (fun x2 -> List.map (fun x3 -> (x1, x2, x3)) l3) l2
      |> List.concat )
    l1
  |> List.concat

let suite =
  "tests"
  >::: [ "packed-strings"
         >::: ( product_3 algos [0; 1; 2; 3; 4; 5] ["keys-long.txt"]
              |> List.map (fun (algo, seed, fn) ->
                     packed_strings_test algo seed fn ) )
       ; "packed-fixedwidth"
         >::: ( product_3 algos [0; 1; 2; 3; 4; 5]
                  ["keys-fw.buf"; "keys-fw-1.buf"]
              |> List.map (fun (algo, seed, fn) -> packed_fw_test algo seed fn)
              ) ]

let () = run_test_tt_main suite
