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
