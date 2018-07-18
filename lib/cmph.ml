(* 1. create adapter to key vector/file. *)
(* 2. Configure the hash function. *)
(* 3. Generate the hash function. *)
(* 4. Export the hash function by dumping to a file or outputting as a buffer. *)

open Base
open Stdio
open Ctypes
open Foreign
module Util = Util

module Bindings = struct
  type file_p = unit ptr

  let file_p = ptr void

  let fopen = foreign "fopen" (string @-> string @-> returning file_p)

  let fclose = foreign "fclose" (file_p @-> returning void)

  let srand = foreign "srand" (int @-> returning void)

  type cmph_io_adapter_t = unit ptr

  let cmph_io_adapter_t : cmph_io_adapter_t typ = ptr void

  type cmph_t = unit ptr

  let cmph_t : cmph_t typ = ptr void

  type cmph_config_t = unit ptr

  let cmph_config_t : cmph_config_t typ = ptr void

  let cmph_io_nlnkfile_adapter =
    foreign "cmph_io_nlnkfile_adapter"
      (file_p @-> int @-> returning cmph_io_adapter_t)

  let cmph_io_nlnkfile_adapter_destroy =
    foreign "cmph_io_nlnkfile_adapter_destroy"
      (cmph_io_adapter_t @-> returning void)

  let cmph_io_vector_adapter =
    foreign "cmph_io_vector_adapter"
      (ptr string @-> int @-> returning cmph_io_adapter_t)

  let cmph_io_vector_adapter_destroy =
    foreign "cmph_io_vector_adapter_destroy"
      (cmph_io_adapter_t @-> returning void)

  let cmph_io_struct_vector_adapter =
    foreign "cmph_io_struct_vector_adapter"
      (string @-> int @-> int @-> int @-> int @-> returning cmph_io_adapter_t)

  let cmph_io_struct_vector_adapter_destroy =
    foreign "cmph_io_struct_vector_adapter_destroy"
      (cmph_io_adapter_t @-> returning void)

  let cmph_config_new =
    foreign "cmph_config_new" (cmph_io_adapter_t @-> returning cmph_config_t)

  let cmph_config_set_verbosity =
    foreign "cmph_config_set_verbosity"
      (cmph_config_t @-> int @-> returning void)

  let cmph_config_set_algo =
    foreign "cmph_config_set_algo" (cmph_config_t @-> int @-> returning void)

  let cmph_config_set_b =
    foreign "cmph_config_set_b" (cmph_config_t @-> int @-> returning void)

  let cmph_config_set_keys_per_bin =
    foreign "cmph_config_set_keys_per_bin"
      (cmph_config_t @-> int @-> returning void)

  let cmph_config_destroy =
    foreign "cmph_config_destroy" (cmph_config_t @-> returning void)

  let cmph_new = foreign "cmph_new" (cmph_config_t @-> returning cmph_t)

  let cmph_search =
    foreign "cmph_search" (cmph_t @-> string @-> int @-> returning int)

  let cmph_destroy = foreign "cmph_destroy" (cmph_t @-> returning void)

  let cmph_pack =
    foreign "cmph_pack" (cmph_t @-> ocaml_bytes @-> returning void)

  let cmph_packed_size = foreign "cmph_packed_size" (cmph_t @-> returning int)

  let cmph_search_packed =
    foreign "cmph_search_packed"
      (ocaml_string @-> string @-> int @-> returning int)
end

exception
  Error of
    [`Empty | `No_suitable_ctor | `Hash_new_failed of string | `Parameter_range]
  [@@deriving sexp]

module KeySet = struct
  type t =
    { length: int
    ; keys:
        [ `Strings of string CArray.t
        | `FixedWidth of string
        | `File of Bindings.file_p * string ]
    ; adapter: Bindings.cmph_io_adapter_t }

  let is_fixed_width = function
    | [] -> raise (Error `Empty)
    | x :: xs ->
        let x_len = String.length x in
        List.for_all ~f:(fun x' -> String.length x' = x_len) xs

  let contains keys char =
    List.exists ~f:(fun k -> String.contains k char) keys

  let of_cstrings : string list -> t =
   fun keys ->
    let nkeys = List.length keys in
    let arr = CArray.make string nkeys in
    List.iteri ~f:(CArray.set arr) keys ;
    let ret =
      { length= List.length keys
      ; keys= `Strings arr
      ; adapter= Bindings.cmph_io_vector_adapter (CArray.start arr) nkeys }
    in
    Caml.Gc.finalise
      (fun {adapter; _} -> Bindings.cmph_io_vector_adapter_destroy adapter)
      ret ;
    ret

  let of_fixed_width : string list -> t =
   fun keys ->
    let len =
      match keys with
      | [] -> raise (Error `Empty)
      | x :: xs ->
          List.fold_left
            ~f:(fun l k -> if String.length k <> l then assert false else l)
            ~init:(String.length x) xs
    in
    let nkeys = List.length keys in
    let buf = String.concat ~sep:"" keys in
    let ret =
      { length= List.length keys
      ; keys= `FixedWidth buf
      ; adapter= Bindings.cmph_io_struct_vector_adapter buf len 0 len nkeys }
    in
    Caml.Gc.finalise
      (fun {adapter; _} ->
        Bindings.cmph_io_struct_vector_adapter_destroy adapter )
      ret ;
    ret

  let of_nlstrings : string list -> t =
   fun keys ->
    let nkeys = List.length keys in
    let fn = Caml.Filename.temp_file "keys" "txt" in
    let ch = Out_channel.create fn in
    List.iter ~f:(fun k -> Out_channel.output_string ch (k ^ "\n")) keys ;
    Out_channel.close ch ;
    let fp = Bindings.fopen fn "r" in
    let ret =
      { length= List.length keys
      ; keys= `File (fp, fn)
      ; adapter= Bindings.(cmph_io_nlnkfile_adapter fp nkeys) }
    in
    Caml.Gc.finalise
      (function
        | {adapter; keys= `File (fp, fn); _} ->
            Bindings.cmph_io_nlnkfile_adapter_destroy adapter ;
            Bindings.fclose fp ;
            Caml.Sys.remove fn
        | _ -> assert false)
      ret ;
    ret

  let create : string list -> t =
   fun keys ->
    (* Pick a keyset creation method. *)
    let ctor =
      if is_fixed_width keys then of_fixed_width
      else if not (contains keys '\x00') then of_cstrings
      else
        (* if not (contains keys '\n') then of_nlstrings else *)
        raise (Error `No_suitable_ctor)
    in
    ctor keys
end

module Config = struct
  type hash = [`Jenkins | `Count] [@@deriving sexp]

  type chd_config = {keys_per_bucket: int; keys_per_bin: int} [@@deriving sexp]

  type algo =
    [ `Bmz
    | `Bmz8
    | `Chm
    | `Fch
    | `Bdz
    | `Bdz_ph
    | `Chd_ph of chd_config
    | `Chd of chd_config ]
  [@@deriving sexp]

  type t = {config: Bindings.cmph_config_t}

  let algo_value = function
    | `Bmz -> 0
    | `Bmz8 -> 1
    | `Chm -> 2
    | `Fch -> 4
    | `Bdz -> 5
    | `Bdz_ph -> 6
    | `Chd_ph _ -> 7
    | `Chd _ -> 8

  let string_of_algo = function
    | `Bmz -> "Bmz"
    | `Bmz8 -> "Bmz8"
    | `Chm -> "Chm"
    | `Fch -> "Fch"
    | `Bdz -> "Bdz"
    | `Bdz_ph -> "Bdz_ph"
    | `Chd_ph _ -> "Chd_ph"
    | `Chd _ -> "Chd"

  let default_chd = `Chd {keys_per_bucket= 4; keys_per_bin= 1}

  let default_chd_ph = `Chd_ph {keys_per_bucket= 4; keys_per_bin= 1}

  let valid_algo algo keyset =
    match algo with
    | `Chd c | `Chd_ph c ->
        if
          c.keys_per_bucket < 1 || c.keys_per_bucket > 32 || c.keys_per_bin < 1
          || c.keys_per_bin > 128
        then raise (Error `Parameter_range)
        else ()
    | `Bmz8 ->
        if keyset.KeySet.length > 256 then raise (Error `Parameter_range)
        else ()
    | _ -> ()

  let create : ?verbose:bool -> ?algo:algo -> ?seed:int -> KeySet.t -> t =
   fun ?(verbose= false) ?(algo= default_chd) ?seed keyset ->
    valid_algo algo keyset ;
    let seed =
      match seed with
      | Some x -> x
      | None -> Random.State.make_self_init () |> Random.State.bits
    in
    Bindings.srand seed ;
    let config = Bindings.cmph_config_new keyset.adapter in
    Bindings.cmph_config_set_algo config (algo_value algo) ;
    Bindings.cmph_config_set_verbosity config (if verbose then 1 else 0) ;
    ( match algo with
    | `Chd c | `Chd_ph c ->
        Bindings.cmph_config_set_b config c.keys_per_bucket ;
        Bindings.cmph_config_set_keys_per_bin config c.keys_per_bin
    | _ -> () ) ;
    let ret = {config} in
    Caml.Gc.finalise (fun {config} -> Bindings.cmph_config_destroy config) ret ;
    ret
end

module Hash = struct
  type t = Config of {hash: Bindings.cmph_t} | Packed of string

  let of_config : Config.t -> t =
   fun {config} ->
    let hash, output = Util.with_output (fun () -> Bindings.cmph_new config) in
    match hash with
    | Ok hash ->
        if Ctypes.is_null hash then raise (Error (`Hash_new_failed output)) ;
        let ret = Config {hash} in
        Caml.Gc.finalise
          (function Config {hash} -> Bindings.cmph_destroy hash | _ -> ())
          ret ;
        ret
    | Error _ -> raise (Error (`Hash_new_failed output))

  let of_packed : string -> t = fun pack -> Packed pack

  let to_packed : t -> string = function
    | Config {hash} ->
        let size = Bindings.cmph_packed_size hash in
        let buf = Bytes.create size in
        Bindings.cmph_pack hash (ocaml_bytes_start buf) ;
        Bytes.to_string buf
    | Packed p -> p

  let hash : t -> string -> int =
   fun t key ->
    match t with
    | Config {hash} -> Bindings.cmph_search hash key (String.length key)
    | Packed pack ->
        Bindings.cmph_search_packed (ocaml_string_start pack) key
          (String.length key)
end
