(* 1. create adapter to key vector/file. *)
(* 2. Configure the hash function. *)
(* 3. Generate the hash function. *)
(* 4. Export the hash function by dumping to a file or outputting as a buffer. *)

open Ctypes
open Foreign

module Bindings = struct
  type file_p = unit ptr
  let file_p = ptr void
  let fopen = foreign "fopen" (string @-> string @-> returning file_p)

  let srand = foreign "srand" (int @-> returning void)

  type cmph_io_adapter_t = unit ptr
  let cmph_io_adapter_t : cmph_io_adapter_t typ = ptr void
  type cmph_t = unit ptr
  let cmph_t : cmph_t typ = ptr void
  type cmph_config_t = unit ptr
  let cmph_config_t : cmph_config_t typ = ptr void

  let cmph_io_nlfile_adapter =
    foreign "cmph_io_nlfile_adapter" (file_p @-> returning cmph_io_adapter_t)
  let cmph_io_nlfile_adapter_destroy =
    foreign "cmph_io_nlfile_adapter_destroy" (cmph_io_adapter_t @-> returning void)

  let cmph_io_vector_adapter =
    foreign "cmph_io_vector_adapter" (ptr string @-> int @-> returning cmph_io_adapter_t)
  let cmph_io_vector_adapter_destroy =
    foreign "cmph_io_vector_adapter_destroy" (cmph_io_adapter_t @-> returning void)

  let cmph_io_struct_vector_adapter =
    foreign "cmph_io_struct_vector_adapter" (string @-> int @-> int @-> int @-> int @-> returning cmph_io_adapter_t)
  let cmph_io_struct_vector_adapter_destroy =
    foreign "cmph_io_struct_vector_adapter_destroy" (cmph_io_adapter_t @-> returning void)

  let cmph_config_new =
    foreign "cmph_config_new" (cmph_io_adapter_t @-> returning cmph_config_t)
  let cmph_config_set_hashfuncs =
    foreign "cmph_config_set_hashfuncs" (cmph_config_t @-> ptr int @-> returning void)
  let cmph_config_set_algo =
    foreign "cmph_config_set_algo" (cmph_config_t @-> int @-> returning void)
  let cmph_config_set_mphf_fd =
    foreign "cmph_config_set_mphf_fd" (cmph_config_t @-> file_p @-> returning void)
  let cmph_config_destroy =
    foreign "cmph_config_destroy" (cmph_config_t @-> returning void)

  let cmph_new =
    foreign "cmph_new" (cmph_config_t @-> returning cmph_t)
  let cmph_search =
    foreign "cmph_search" (cmph_t @-> string @-> int @-> returning int)
  let cmph_destroy =
    foreign "cmph_destroy" (cmph_t @-> returning void)

  let cmph_pack =
    foreign "cmph_pack" (cmph_t @-> ocaml_bytes @-> returning void)
  let cmph_packed_size =
    foreign "cmph_packed_size" (cmph_t @-> returning int)
  let cmph_search_packed =
    foreign "cmph_search_packed" (ocaml_string @-> string @-> int @-> returning int)
end

external int_of_file_descr: Unix.file_descr -> int = "%identity"

module KeySet = struct
  exception Error of [`Empty | `Null_byte_in_key | `Not_fixed_width]

  type t = {
    keys : [`Strings of string CArray.t | `FixedWidth of string];
    adapter : Bindings.cmph_io_adapter_t;
  }

  let of_strings : string list -> t = fun keys ->
    if keys = [] then raise (Error `Empty);
    List.iter (String.iter (fun c ->
        if c = '\x00' then raise (Error `Null_byte_in_key))) keys;
    let nkeys = List.length keys in
    let arr = CArray.make string nkeys in
    List.iteri (CArray.set arr) keys;
    let ret = {
      keys = `Strings arr;
      adapter = Bindings.cmph_io_vector_adapter (CArray.start arr) nkeys;
    } in
    Gc.finalise
      (fun { adapter } -> Bindings.cmph_io_vector_adapter_destroy adapter)
      ret;
    ret

  let of_fixed_width : string list -> t = fun keys ->
    let len = match keys with
      | [] -> raise (Error `Empty)
      | x::xs -> List.fold_left (fun l k ->
          if String.length k <> l then raise (Error `Not_fixed_width) else l)
          (String.length x) xs
    in
    let nkeys = List.length keys in
    let buf = String.concat "" keys in
    let ret = {
      keys = `FixedWidth buf;
      adapter = Bindings.cmph_io_struct_vector_adapter buf len 0 len nkeys;
    } in
    Gc.finalise
      (fun { adapter } -> Bindings.cmph_io_struct_vector_adapter_destroy adapter)
      ret;
    ret

end

module Config = struct
  type hash = [`Jenkins | `Count]
  type algo = [
      `Bmz | `Bmz8 | `Chm | `Brz | `Fch | `Bdz | `Bdz_ph | `Chd_ph | `Chd | `Count
  ]
  type t = { config : Bindings.cmph_config_t }

  let hash_value = function
    | `Jenkins -> 0
    | `Count -> 1

  let algo_value = function
    | `Bmz -> 0
    | `Bmz8 -> 1
    | `Chm -> 2
    | `Brz -> 3
    | `Fch -> 4
    | `Bdz -> 5
    | `Bdz_ph -> 6
    | `Chd_ph -> 7
    | `Chd -> 8
    | `Count -> 9

  let string_of_algo = function
    | `Bmz -> "Bmz"
    | `Bmz8 -> "Bmz8"
    | `Chm -> "Chm"
    | `Brz -> "Brz"
    | `Fch -> "Fch"
    | `Bdz -> "Bdz"
    | `Bdz_ph -> "Bdz_ph"
    | `Chd_ph -> "Chd_ph"
    | `Chd -> "Chd"
    | `Count -> "Count"

  let create : ?algo:algo -> ?file:string -> ?seed:int -> KeySet.t -> t =
    fun ?(algo = `Brz) ?file ?seed keyset ->
      let seed = match seed with
        | Some x -> x
        | None -> Random.State.make_self_init () |> Random.State.bits
      in
      Bindings.srand seed;
      let config = Bindings.cmph_config_new keyset.adapter in
      Bindings.cmph_config_set_algo config (algo_value algo);
      let ret = { config } in
      Gc.finalise (fun { config } -> Bindings.cmph_config_destroy config)
        ret;
      ret
end

module Hash = struct
  type t =
    | Config of { hash : Bindings.cmph_t }
    | Packed of string

  let of_config : Config.t -> t = fun { config } ->
    let hash = Bindings.cmph_new config in
    let ret = Config { hash } in
    Gc.finalise (function
        | Config { hash } -> Bindings.cmph_destroy hash
        | _ -> ()) ret;
    ret

  let of_packed : string -> t = fun pack -> Packed pack

  let to_packed : t -> string = function
    | Config { hash } ->
      let size = Bindings.cmph_packed_size hash in
      let buf = Bytes.create size in
      Bindings.cmph_pack hash (ocaml_bytes_start buf);
      Bytes.to_string buf
    | Packed p -> p

  let hash : t -> string -> int = fun t key ->
    match t with
    | Config { hash } -> Bindings.cmph_search hash key (String.length key)
    | Packed pack -> Bindings.cmph_search_packed (ocaml_string_start pack) key
                       (String.length key)
end

