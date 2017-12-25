(* 1. create adapter to key vector/file. *)
(* 2. Configure the hash function. *)
(* 3. Generate the hash function. *)
(* 4. Export the hash function by dumping to a file or outputting as a buffer. *)

open Ctypes
open Foreign

module Bindings = struct
  type file_p = unit ptr
  let file_p = ptr void
  let fdopen = foreign "fdopen" (int @-> string @-> returning file_p)

  type cmph_io_adapter_t = unit ptr
  let cmph_io_adapter_t : cmph_io_adapter_t typ = ptr void
  type cmph_t = unit ptr
  let cmph_t : cmph_t typ = ptr void
  type cmph_config_t = unit ptr
  let cmph_config_t : cmph_config_t typ = ptr void

  let cmph_io_vector_adapter =
    foreign "cmph_io_vector_adapter" (ptr string @-> int @-> returning cmph_io_adapter_t)
  let cmph_io_vector_adapter_destroy =
    foreign "cmph_io_vector_adapter_destroy" (cmph_io_adapter_t @-> returning void)

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
end

module KeySet = struct
  type t = {
    keys : string CArray.t;
    adapter : Bindings.cmph_io_adapter_t;
  }

  let of_list : string list -> t = fun keys ->
    let nkeys = List.length keys in
    let arr = CArray.make string nkeys in
    List.iteri (CArray.set arr) keys;
    let ret = {
      keys = arr;
      adapter = Bindings.cmph_io_vector_adapter (CArray.start arr) nkeys;
    } in
    Gc.finalise
      (fun { adapter } -> Bindings.cmph_io_vector_adapter_destroy adapter)
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

  external int_of_file_descr: Unix.file_descr -> int = "%identity"

  let create : ?algo:algo -> ?file:string -> KeySet.t -> t =
    fun ?(algo = `Brz) ?file keyset ->
      let config = Bindings.cmph_config_new keyset.adapter in
      let ret = { config } in
      Gc.finalise (fun { config } -> Bindings.cmph_config_destroy config)
        ret;
      ret
end

module Hash = struct
  type t = { hash : Bindings.cmph_t }

  let create : Config.t -> t = fun { config } ->
    let hash = Bindings.cmph_new config in
    let ret = { hash } in
    Gc.finalise (fun { hash } -> Bindings.cmph_destroy hash) ret;
    ret

  let hash : t -> string -> int = fun { hash } key ->
    Bindings.cmph_search hash key (String.length key)
end

