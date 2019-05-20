(* 1. create adapter to key vector/file. *)
(* 2. Configure the hash function. *)
(* 3. Generate the hash function. *)
(* 4. Export the hash function by dumping to a file or outputting as a buffer. *)

open Core
open Ctypes
open Foreign
module Util = Util

module Bindings = struct
  let srand = foreign "srand" (int @-> returning void)

  type cmph_io_adapter_t

  let cmph_io_adapter_t : cmph_io_adapter_t structure typ =
    structure "cmph_io_adapter_t"

  let _data = field cmph_io_adapter_t "data" (ptr void)

  let nkeys = field cmph_io_adapter_t "nkeys" uint32_t

  let read =
    field cmph_io_adapter_t "read"
      (funptr (ptr void @-> ptr (ptr char) @-> ptr uint32_t @-> returning int))

  let dispose =
    field cmph_io_adapter_t "dispose"
      (funptr (ptr void @-> ptr char @-> uint32_t @-> returning void))

  let rewind =
    field cmph_io_adapter_t "rewind" (funptr (ptr void @-> returning void))

  let () = seal cmph_io_adapter_t

  type cmph_t = unit ptr

  let cmph_t : cmph_t typ = ptr void

  type cmph_config_t = unit ptr

  let cmph_config_t : cmph_config_t typ = ptr void

  let cmph_config_new =
    foreign "cmph_config_new" (ptr cmph_io_adapter_t @-> returning cmph_config_t)

  let cmph_config_set_verbosity =
    foreign "cmph_config_set_verbosity" (cmph_config_t @-> int @-> returning void)

  let cmph_config_set_algo =
    foreign "cmph_config_set_algo" (cmph_config_t @-> int @-> returning void)

  let cmph_config_set_b =
    foreign "cmph_config_set_b" (cmph_config_t @-> int @-> returning void)

  let cmph_config_set_graphsize =
    foreign "cmph_config_set_graphsize" (cmph_config_t @-> double @-> returning void)

  let cmph_config_set_keys_per_bin =
    foreign "cmph_config_set_keys_per_bin" (cmph_config_t @-> int @-> returning void)

  let cmph_config_destroy =
    foreign "cmph_config_destroy" (cmph_config_t @-> returning void)

  let cmph_new = foreign "cmph_new" (cmph_config_t @-> returning cmph_t)

  let cmph_search =
    foreign "cmph_search" (cmph_t @-> string @-> int @-> returning int)

  let cmph_destroy = foreign "cmph_destroy" (cmph_t @-> returning void)

  let cmph_pack = foreign "cmph_pack" (cmph_t @-> ocaml_bytes @-> returning void)

  let cmph_packed_size = foreign "cmph_packed_size" (cmph_t @-> returning int)

  let cmph_search_packed =
    foreign "cmph_search_packed" (ocaml_string @-> string @-> int @-> returning int)
end

exception
  Error of
    [ `Empty
    | `No_suitable_ctor
    | `Hash_new_failed of string
    | `Parameter_range
    | `Contains_null_byte of string
    | `Contains_newline of string
    | `Not_fixed_width of string * int ]
  [@@deriving sexp]

module KeySet = struct
  type t =
    { length: int
    ; read: unit ptr -> char ptr ptr -> Unsigned.uint32 ptr -> int
    ; dispose: unit ptr -> char ptr -> Unsigned.uint32 -> unit
    ; rewind: unit ptr -> unit
    ; adapter: (Bindings.cmph_io_adapter_t, [`Struct]) structured ptr }

  let uniq keys =
    List.sort keys ~compare:[%compare: string]
    |> List.remove_consecutive_duplicates ~equal:[%compare.equal: string]

  let create keys =
    let open Bigarray in
    if List.is_empty keys then raise (Error `Empty) ;
    let keys =
      uniq keys
      |> List.map ~f:(fun k -> Array1.of_array char C_layout (String.to_array k))
    in
    let key_ptrs =
      List.map keys ~f:(bigarray_start array1) |> CArray.of_list Ctypes.(ptr char)
    in
    let keys = Array.of_list keys in
    let key_idx = ref 0 in
    let nkeys = Unsigned.UInt32.of_int (Array.length keys) in
    let read _ key_ptr key_len =
      (let module Array = CArray in
      key_ptr <-@ key_ptrs.(!key_idx)) ;
      let len = Array1.dim keys.(!key_idx) in
      key_len <-@ Unsigned.UInt32.of_int len ;
      incr key_idx ;
      len
    in
    let dispose _ _ _ = () in
    let rewind _ = key_idx := 0 in
    let adapter = make Bindings.cmph_io_adapter_t in
    setf adapter Bindings.nkeys nkeys ;
    setf adapter Bindings.read read ;
    setf adapter Bindings.dispose dispose ;
    setf adapter Bindings.rewind rewind ;
    {length= Array.length keys; read; dispose; rewind; adapter= addr adapter}
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

  type t = {config: Bindings.cmph_config_t; keyset: KeySet.t}

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
        if keyset.KeySet.length > 256 then raise (Error `Parameter_range) else ()
    | _ -> ()

  let create : ?verbose:bool -> ?algo:algo -> ?seed:int -> KeySet.t -> t =
   fun ?(verbose = false) ?(algo = default_chd) ?seed keyset ->
    valid_algo algo keyset ;
    let seed =
      match seed with
      | Some x -> x
      | None -> Random.State.make_self_init () |> Random.State.bits
    in
    Bindings.srand seed ;
    let config = Bindings.cmph_config_new keyset.adapter in
    Bindings.cmph_config_set_graphsize config 0.90 ;
    Bindings.cmph_config_set_algo config (algo_value algo) ;
    Bindings.cmph_config_set_verbosity config (if verbose then 1 else 0) ;
    ( match algo with
    | `Chd c | `Chd_ph c ->
        Bindings.cmph_config_set_b config c.keys_per_bucket ;
        Bindings.cmph_config_set_keys_per_bin config c.keys_per_bin
    | _ -> () ) ;
    let ret = {config; keyset} in
    Caml.Gc.finalise (fun {config; _} -> Bindings.cmph_config_destroy config) ret ;
    ret
end

module Hash = struct
  type t = Config of {hash: Bindings.cmph_t; config: Config.t} | Packed of string

  let of_config : Config.t -> t =
   fun ({config= cconfig; _} as config) ->
    let hash, output = Util.with_output (fun () -> Bindings.cmph_new cconfig) in
    match hash with
    | Ok hash ->
        if Ctypes.is_null hash then raise (Error (`Hash_new_failed output)) ;
        let ret = Config {hash; config} in
        Caml.Gc.finalise
          (function Config {hash; _} -> Bindings.cmph_destroy hash | _ -> ())
          ret ;
        ret
    | Error _ -> raise (Error (`Hash_new_failed output))

  let of_packed : string -> t = fun pack -> Packed pack

  let to_packed : t -> string = function
    | Config {hash; _} ->
        let size = Bindings.cmph_packed_size hash in
        let buf = Bytes.create size in
        Bindings.cmph_pack hash (ocaml_bytes_start buf) ;
        Bytes.to_string buf
    | Packed p -> p

  let hash : t -> string -> int =
   fun t key ->
    match t with
    | Config {hash; _} -> Bindings.cmph_search hash key (String.length key)
    | Packed pack ->
        Bindings.cmph_search_packed (ocaml_string_start pack) key
          (String.length key)
end
