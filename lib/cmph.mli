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

module Util : sig
  val with_output : (unit -> 'a) -> ('a, exn) result * string
end

module KeySet : sig
  type t

  val create : string list -> t
  (** Create a key set. *)
end

module Config : sig
  type chd_config = {keys_per_bucket: int; keys_per_bin: int}

  type algo =
    [ `Bmz
    | `Bmz8
    | `Chm
    | `Fch
    | `Bdz
    | `Bdz_ph
    | `Chd_ph of chd_config
    | `Chd of chd_config ]

  type t

  val default_chd : algo

  val default_chd_ph : algo

  val string_of_algo : algo -> string

  val create : ?verbose:bool -> ?algo:algo -> ?seed:int -> KeySet.t -> t
end

module Hash : sig
  type t

  val of_config : Config.t -> t

  val of_packed : string -> t

  val to_packed : t -> string

  val hash : t -> string -> int
end
