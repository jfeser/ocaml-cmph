module KeySet : sig
  type t

  val of_cstrings : string list -> t
  (** Create a key set from a list of strings. Keys can be of different lengths,
     but cannot contain null bytes. *)

  val of_nlstrings : string list -> t

  val of_fixed_width : string list -> t
  (** Create a key set from a list of fixed-width strings. Keys can contain any
     byte value. *)

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
