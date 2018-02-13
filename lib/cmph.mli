module KeySet : sig
  type t

  (** Create a key set from a list of strings. Keys can be of different lengths,
     but cannot contain null bytes. *)
  val of_strings : string list -> t

  (** Create a key set from a list of fixed-width strings. Keys can contain any
     byte value. *)
  val of_fixed_width : string list -> t
end

module Config : sig
  type algo = [
      `Bmz | `Bmz8 | `Chm | `Brz | `Fch | `Bdz | `Bdz_ph | `Chd_ph | `Chd | `Count
  ]
  type t

  val string_of_algo : algo -> string

  val create : ?algo:algo -> ?file:string -> ?seed:int -> KeySet.t -> t
end

module Hash : sig
  type t

  val of_config : Config.t -> t
  val of_packed : string -> t
  val to_packed : t -> string
  val hash : t -> string -> int
end
