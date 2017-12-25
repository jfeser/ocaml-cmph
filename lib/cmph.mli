module KeySet : sig
  type t

  val of_list : string list -> t
end

module Config : sig
  type algo = [
      `Bmz | `Bmz8 | `Chm | `Brz | `Fch | `Bdz | `Bdz_ph | `Chd_ph | `Chd | `Count
  ]
  type t

  val create : ?algo:algo -> ?file:string -> KeySet.t -> t
end

module Hash : sig
  type t

  val create : Config.t -> t
  val hash : t -> string -> int
end
