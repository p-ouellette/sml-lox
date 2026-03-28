structure Globals :>
sig
  type t

  val new: unit -> t
  val insert: t * string * Value.t -> t
  val find: t * string -> Value.t option
  val defined: t * string -> bool
end =
struct
  type t = Value.t StringMap.map

  fun new () = StringMap.empty
  val insert = StringMap.insert
  val find = StringMap.find
  val defined = StringMap.inDomain
end
