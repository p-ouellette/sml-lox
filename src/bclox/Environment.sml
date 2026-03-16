structure Environment :>
sig
  (* The type of an environment mapping identifiers to values of type 'a. *)
  type 'a t

  val empty: 'a t
  val insert: 'a t * string * 'a -> 'a t
  val find: 'a t * string -> 'a option
end =
struct
  type 'a t = 'a StringMap.map

  val empty = StringMap.empty
  val insert = StringMap.insert
  val find = StringMap.find
end
