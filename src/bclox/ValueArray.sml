structure ValueArray:
sig
  type t
  val new: unit -> t
  val count: t -> int
  val sub: t * int -> Value.t
  val write: t * Value.t -> unit
end =
struct
  structure A = Array

  type t = {count: int ref, values: Value.t A.array ref}

  fun new () =
    {count = ref 0, values = ref (A.array (8, Value.Nil))}

  fun count (array: t) =
    !(#count array)

  fun sub (array: t, i) =
    A.sub (!(#values array), i)

  fun growArray (array, count) =
    A.tabulate (count * 2, fn i =>
      if i >= count then A.sub (array, i) else Value.Nil)

  fun write ({count, values}, value) =
    let
      val capacity = A.length (!values)
    in
      if capacity < !count + 1 then values := growArray (!values, capacity)
      else ();
      A.update (!values, !count, value);
      count := !count + 1
    end
end
