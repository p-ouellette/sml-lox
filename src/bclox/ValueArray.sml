structure ValueArray:
sig
  type t
  val new: unit -> t
  val count: t -> int
  val sub: t * int -> Value.t
  val write: t * Value.t -> unit
end =
struct
  structure A = RealArray

  type t = {count: int ref, values: A.array ref}

  fun new () =
    {count = ref 0, values = ref (A.array (8, 0.0))}

  fun count (array: t) =
    !(#count array)

  fun sub (array: t, i) =
    A.sub (!(#values array), i)

  fun growArray (array, oldlen, newlen) =
    A.tabulate (newlen, fn i => if i >= oldlen then A.sub (array, i) else 0.0)

  fun write ({count, values}, value) =
    let
      val capacity = A.length (!values)
    in
      if capacity < !count + 1 then
        values := growArray (!values, capacity, capacity * 2)
      else
        ();
      A.update (!values, !count, value);
      count := !count + 1
    end
end
