structure Value:
sig
  type t
  type array
  val newArray: unit -> array
  val writeArray: array * t -> unit
end =
struct
  structure A = RealArray

  type t = real

  type array = {count: int ref, values: A.array ref}

  fun newArray () =
    {count = ref 0, values = ref (A.array (8, 0.0))}

  fun growArray (array, oldCount, newCount) =
    A.tabulate (newCount, fn i =>
      if i >= oldCount then A.sub (array, i) else 0.0)

  fun writeArray ({count, values}, value) =
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
