structure Value:
sig
  type t
  type array
  val newArray: unit -> array
  val writeArray: array * t -> array
end =
struct
  type t = real

  type array = {count: int, capacity: int, values: RealArray.array}

  fun newArray () =
    {count = 0, capacity = 8, values = RealArray.array (8, 0.0)}

  fun writeArray ({count, capacity, values}, value) =
    let
      val (capacity, values) =
        if capacity > count then
          (capacity, values)
        else
          let
            val capacity' = capacity * 2
            val values' = RealArray.array (capacity', 0.0)
          in
            RealArray.copy {src = values, dst = values', di = 0};
            (capacity', values')
          end
    in
      RealArray.update (values, count, value);
      {count = count + 1, capacity = capacity, values = values}
    end
end
