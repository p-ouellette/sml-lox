structure Chunk:
sig
  structure Opcode:
  sig
    type t = Word8.word
    val return: t
  end

  type t
  val init: unit -> t
  val write: t * Opcode.t -> t
end =
struct
  structure Opcode = struct type t = Word8.word val return: t = 0w0 end

  type t = {count: int, capacity: int, code: Word8Array.array}

  fun init () =
    {count = 0, capacity = 8, code = Word8Array.array (8, 0w0)}

  fun write ({count, capacity, code}, byte) =
    let
      val (capacity, code) =
        if capacity > count then
          (capacity, code)
        else
          let
            val capacity' = capacity * 2
            val code' = Word8Array.array (capacity', 0w0)
          in
            Word8Array.copy {src = code, dst = code', di = 0};
            (capacity', code')
          end
    in
      Word8Array.update (code, count, byte);
      {count = count + 1, capacity = capacity, code = code}
    end
end
