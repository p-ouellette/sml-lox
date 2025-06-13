structure Chunk:
sig
  type t
  val new: unit -> t
  val write: t * Word8.word -> t
  val disassemble: t * string -> unit
  val disassembleInstruction: t * int -> int
end =
struct
  structure OP = Opcode

  type t =
    {count: int, capacity: int, code: Word8Array.array, constants: Value.array}

  fun new () =
    { count = 0
    , capacity = 8
    , code = Word8Array.array (8, 0w0)
    , constants = Value.newArray ()
    }

  fun write ({count, capacity, code, constants}, byte) =
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
      { count = count + 1
      , capacity = capacity
      , code = code
      , constants = constants
      }
    end

  fun disassemble (chunk, name) =
    let
      fun loop offset =
        if offset < #count chunk then
          loop (disassembleInstruction (chunk, offset))
        else
          ()
    in
      print ("== " ^ name ^ " ==\n");
      loop 0
    end

  and disassembleInstruction (chunk: t, offset) =
    let
      fun simpleInstruction (name, offset) =
        (print (name ^ "\n"); offset + 1)
    in
      print (StringCvt.padLeft #"0" 4 (Int.toString offset));
      print " ";
      case OP.decode (Word8Array.sub (#code chunk, offset)) of
        OP.RETURN => simpleInstruction ("OP_RETURN", offset)
    end
end
