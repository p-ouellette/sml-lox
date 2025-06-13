structure Main:
sig
  val main: string list -> Word8.word
end =
struct
  structure OP = Opcode

  fun main _ =
    let
      val chunk = Chunk.new ()
    in
      Chunk.write (chunk, OP.encode OP.RETURN);
      Chunk.disassemble (chunk, "test chunk");
      0w0 : Word8.word
    end
end
