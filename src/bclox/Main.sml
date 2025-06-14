structure Main:
sig
  val main: string list -> Word8.word
end =
struct
  structure OP = Opcode

  fun main _ =
    let
      val chunk = Chunk.new ()
      val constant = Chunk.addConstant (chunk, 1.2)
    in
      Chunk.write (chunk, OP.encode OP.CONSTANT, 123);
      Chunk.write (chunk, Word8.fromInt constant, 123);
      Chunk.write (chunk, OP.encode OP.RETURN, 123);
      Debug.disassembleChunk (chunk, "test chunk");
      0w0 : Word8.word
    end
end
