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
      val _ = Chunk.write (chunk, OP.encode OP.CONSTANT, 123)
      val _ = Chunk.write (chunk, Word8.fromInt constant, 123)

      val constant = Chunk.addConstant (chunk, 3.4)
      val _ = Chunk.write (chunk, OP.encode OP.CONSTANT, 123)
      val _ = Chunk.write (chunk, Word8.fromInt constant, 123)

      val _ = Chunk.write (chunk, OP.encode OP.ADD, 123)

      val constant = Chunk.addConstant (chunk, 5.6)
      val _ = Chunk.write (chunk, OP.encode OP.CONSTANT, 123)
      val _ = Chunk.write (chunk, Word8.fromInt constant, 123)

      val _ = Chunk.write (chunk, OP.encode OP.DIVIDE, 123)
      val _ = Chunk.write (chunk, OP.encode OP.NEGATE, 123)

      val _ = Chunk.write (chunk, OP.encode OP.RETURN, 123)
      val _ = Debug.disassembleChunk (chunk, "test chunk")

      val _ = VM.interpret chunk
    in
      0w0 : Word8.word
    end
end
