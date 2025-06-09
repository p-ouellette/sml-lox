structure Main:
sig
  val main: string list -> Word8.word
end =
struct
  structure OP = Chunk.Opcode

  fun main _ =
    let
      val chunk = Chunk.init ()
      val chunk = Chunk.write (chunk, OP.encode OP.RETURN)
    in
      Chunk.disassemble (chunk, "test chunk");
      0w0 : Word8.word
    end
end
