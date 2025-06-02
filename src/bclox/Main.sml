structure Main:
sig
  val main: string list -> Word8.word
end =
struct
  structure OP = Chunk.Opcode

  fun main _ =
    let
      val chunk = Chunk.init ()
      val _ = Chunk.write (chunk, OP.return)
    in
      0w0 : Word8.word
    end
end
