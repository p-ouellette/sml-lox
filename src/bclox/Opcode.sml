structure Opcode:
sig
  datatype t = RETURN

  val encode: t -> Word8.word
  val decode: Word8.word -> t
end =
struct
  datatype t = RETURN

  fun encode RETURN = 0w0 : Word8.word

  fun decode 0w0 = RETURN
    | decode opcode =
        raise Fail ("invalid opcode " ^ Word8.toString opcode)
end
