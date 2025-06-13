structure Opcode:
sig
  datatype t = CONSTANT | RETURN

  val encode: t -> Word8.word
  val decode: Word8.word -> t
end =
struct
  datatype t = CONSTANT | RETURN

  fun encode CONSTANT = 0w0 : Word8.word
    | encode RETURN = 0w1

  fun decode 0w0 = CONSTANT
    | decode 0w1 = RETURN
    | decode opcode =
        raise Fail ("invalid opcode " ^ Word8.toString opcode)
end
