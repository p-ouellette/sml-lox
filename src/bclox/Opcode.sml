structure Opcode:
sig
  datatype t = CONSTANT | ADD | SUBTRACT | MULTIPLY | DIVIDE | NEGATE | RETURN

  val encode: t -> Word8.word
  val decode: Word8.word -> t
end =
struct
  datatype t = CONSTANT | ADD | SUBTRACT | MULTIPLY | DIVIDE | NEGATE | RETURN

  val encode =
    fn CONSTANT => 0w0 : Word8.word
     | ADD => 0w1
     | SUBTRACT => 0w2
     | MULTIPLY => 0w3
     | DIVIDE => 0w4
     | NEGATE => 0w5
     | RETURN => 0w6

  val decode =
    fn 0w0 => CONSTANT
     | 0w1 => ADD
     | 0w2 => SUBTRACT
     | 0w3 => MULTIPLY
     | 0w4 => DIVIDE
     | 0w5 => NEGATE
     | 0w6 => RETURN
     | opcode => raise Fail ("invalid opcode " ^ Word8.toString opcode)
end
