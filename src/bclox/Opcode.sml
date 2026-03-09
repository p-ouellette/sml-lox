structure Opcode:
sig
  datatype t =
    CONSTANT
  | NIL
  | TRUE
  | FALSE
  | ADD
  | SUBTRACT
  | MULTIPLY
  | DIVIDE
  | NEGATE
  | RETURN

  val encode: t -> Word8.word
  val decode: Word8.word -> t
end =
struct
  datatype t =
    CONSTANT
  | NIL
  | TRUE
  | FALSE
  | ADD
  | SUBTRACT
  | MULTIPLY
  | DIVIDE
  | NEGATE
  | RETURN

  val encode =
    fn CONSTANT => 0w0 : Word8.word
     | NIL => 0w1
     | TRUE => 0w2
     | FALSE => 0w3
     | ADD => 0w4
     | SUBTRACT => 0w5
     | MULTIPLY => 0w6
     | DIVIDE => 0w7
     | NEGATE => 0w8
     | RETURN => 0w9

  val decode =
    fn 0w0 => CONSTANT
     | 0w1 => NIL
     | 0w2 => TRUE
     | 0w3 => FALSE
     | 0w4 => ADD
     | 0w5 => SUBTRACT
     | 0w6 => MULTIPLY
     | 0w7 => DIVIDE
     | 0w8 => NEGATE
     | 0w9 => RETURN
     | opcode => raise Fail ("invalid opcode " ^ Word8.toString opcode)
end
