structure Opcode:
sig
  datatype t =
    Constant
  | Nil
  | True
  | False
  | Add
  | Subtract
  | Multiply
  | Divide
  | Negate
  | Return

  val encode: t -> Word8.word
  val decode: Word8.word -> t
end =
struct
  datatype t =
    Constant
  | Nil
  | True
  | False
  | Add
  | Subtract
  | Multiply
  | Divide
  | Negate
  | Return

  val encode =
    fn Constant => 0w0 : Word8.word
     | Nil => 0w1
     | True => 0w2
     | False => 0w3
     | Add => 0w4
     | Subtract => 0w5
     | Multiply => 0w6
     | Divide => 0w7
     | Negate => 0w8
     | Return => 0w9

  val decode =
    fn 0w0 => Constant
     | 0w1 => Nil
     | 0w2 => True
     | 0w3 => False
     | 0w4 => Add
     | 0w5 => Subtract
     | 0w6 => Multiply
     | 0w7 => Divide
     | 0w8 => Negate
     | 0w9 => Return
     | opcode => raise Fail ("invalid opcode " ^ Word8.toString opcode)
end
