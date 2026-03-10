structure Opcode:
sig
  datatype t =
    Constant
  | Nil
  | True
  | False
  | Equal
  | Greater
  | Less
  | Add
  | Subtract
  | Multiply
  | Divide
  | Not
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
  | Equal
  | Greater
  | Less
  | Add
  | Subtract
  | Multiply
  | Divide
  | Not
  | Negate
  | Return

  val encode =
    fn Constant => 0w0 : Word8.word
     | Nil => 0w1
     | True => 0w2
     | False => 0w3
     | Equal => 0w4
     | Greater => 0w5
     | Less => 0w6
     | Add => 0w7
     | Subtract => 0w8
     | Multiply => 0w9
     | Divide => 0w10
     | Not => 0w11
     | Negate => 0w12
     | Return => 0w13

  val decode =
    fn 0w0 => Constant
     | 0w1 => Nil
     | 0w2 => True
     | 0w3 => False
     | 0w4 => Equal
     | 0w5 => Greater
     | 0w6 => Less
     | 0w7 => Add
     | 0w8 => Subtract
     | 0w9 => Multiply
     | 0w10 => Divide
     | 0w11 => Not
     | 0w12 => Negate
     | 0w13 => Return
     | opcode => raise Fail ("invalid opcode " ^ Word8.toString opcode)
end
