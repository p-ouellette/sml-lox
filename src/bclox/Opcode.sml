structure Opcode:
sig
  datatype t =
    Constant
  | Nil
  | True
  | False
  | Pop
  | GetGlobal
  | DefineGlobal
  | Equal
  | Greater
  | Less
  | Add
  | Subtract
  | Multiply
  | Divide
  | Not
  | Negate
  | Print
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
  | Pop
  | GetGlobal
  | DefineGlobal
  | Equal
  | Greater
  | Less
  | Add
  | Subtract
  | Multiply
  | Divide
  | Not
  | Negate
  | Print
  | Return

  val encode =
    fn Constant => 0w0 : Word8.word
     | Nil => 0w1
     | True => 0w2
     | False => 0w3
     | Pop => 0w4
     | GetGlobal => 0w5
     | DefineGlobal => 0w6
     | Equal => 0w7
     | Greater => 0w8
     | Less => 0w9
     | Add => 0w10
     | Subtract => 0w11
     | Multiply => 0w12
     | Divide => 0w13
     | Not => 0w14
     | Negate => 0w15
     | Print => 0w16
     | Return => 0w17

  val decode =
    fn 0w0 => Constant
     | 0w1 => Nil
     | 0w2 => True
     | 0w3 => False
     | 0w4 => Pop
     | 0w5 => GetGlobal
     | 0w6 => DefineGlobal
     | 0w7 => Equal
     | 0w8 => Greater
     | 0w9 => Less
     | 0w10 => Add
     | 0w11 => Subtract
     | 0w12 => Multiply
     | 0w13 => Divide
     | 0w14 => Not
     | 0w15 => Negate
     | 0w16 => Print
     | 0w17 => Return
     | opcode => raise Fail ("invalid opcode " ^ Word8.toString opcode)
end
