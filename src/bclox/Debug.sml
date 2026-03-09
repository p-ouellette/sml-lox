structure Debug:
sig
  val printCode: bool
  val traceExecution: bool
  val disassembleChunk: Chunk.t * string -> unit
  val disassembleInstruction: Chunk.t * int -> int
end =
struct
  structure Op = Opcode

  val printCode = false
  val traceExecution = false

  fun disassembleChunk (chunk: Chunk.t, name) =
    let
      fun loop offset =
        if offset < Chunk.count chunk then
          loop (disassembleInstruction (chunk, offset))
        else
          ()
    in
      print ("== " ^ name ^ " ==\n");
      loop 0
    end

  and disassembleInstruction (chunk, offset) =
    let
      fun constantInstruction (name, chunk, offset) =
        let
          val constant = Word8.toInt (Chunk.sub (chunk, offset + 1))
        in
          print (StringCvt.padRight #" " 16 name);
          print " ";
          print (StringCvt.padLeft #" " 4 (Int.toString constant));
          print " '";
          Value.print (Chunk.getConstant (chunk, constant));
          print "'\n";
          offset + 2
        end

      fun simpleInstruction (name, offset) =
        (print (name ^ "\n"); offset + 1)

      val line = Chunk.getLine (chunk, offset)
    in
      print (StringCvt.padLeft #"0" 4 (Int.toString offset));
      print " ";
      if offset > 0 andalso line = Chunk.getLine (chunk, offset - 1) then
        print "   | "
      else
        print (StringCvt.padLeft #" " 4 (Int.toString line) ^ " ");

      case Chunk.getOpcode (chunk, offset) of
        Op.Constant => constantInstruction ("CONSTANT", chunk, offset)
      | Op.Nil => simpleInstruction ("NIL", offset)
      | Op.True => simpleInstruction ("TRUE", offset)
      | Op.False => simpleInstruction ("FALSE", offset)
      | Op.Add => simpleInstruction ("ADD", offset)
      | Op.Subtract => simpleInstruction ("SUBTRACT", offset)
      | Op.Multiply => simpleInstruction ("MULTIPLY", offset)
      | Op.Divide => simpleInstruction ("DIVIDE", offset)
      | Op.Not => simpleInstruction ("NOT", offset)
      | Op.Negate => simpleInstruction ("NEGATE", offset)
      | Op.Return => simpleInstruction ("RETURN", offset)
    end
end
