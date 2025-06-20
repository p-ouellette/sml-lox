structure Debug:
sig
  val disassembleChunk: Chunk.t * string -> unit
  val disassembleInstruction: Chunk.t * int -> int
end =
struct
  structure OP = Opcode

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
        OP.CONSTANT => constantInstruction ("OP_CONSTANT", chunk, offset)
      | OP.ADD => simpleInstruction ("OP_ADD", offset)
      | OP.SUBTRACT => simpleInstruction ("OP_SUBTRACT", offset)
      | OP.MULTIPLY => simpleInstruction ("OP_MULTIPLY", offset)
      | OP.DIVIDE => simpleInstruction ("OP_DIVIDE", offset)
      | OP.NEGATE => simpleInstruction ("OP_NEGATE", offset)
      | OP.RETURN => simpleInstruction ("OP_RETURN", offset)
    end
end
