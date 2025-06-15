structure VM:
sig
  type result

  val interpret: Chunk.t -> result
end =
struct
  structure OP = Opcode

  datatype result = OK | COMPILE_ERROR | RUNTIME_ERROR

  val debugTraceExecution = true

  fun interpret chunk =
    let
      fun readConstant i =
        Chunk.getConstant (chunk, Word8.toInt (Chunk.sub (chunk, i)))

      fun run i =
        let
          val _ =
            if debugTraceExecution then Debug.disassembleInstruction (chunk, i)
            else 0
        in
          case Chunk.getOpcode (chunk, i) of
            OP.CONSTANT =>
              let val constant = readConstant (i + 1)
              in Value.print constant; print "\n"; run (i + 2)
              end
          | OP.RETURN => OK
        end
    in
      run 0
    end
end
