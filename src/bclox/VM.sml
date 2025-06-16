structure VM:
sig
  type result

  val interpret: Chunk.t -> result
end =
struct
  structure OP = Opcode

  datatype result = OK | COMPILE_ERROR | RUNTIME_ERROR

  val debugTraceExecution = true

  fun push (stack, value) = value :: stack

  fun pop [] = raise Empty
    | pop (v :: stack) = (v, stack)

  fun interpret chunk =
    let
      fun readConstant i =
        Chunk.getConstant (chunk, Word8.toInt (Chunk.sub (chunk, i)))

      fun run (i, stack) =
        let
          val _ =
            if debugTraceExecution then
              ( print "          "
              ; app (fn v => (print "[ "; Value.print v; print " ]"))
                  (rev stack)
              ; print "\n"
              ; ignore (Debug.disassembleInstruction (chunk, i))
              )
            else
              ()
        in
          case Chunk.getOpcode (chunk, i) of
            OP.CONSTANT => run (i + 2, push (stack, readConstant (i + 1)))
          | OP.RETURN =>
              let val (v, _) = pop stack
              in Value.print v; print "\n"; OK
              end
        end
    in
      run (0, [])
    end
end
