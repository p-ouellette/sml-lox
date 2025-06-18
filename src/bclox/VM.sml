structure VM:
sig
  datatype result = OK | COMPILE_ERROR | RUNTIME_ERROR

  val interpret: string -> result
end =
struct
  structure OP = Opcode

  datatype result = OK | COMPILE_ERROR | RUNTIME_ERROR

  val debugTraceExecution = true

  fun push (stack, value) = value :: stack

  fun pop [] = raise Empty
    | pop (v :: stack) = (v, stack)

  fun interpret source =
    let
      val chunk = Chunk.new ()

      fun readConstant i =
        Chunk.getConstant (chunk, Word8.toInt (Chunk.sub (chunk, i)))

      fun run (i, stack) =
        let
          fun binaryOp f =
            let
              val (b, stack) = pop stack
              val (a, stack) = pop stack
            in
              run (i + 1, push (stack, f (a, b)))
            end

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
          | OP.ADD => binaryOp op+
          | OP.SUBTRACT => binaryOp op-
          | OP.MULTIPLY => binaryOp op*
          | OP.DIVIDE => binaryOp op/
          | OP.NEGATE =>
              let val (v, stack) = pop stack
              in run (i + 1, push (stack, ~v))
              end
          | OP.RETURN =>
              let val (v, _) = pop stack
              in Value.print v; print "\n"; OK
              end
        end
    in
      Compiler.compile source;
      OK
    end
end
