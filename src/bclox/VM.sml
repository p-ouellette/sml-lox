structure VM:
sig
  datatype result = Ok | CompileError | RuntimeError

  val interpret: string -> result
end =
struct
  structure Op = Opcode
  structure V = Value

  datatype result = Ok | CompileError | RuntimeError

  fun push (stack, value) = value :: stack

  fun pop [] = raise Empty
    | pop (v :: stack) = (v, stack)

  fun eprint s =
    (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr)

  fun runtimeError (chunk, ip, message) =
    let
      val line = Chunk.getLine (chunk, ip)
    in
      eprint (message ^ "\n");
      eprint ("[line " ^ Int.toString line ^ "] in script\n");
      RuntimeError
    end

  fun readConstant (chunk, i) =
    Chunk.getConstant (chunk, Word8.toInt (Chunk.sub (chunk, i)))

  fun run (chunk, ip, stack) =
    let
      fun binaryOp f =
        let
          val (b, stack) = pop stack
          val (a, stack) = pop stack
        in
          case (a, b) of
            (V.Number a, V.Number b) =>
              run (chunk, ip + 1, push (stack, V.Number (f (a, b))))
          | _ => runtimeError (chunk, ip, "Operands must be numbers.")
        end
    in
      if Debug.traceExecution then
        ( print "          "
        ; app (fn v => (print "[ "; V.print v; print " ]")) (rev stack)
        ; print "\n"
        ; ignore (Debug.disassembleInstruction (chunk, ip))
        )
      else
        ();

      case Chunk.getOpcode (chunk, ip) of
        Op.Constant =>
          run (chunk, ip + 2, push (stack, readConstant (chunk, ip + 1)))
      | Op.Nil => run (chunk, ip + 1, push (stack, V.Nil))
      | Op.True => run (chunk, ip + 1, push (stack, V.Boolean true))
      | Op.False => run (chunk, ip + 1, push (stack, V.Boolean false))
      | Op.Add => binaryOp op+
      | Op.Subtract => binaryOp op-
      | Op.Multiply => binaryOp op*
      | Op.Divide => binaryOp op/
      | Op.Not =>
          let val (v, stack) = pop stack
          in run (chunk, ip + 1, push (stack, V.Boolean (V.isFalsy v)))
          end
      | Op.Negate =>
          let
            val (v, stack) = pop stack
          in
            case v of
              V.Number n => run (chunk, ip + 1, push (stack, V.Number (~n)))
            | _ => runtimeError (chunk, ip, "Operand must be a number.")
          end
      | Op.Return => let val (v, _) = pop stack in V.print v; print "\n"; Ok end
    end

  fun interpret source =
    case Compiler.compile source of
      SOME chunk => run (chunk, 0, [])
    | NONE => CompileError
end
