structure VM:
sig
  datatype result = Ok | CompileError | RuntimeError

  val interpret: string * Globals.t -> result * Globals.t
end =
struct
  structure Op = Opcode
  structure V = Value

  datatype result = Ok | CompileError | RuntimeError

  fun push (stack, value) = value :: stack

  fun pop [] = raise Empty
    | pop (v :: stack) = (v, stack)

  fun peek [] = raise Empty
    | peek (v :: _) = v

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

  fun readConstant (chunk, ip) =
    Chunk.getConstant (chunk, Word8.toInt (Chunk.sub (chunk, ip + 1)))

  fun readString (chunk, ip) =
    case readConstant (chunk, ip) of
      V.String s => s
    | _ => raise Fail "expected string constant"

  fun run (chunk, ip, stack, globals) =
    let
      fun continue (offset, stack, globals) =
        run (chunk, ip + offset, stack, globals)

      fun binaryOp (cons, f) =
        let
          val (b, stack) = pop stack
          val (a, stack) = pop stack
        in
          case (a, b) of
            (V.Number a, V.Number b) =>
              continue (1, push (stack, cons (f (a, b))), globals)
          | _ =>
              (runtimeError (chunk, ip, "Operands must be numbers."), globals)
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
          continue (2, push (stack, readConstant (chunk, ip)), globals)
      | Op.Nil => continue (1, push (stack, V.Nil), globals)
      | Op.True => continue (1, push (stack, V.Boolean true), globals)
      | Op.False => continue (1, push (stack, V.Boolean false), globals)
      | Op.Pop =>
          let val (_, stack) = pop stack
          in continue (1, stack, globals)
          end
      | Op.GetGlobal =>
          let
            val name = readString (chunk, ip)
          in
            case Globals.find (globals, name) of
              SOME value => continue (2, push (stack, value), globals)
            | NONE =>
                let val message = "Undefined variable '" ^ name ^ "'."
                in (runtimeError (chunk, ip, message), globals)
                end
          end
      | Op.DefineGlobal =>
          let
            val name = readString (chunk, ip)
            val (v, stack) = pop stack
            val globals = Globals.insert (globals, name, v)
          in
            continue (2, stack, globals)
          end
      | Op.SetGlobal =>
          let
            val name = readString (chunk, ip)
          in
            if Globals.defined (globals, name) then
              continue (2, stack, Globals.insert (globals, name, peek stack))
            else
              let val message = "Undefined variable '" ^ name ^ "'."
              in (runtimeError (chunk, ip, message), globals)
              end
          end
      | Op.Equal =>
          let
            val (b, stack) = pop stack
            val (a, stack) = pop stack
          in
            continue (1, push (stack, V.Boolean (V.isEqual (a, b))), globals)
          end
      | Op.Greater => binaryOp (V.Boolean, op>)
      | Op.Less => binaryOp (V.Boolean, op<)
      | Op.Add =>
          let
            val (b, stack) = pop stack
            val (a, stack) = pop stack
          in
            case (a, b) of
              (V.String a, V.String b) =>
                continue (1, push (stack, V.String (concat [a, b])), globals)
            | (V.Number a, V.Number b) =>
                continue (1, push (stack, V.Number (a + b)), globals)
            | _ =>
                let val message = "Operands must be two numbers or two strings."
                in (runtimeError (chunk, ip, message), globals)
                end
          end
      | Op.Subtract => binaryOp (V.Number, op-)
      | Op.Multiply => binaryOp (V.Number, op*)
      | Op.Divide => binaryOp (V.Number, op/)
      | Op.Not =>
          let val (v, stack) = pop stack
          in continue (1, push (stack, V.Boolean (V.isFalsy v)), globals)
          end
      | Op.Negate =>
          let
            val (v, stack) = pop stack
          in
            case v of
              V.Number n => continue (1, push (stack, V.Number (~n)), globals)
            | _ =>
                (runtimeError (chunk, ip, "Operand must be a number."), globals)
          end
      | Op.Print =>
          let val (v, stack) = pop stack
          in V.print v; print "\n"; continue (1, stack, globals)
          end
      | Op.Return => (Ok, globals)
    end

  fun interpret (source, globals) =
    case Compiler.compile source of
      SOME chunk => run (chunk, 0, [], globals)
    | NONE => (CompileError, globals)
end
