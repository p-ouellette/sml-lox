structure Main:
sig
  val run: string * Value.t Environment.t -> Value.t Environment.t
  val runFile: string -> Word8.word
  val runPrompt: Value.t Environment.t -> Word8.word
  val main: string list -> Word8.word
end =
struct
  structure TIO = TextIO

  structure Status =
  struct
    val success: Word8.word = 0w0
    val usageError: Word8.word = 0w64
    val parseError: Word8.word = 0w65
    val runtimeError: Word8.word = 0w70
  end

  fun run (prog, env) =
    let
      val tokens = Scanner.scanTokens prog
      val statements = Parser.parse tokens
    in
      if ! Error.hadError then env else (Resolver.resolve statements; env);
      if ! Error.hadError then env else Interpreter.interpret (statements, env)
    end
    handle Error.ParserError _ => env

  fun runFile fname =
    let
      val strm = TIO.openIn fname
      val prog = TIO.inputAll strm
    in
      TIO.closeIn strm;
      run (prog, Interpreter.baseEnv ());

      if ! Error.hadError then Status.parseError
      else if ! Error.hadRuntimeError then Status.runtimeError
      else Status.success
    end

  fun runPrompt env =
    ( print "> "
    ; case TIO.inputLine TIO.stdIn of
        NONE => (print "\n"; Status.success)
      | SOME s => runPrompt (run (s, env) before Error.hadError := false)
    )

  fun main [] =
        runPrompt (Interpreter.baseEnv ())
    | main [fname] = runFile fname
    | main _ =
        (print "Usage: treelox [script]"; Status.usageError)
end
