structure Lox:
sig
  val run: string * Environment.t -> Environment.t
  val runFile: string -> Word8.word
  val runPrompt: Environment.t -> Word8.word
  val main: string list -> Word8.word
end =
struct
  structure TIO = TextIO

  structure Status =
  struct
    val success = 0w0 : Word8.word
    val usageError = 0w64 : Word8.word
    val parseError = 0w65 : Word8.word
    val runtimeError = 0w70 : Word8.word
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
      run (prog, Environment.global ());

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
        runPrompt (Environment.global ())
    | main [fname] = runFile fname
    | main _ =
        (print "Usage: lox [script]"; Status.usageError)
end
