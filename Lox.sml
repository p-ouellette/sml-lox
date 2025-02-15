structure Lox:
sig
  val run: string * Environment.t -> Environment.t
  val runFile: string -> Word8.word
  val runPrompt: unit -> Word8.word
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
      Interpreter.interpret (statements, env)
    end
    handle Error.ParserError _ => env

  fun runFile fname =
    let
      val strm = TIO.openIn fname
      val prog = TIO.inputAll strm
    in
      TIO.closeIn strm;
      run (prog, Environment.empty);

      if ! Error.hadError then Status.parseError
      else if ! Error.hadRuntimeError then Status.runtimeError
      else Status.success
    end

  fun runPrompt () =
    let
      fun loop env =
        ( print "> "
        ; case TIO.inputLine TIO.stdIn of
            NONE => (print "\n"; Status.success)
          | SOME s =>
              let val env' = run (s, env)
              (* XXX: no need to reset hadError if we don't check it in run *)
              in Error.hadError := false; loop env'
              end
        )
    in
      loop Environment.empty
    end

  fun main [] = runPrompt ()
    | main [fname] = runFile fname
    | main _ =
        (print "Usage: lox [script]"; Status.usageError)
end
