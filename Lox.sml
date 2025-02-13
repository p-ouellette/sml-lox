structure Lox:
sig
  val run: string * Environment.t -> Environment.t
  val runFile: string -> OS.Process.status
  val runPrompt: unit -> OS.Process.status
  val main: string list -> OS.Process.status
end =
struct
  structure TIO = TextIO

  fun run (prog, env) =
    let
      val tokens = Scanner.scanTokens prog
      val statements = Parser.parse tokens
    in
      Interpreter.interpret (statements, env)
    end
    handle Error.ParserError => env

  fun runFile fname =
    let
      val strm = TIO.openIn fname
      val prog = TIO.inputAll strm
    in
      TIO.closeIn strm;
      run (prog, Environment.empty);
      if ! Error.hadError orelse ! Error.hadRuntimeError then OS.Process.failure
      else OS.Process.success
    end

  fun runPrompt () =
    let
      fun loop env =
        ( print "> "
        ; case TIO.inputLine TIO.stdIn of
            NONE => (print "\n"; OS.Process.success)
          | SOME s =>
              let val env' = run (s, env)
              (* XXX: resetting hadError is pointless because we don't
               * check it in run *)
              in Error.hadError := false; loop env'
              end
        )
    in
      loop Environment.empty
    end

  fun main [] = runPrompt ()
    | main [fname] = runFile fname
    | main _ =
        (print "Usage: lox [script]"; OS.Process.failure)
end

fun main () =
  OS.Process.exit (Lox.main (CommandLine.arguments ()))
