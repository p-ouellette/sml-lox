structure Lox:
sig
  val run: string -> unit
  val runFile: string -> OS.Process.status
  val runPrompt: unit -> OS.Process.status
  val main: string list -> OS.Process.status
end =
struct
  structure TIO = TextIO

  fun run prog =
    let val tokens = Scanner.scanTokens prog
    in app (fn t => print (Token.toString t ^ "\n")) tokens
    end

  fun runFile fname =
    let
      val strm = TIO.openIn fname
      val prog = TIO.inputAll strm
    in
      TIO.closeIn strm;
      run prog;
      if ! Error.hadError then OS.Process.failure else OS.Process.success
    end

  fun runPrompt () =
    ( print "> "
    ; case TIO.inputLine TIO.stdIn of
        NONE => (print "\n"; OS.Process.success)
      | SOME s => (run s; Error.hadError := false; runPrompt ())
    )

  fun main [] = runPrompt ()
    | main [fname] = runFile fname
    | main _ =
        (print "Usage: lox [script]"; OS.Process.failure)
end
