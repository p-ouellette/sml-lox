structure Main:
sig
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

  fun runFile fname =
    let
      val strm = TIO.openIn fname
      val prog = TIO.inputAll strm
      val () = TIO.closeIn strm
      val (result, _) = VM.interpret (prog, Globals.new ())
    in
      case result of
        VM.Ok => Status.success
      | VM.CompileError => Status.parseError
      | VM.RuntimeError => Status.runtimeError
    end

  fun runPrompt env =
    ( print "> "
    ; case TIO.inputLine TIO.stdIn of
        NONE => (print "\n"; Status.success)
      | SOME s => let val (_, env) = VM.interpret (s, env) in runPrompt env end
    )

  fun main [] =
        runPrompt (Globals.new ())
    | main [fname] = runFile fname
    | main _ =
        (print "Usage: bclox [script]"; Status.usageError)
end
