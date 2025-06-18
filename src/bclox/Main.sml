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
    in
      TIO.closeIn strm;
      case VM.interpret prog of
        VM.OK => Status.success
      | VM.COMPILE_ERROR => Status.parseError
      | VM.RUNTIME_ERROR => Status.runtimeError
    end

  fun runPrompt () =
    ( print "> "
    ; case TIO.inputLine TIO.stdIn of
        NONE => (print "\n"; Status.success)
      | SOME s => (VM.interpret s; runPrompt ())
    )

  fun main [] = runPrompt ()
    | main [fname] = runFile fname
    | main _ =
        (print "Usage: bclox [script]"; Status.usageError)
end
