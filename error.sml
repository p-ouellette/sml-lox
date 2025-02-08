structure Error:
sig
  val hadError: bool ref

  val errorAt: SourceToken.t * string -> unit

  val error: int * string -> unit
end =
struct
  structure TIO = TextIO

  val hadError = ref false

  fun report (line, where_, msg) =
    ( app (fn s => TIO.output (TIO.stdErr, s))
        ["[line ", Int.toString line, "] Error", where_, ": ", msg, "\n"]
    ; hadError := true
    )

  fun error (line, msg) = report (line, "", msg)

  fun errorAt ({token = Token.EOF, line, lexeme = _}, msg) =
        report (line, " at end", msg)
    | errorAt ({line, lexeme, ...}, msg) =
        report (line, " at '" ^ lexeme ^ "'", msg)
end
