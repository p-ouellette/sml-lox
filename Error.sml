structure Error:
sig
  (* holds the unconsumed input *)
  exception ParserError of SourceToken.t list
  (* holds the token where the error occurred and an error message *)
  exception RuntimeError of SourceToken.t * string

  val hadError: bool ref
  val hadRuntimeError: bool ref

  val error: int * string -> unit
  val errorAt: SourceToken.t * string -> unit
  val runtimeError: SourceToken.t * string -> unit
end =
struct
  exception ParserError of SourceToken.t list
  exception RuntimeError of SourceToken.t * string

  val hadError = ref false
  val hadRuntimeError = ref false

  fun sayErr s = TextIO.output (TextIO.stdErr, s)

  fun report (line, where_, msg) =
    ( app sayErr
        ["[line ", Int.toString line, "] Error", where_, ": ", msg, "\n"]
    ; hadError := true
    )

  fun error (line, msg) = report (line, "", msg)

  fun errorAt ({token = Token.EOF, line, lexeme = _}, msg) =
        report (line, " at end", msg)
    | errorAt ({line, lexeme, ...}, msg) =
        report (line, " at '" ^ lexeme ^ "'", msg)

  fun runtimeError ({line, ...}: SourceToken.t, msg) =
    ( app sayErr [msg, "\n[line ", Int.toString line, "]\n"]
    ; hadRuntimeError := true
    )
end
