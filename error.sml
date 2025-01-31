structure Error:
sig
  val hadError: bool ref

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
end
