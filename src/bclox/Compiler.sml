structure Compiler:
sig
  exception Error

  val compile: string -> Chunk.t
end =
struct
  structure TIO = TextIO
  structure Op = Opcode

  exception Error

  type parser =
    { cs: Scanner.cs
    , current: Token.t
    , previous: Token.t
    , hadError: bool ref
    , panicMode: bool ref
    }

  fun newParser source =
    let
      val dummy = {kind = Token.EOF, lexeme = "", line = 0}
    in
      { cs = Scanner.new source
      , current = dummy
      , previous = dummy
      , hadError = ref false
      , panicMode = ref false
      }
    end

  fun hadError (parser: parser) =
    !(#hadError parser)

  fun panicMode (parser: parser) =
    !(#panicMode parser)

  fun eprint s =
    (TIO.output (TIO.stdErr, s); TIO.flushOut TIO.stdErr)

  fun errorAt (parser: parser, token: Token.t, message) =
    if panicMode parser then
      ()
    else
      ( (#panicMode parser) := true
      ; eprint ("[line " ^ Int.toString (#line token) ^ "] Error")
      ; case #kind token of
          Token.EOF => eprint " at end"
        | Token.ERROR => ()
        | _ => eprint (" at '" ^ #lexeme token ^ "'")
      ; eprint (": " ^ message ^ "\n")
      ; (#hadError parser) := true
      )

  fun error (parser, message) =
    errorAt (parser, #previous parser, message)

  fun errorAtCurrent (parser, message) =
    errorAt (parser, #current parser, message)

  fun advance (parser: parser) =
    let
      val previous = #current parser
      fun loop (parser: parser) =
        let
          val (current, cs) = Scanner.scan (#cs parser)
          val parser =
            { cs = cs
            , current = current
            , previous = previous
            , hadError = #hadError parser
            , panicMode = #panicMode parser
            }
        in
          if #kind current = Token.ERROR then loop parser else parser
        end
    in
      loop parser
    end

  fun consume (parser, tokenKind, message) =
    if #kind (#current parser) = tokenKind then advance parser
    else (errorAtCurrent (parser, message); parser)

  fun compile source =
    let
      val chunk = Chunk.new ()

      fun emitByte (parser: parser, byte) =
        Chunk.write (chunk, byte, #line (#previous parser))

      fun emitBytes (parser, byte1, byte2) =
        (emitByte (parser, byte1); emitByte (parser, byte2))

      fun emitReturn parser =
        emitByte (parser, Op.encode Op.RETURN)

      fun makeConstant (parser, value) =
        let
          val constant = Chunk.addConstant (chunk, value)
        in
          if constant > 255 then
            error (parser, "Too many constants in one chunk.")
          else
            ();
          Word8.fromInt constant
        end

      fun emitConstant (parser, value) =
        emitBytes (parser, Op.encode Op.CONSTANT, makeConstant (parser, value))

      fun number (parser: parser) =
        let val value = valOf (Real.fromString (#lexeme (#previous parser)))
        in emitConstant (parser, value)
        end

      val parser = newParser source
      val parser = advance parser
      val parser = expression parser
      val parser = consume (parser, Token.EOF, "Expect end of expression.")
    in
      emitReturn parser;
      if hadError parser then raise Error else chunk
    end
end
