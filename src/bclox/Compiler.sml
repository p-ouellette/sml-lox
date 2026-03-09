structure Compiler:
sig
  val compile: string -> Chunk.t option
end =
struct
  structure T = Token
  structure Op = Opcode

  type parser =
    { scanner: Scanner.t
    , current: T.t
    , previous: T.t
    , hadError: bool ref
    , panicMode: bool ref
    }

  type parse_fn = parser * Chunk.t -> parser

  fun newParser source =
    let
      val dummy = {kind = T.Eof, lexeme = "", line = 0}
    in
      { scanner = Scanner.new source
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
    (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr)

  fun errorAt (parser: parser, token: T.t, message) =
    if panicMode parser then
      ()
    else
      ( (#panicMode parser) := true
      ; eprint ("[line " ^ Int.toString (T.line token) ^ "] Error")
      ; case T.kind token of
          T.Eof => eprint " at end"
        | T.Error => ()
        | _ => eprint (" at '" ^ T.lexeme token ^ "'")
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
          val (current, scanner) = Scanner.scan (#scanner parser)
          val parser =
            { scanner = scanner
            , current = current
            , previous = previous
            , hadError = #hadError parser
            , panicMode = #panicMode parser
            }
        in
          if T.kind current = T.Error then loop parser else parser
        end
    in
      loop parser
    end

  fun consume (parser, tokenKind, message) =
    if T.kind (#current parser) = tokenKind then advance parser
    else (errorAtCurrent (parser, message); parser)

  fun emitByte (chunk, parser: parser, byte) =
    Chunk.write (chunk, byte, T.line (#previous parser))

  fun emitBytes (chunk, parser, byte1, byte2) =
    (emitByte (chunk, parser, byte1); emitByte (chunk, parser, byte2))

  fun emitReturn (chunk, parser) =
    emitByte (chunk, parser, Op.encode Op.Return)

  fun makeConstant (chunk, parser, value) =
    let
      val constant = Chunk.addConstant (chunk, value)
    in
      if constant > 255 then error (parser, "Too many constants in one chunk.")
      else ();
      Word8.fromInt constant
    end

  fun emitConstant (chunk, parser, value) =
    let val constant = makeConstant (chunk, parser, value)
    in emitBytes (chunk, parser, Op.encode Op.Constant, constant)
    end

  structure Precedence =
  struct
    val none = 0
    val assignment = 1 (* = *)
    val or_ = 2 (* or *)
    val and_ = 3 (* and *)
    val equality = 4 (* == != *)
    val comparision = 5 (* < > <= >= *)
    val term = 6 (* + - *)
    val factor = 7 (* * / *)
    val unary = 8 (* ! - *)
    val call = 9 (* . () *)
    val primary = 10
  end
  structure Prec = Precedence

  fun makeRule (prefix: parse_fn option, infix_: parse_fn option, precedence) =
    {prefix = prefix, infix_ = infix_, precedence = precedence}

  fun getRule tokenKind =
    case tokenKind of
      T.LeftParen => makeRule (SOME grouping, NONE, Prec.none)
    | T.RightParen => makeRule (NONE, NONE, Prec.none)
    | T.LeftBrace => makeRule (NONE, NONE, Prec.none)
    | T.RightBrace => makeRule (NONE, NONE, Prec.none)
    | T.Comma => makeRule (NONE, NONE, Prec.none)
    | T.Dot => makeRule (NONE, NONE, Prec.none)
    | T.Minus => makeRule (SOME unary, SOME binary, Prec.term)
    | T.Plus => makeRule (NONE, SOME binary, Prec.term)
    | T.Semicolon => makeRule (NONE, NONE, Prec.none)
    | T.Slash => makeRule (NONE, SOME binary, Prec.factor)
    | T.Star => makeRule (NONE, SOME binary, Prec.factor)
    | T.Bang => makeRule (SOME unary, NONE, Prec.none)
    | T.BangEqual => makeRule (NONE, NONE, Prec.none)
    | T.Equal => makeRule (NONE, NONE, Prec.none)
    | T.EqualEqual => makeRule (NONE, NONE, Prec.none)
    | T.Greater => makeRule (NONE, NONE, Prec.none)
    | T.GreaterEqual => makeRule (NONE, NONE, Prec.none)
    | T.Less => makeRule (NONE, NONE, Prec.none)
    | T.LessEqual => makeRule (NONE, NONE, Prec.none)
    | T.Identifier => makeRule (NONE, NONE, Prec.none)
    | T.String => makeRule (NONE, NONE, Prec.none)
    | T.Number => makeRule (SOME number, NONE, Prec.none)
    | T.And => makeRule (NONE, NONE, Prec.none)
    | T.Class => makeRule (NONE, NONE, Prec.none)
    | T.Else => makeRule (NONE, NONE, Prec.none)
    | T.False => makeRule (SOME false_, NONE, Prec.none)
    | T.Fun => makeRule (NONE, NONE, Prec.none)
    | T.For => makeRule (NONE, NONE, Prec.none)
    | T.If => makeRule (NONE, NONE, Prec.none)
    | T.Nil => makeRule (SOME nil_, NONE, Prec.none)
    | T.Or => makeRule (NONE, NONE, Prec.none)
    | T.Print => makeRule (NONE, NONE, Prec.none)
    | T.Return => makeRule (NONE, NONE, Prec.none)
    | T.Super => makeRule (NONE, NONE, Prec.none)
    | T.This => makeRule (NONE, NONE, Prec.none)
    | T.True => makeRule (SOME true_, NONE, Prec.none)
    | T.Var => makeRule (NONE, NONE, Prec.none)
    | T.While => makeRule (NONE, NONE, Prec.none)
    | T.Error => makeRule (NONE, NONE, Prec.none)
    | T.Eof => makeRule (NONE, NONE, Prec.none)

  and parsePrecedence (parser, chunk, precedence) =
    let
      val parser = advance parser
      val rule = getRule (T.kind (#previous parser))
      val parser =
        case #prefix rule of
          SOME prefixRule => prefixRule (parser, chunk)
        | NONE => (error (parser, "Expect expression."); parser)
      fun parseInfix parser =
        let
          val rule = getRule (T.kind (#current parser))
        in
          if precedence <= #precedence rule then
            case #infix_ rule of
              SOME infixRule => parseInfix (infixRule (advance parser, chunk))
            | NONE => raise Fail "missing infix rule"
          else
            parser
        end
    in
      parseInfix parser
    end

  and expression (parser, chunk) =
    parsePrecedence (parser, chunk, Prec.assignment)

  and binary (parser, chunk) =
    let
      val operatorKind = T.kind (#previous parser)
      val rule = getRule operatorKind
      val parser' = parsePrecedence (parser, chunk, #precedence rule + 1)
    in
      case operatorKind of
        T.Plus => emitByte (chunk, parser, Op.encode Op.Add)
      | T.Minus => emitByte (chunk, parser, Op.encode Op.Subtract)
      | T.Star => emitByte (chunk, parser, Op.encode Op.Multiply)
      | T.Slash => emitByte (chunk, parser, Op.encode Op.Divide)
      | _ => raise Fail "expected binary operator";
      parser'
    end

  and unary (parser, chunk) =
    let
      val operatorKind = T.kind (#previous parser)
      val parser' = parsePrecedence (parser, chunk, Prec.unary)
    in
      case operatorKind of
        T.Bang => emitByte (chunk, parser, Op.encode Op.Not)
      | T.Minus => emitByte (chunk, parser, Op.encode Op.Negate)
      | _ => raise Fail "expected unary operator";
      parser'
    end

  and number (parser, chunk) =
    let val n = valOf (Real.fromString (T.lexeme (#previous parser)))
    in emitConstant (chunk, parser, Value.Number n); parser
    end

  and nil_ (parser, chunk) =
    (emitByte (chunk, parser, Op.encode Op.Nil); parser)

  and true_ (parser, chunk) =
    (emitByte (chunk, parser, Op.encode Op.True); parser)

  and false_ (parser, chunk) =
    (emitByte (chunk, parser, Op.encode Op.False); parser)

  and grouping args =
    consume (expression args, T.RightParen, "Expect ')' after expression.")

  fun compile source =
    let
      val chunk = Chunk.new ()
      val parser = advance (newParser source)
      val parser = expression (parser, chunk)
      val parser = consume (parser, T.Eof, "Expect end of expression.")
    in
      emitReturn (chunk, parser);
      if Debug.printCode then Debug.disassembleChunk (chunk, "code") else ();
      if hadError parser then NONE else SOME chunk
    end
end
