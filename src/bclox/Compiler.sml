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
      val dummy = {kind = T.EOF, lexeme = "", line = 0}
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
          T.EOF => eprint " at end"
        | T.ERROR => ()
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
          if T.kind current = T.ERROR then loop parser else parser
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
    emitByte (chunk, parser, Op.encode Op.RETURN)

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
    in emitBytes (chunk, parser, Op.encode Op.CONSTANT, constant)
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
      T.LEFT_PAREN => makeRule (SOME grouping, NONE, Prec.none)
    | T.RIGHT_PAREN => makeRule (NONE, NONE, Prec.none)
    | T.LEFT_BRACE => makeRule (NONE, NONE, Prec.none)
    | T.RIGHT_BRACE => makeRule (NONE, NONE, Prec.none)
    | T.COMMA => makeRule (NONE, NONE, Prec.none)
    | T.DOT => makeRule (NONE, NONE, Prec.none)
    | T.MINUS => makeRule (SOME unary, SOME binary, Prec.term)
    | T.PLUS => makeRule (NONE, SOME binary, Prec.term)
    | T.SEMICOLON => makeRule (NONE, NONE, Prec.none)
    | T.SLASH => makeRule (NONE, SOME binary, Prec.factor)
    | T.STAR => makeRule (NONE, SOME binary, Prec.factor)
    | T.BANG => makeRule (NONE, NONE, Prec.none)
    | T.BANG_EQUAL => makeRule (NONE, NONE, Prec.none)
    | T.EQUAL => makeRule (NONE, NONE, Prec.none)
    | T.EQUAL_EQUAL => makeRule (NONE, NONE, Prec.none)
    | T.GREATER => makeRule (NONE, NONE, Prec.none)
    | T.GREATER_EQUAL => makeRule (NONE, NONE, Prec.none)
    | T.LESS => makeRule (NONE, NONE, Prec.none)
    | T.LESS_EQUAL => makeRule (NONE, NONE, Prec.none)
    | T.IDENTIFIER => makeRule (NONE, NONE, Prec.none)
    | T.STRING => makeRule (NONE, NONE, Prec.none)
    | T.NUMBER => makeRule (SOME number, NONE, Prec.none)
    | T.AND => makeRule (NONE, NONE, Prec.none)
    | T.CLASS => makeRule (NONE, NONE, Prec.none)
    | T.ELSE => makeRule (NONE, NONE, Prec.none)
    | T.FALSE => makeRule (SOME false_, NONE, Prec.none)
    | T.FUN => makeRule (NONE, NONE, Prec.none)
    | T.FOR => makeRule (NONE, NONE, Prec.none)
    | T.IF => makeRule (NONE, NONE, Prec.none)
    | T.NIL => makeRule (SOME nil_, NONE, Prec.none)
    | T.OR => makeRule (NONE, NONE, Prec.none)
    | T.PRINT => makeRule (NONE, NONE, Prec.none)
    | T.RETURN => makeRule (NONE, NONE, Prec.none)
    | T.SUPER => makeRule (NONE, NONE, Prec.none)
    | T.THIS => makeRule (NONE, NONE, Prec.none)
    | T.TRUE => makeRule (SOME true_, NONE, Prec.none)
    | T.VAR => makeRule (NONE, NONE, Prec.none)
    | T.WHILE => makeRule (NONE, NONE, Prec.none)
    | T.ERROR => makeRule (NONE, NONE, Prec.none)
    | T.EOF => makeRule (NONE, NONE, Prec.none)

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
        T.PLUS => emitByte (chunk, parser, Op.encode Op.ADD)
      | T.MINUS => emitByte (chunk, parser, Op.encode Op.SUBTRACT)
      | T.STAR => emitByte (chunk, parser, Op.encode Op.MULTIPLY)
      | T.SLASH => emitByte (chunk, parser, Op.encode Op.DIVIDE)
      | _ => raise Fail "expected binary operator";
      parser'
    end

  and unary (parser, chunk) =
    let
      val operatorKind = T.kind (#previous parser)
      val parser' = parsePrecedence (parser, chunk, Prec.unary)
    in
      case operatorKind of
        T.MINUS => emitByte (chunk, parser, Op.encode Op.NEGATE)
      | _ => raise Fail "expected unary operator";
      parser'
    end

  and number (parser, chunk) =
    let val n = valOf (Real.fromString (T.lexeme (#previous parser)))
    in emitConstant (chunk, parser, Value.Number n); parser
    end

  and nil_ (parser, chunk) =
    (emitByte (chunk, parser, Op.encode Op.NIL); parser)

  and true_ (parser, chunk) =
    (emitByte (chunk, parser, Op.encode Op.TRUE); parser)

  and false_ (parser, chunk) =
    (emitByte (chunk, parser, Op.encode Op.FALSE); parser)

  and grouping args =
    consume (expression args, T.RIGHT_PAREN, "Expect ')' after expression.")

  fun compile source =
    let
      val chunk = Chunk.new ()
      val parser = advance (newParser source)
      val parser = expression (parser, chunk)
      val parser = consume (parser, T.EOF, "Expect end of expression.")
    in
      emitReturn (chunk, parser);
      if Debug.printCode then Debug.disassembleChunk (chunk, "code") else ();
      if hadError parser then NONE else SOME chunk
    end
end
