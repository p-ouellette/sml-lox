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

  type parse_prec_fn = parser -> Chunk.t -> bool -> parser

  type local_ = {name: Token.t, depth: int}
  type compiler = {locals: local_ Array.array, localCount: int, scopeDepth: int}

  val dummyToken = {kind = T.Eof, lexeme = "", line = 0}

  fun newParser source =
    { scanner = Scanner.new source
    , current = dummyToken
    , previous = dummyToken
    , hadError = ref false
    , panicMode = ref false
    }

  fun newCompiler () =
    { locals = Array.array (256, {name = dummyToken, depth = 0})
    , localCount = 0
    , scopeDepth = 0
    }

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

  fun check (parser: parser, tokenKind) =
    T.kind (#current parser) = tokenKind

  fun match (parser, tokenKind) =
    if check (parser, tokenKind) then SOME (advance parser) else NONE

  fun synchronize parser =
    ( (#panicMode parser) := false
    ; case T.kind (#current parser) of
        T.Eof => parser
      | T.Class => parser
      | T.For => parser
      | T.Fun => parser
      | T.If => parser
      | T.Print => parser
      | T.Return => parser
      | T.Var => parser
      | T.While => parser
      | _ =>
          if T.kind (#previous parser) = T.Semicolon then parser
          else synchronize (advance parser)
    )

  fun emitByte (parser: parser, chunk) byte =
    Chunk.write (chunk, byte, T.line (#previous parser))

  fun emitOpcode args opcode =
    emitByte args (Op.encode opcode)

  fun emitConstInstr args (opcode, constant) =
    (emitOpcode args opcode; emitByte args constant)

  fun emitReturn args = emitOpcode args Op.Return

  fun makeConstant (parser, chunk) value =
    let
      val constant = Chunk.addConstant (chunk, value)
    in
      if constant > 255 then error (parser, "Too many constants in one chunk.")
      else ();
      Word8.fromInt constant
    end

  fun emitConstant args value =
    emitConstInstr args (Op.Constant, makeConstant args value)

  fun identifierConstant args name =
    makeConstant args (Value.String (T.lexeme name))

  fun parseVariable (parser, chunk) errorMessage =
    let
      val parser = consume (parser, T.Identifier, errorMessage)
      val constant = identifierConstant (parser, chunk) (#previous parser)
    in
      (parser, constant)
    end

  fun defineVariable args global = emitConstInstr args (Op.DefineGlobal, global)

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

  fun makeRule
    (prefix: parse_prec_fn option, infix_: parse_prec_fn option, precedence) =
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
    | T.BangEqual => makeRule (NONE, SOME binary, Prec.equality)
    | T.Equal => makeRule (NONE, NONE, Prec.none)
    | T.EqualEqual => makeRule (NONE, SOME binary, Prec.comparision)
    | T.Greater => makeRule (NONE, SOME binary, Prec.comparision)
    | T.GreaterEqual => makeRule (NONE, SOME binary, Prec.comparision)
    | T.Less => makeRule (NONE, SOME binary, Prec.comparision)
    | T.LessEqual => makeRule (NONE, SOME binary, Prec.comparision)
    | T.Identifier => makeRule (SOME variable, NONE, Prec.none)
    | T.String => makeRule (SOME string, NONE, Prec.none)
    | T.Number => makeRule (SOME number, NONE, Prec.none)
    | T.And => makeRule (NONE, NONE, Prec.none)
    | T.Class => makeRule (NONE, NONE, Prec.none)
    | T.Else => makeRule (NONE, NONE, Prec.none)
    | T.False => makeRule (SOME literal, NONE, Prec.none)
    | T.Fun => makeRule (NONE, NONE, Prec.none)
    | T.For => makeRule (NONE, NONE, Prec.none)
    | T.If => makeRule (NONE, NONE, Prec.none)
    | T.Nil => makeRule (SOME literal, NONE, Prec.none)
    | T.Or => makeRule (NONE, NONE, Prec.none)
    | T.Print => makeRule (NONE, NONE, Prec.none)
    | T.Return => makeRule (NONE, NONE, Prec.none)
    | T.Super => makeRule (NONE, NONE, Prec.none)
    | T.This => makeRule (NONE, NONE, Prec.none)
    | T.True => makeRule (SOME literal, NONE, Prec.none)
    | T.Var => makeRule (NONE, NONE, Prec.none)
    | T.While => makeRule (NONE, NONE, Prec.none)
    | T.Error => makeRule (NONE, NONE, Prec.none)
    | T.Eof => makeRule (NONE, NONE, Prec.none)

  and parsePrecedence parser chunk precedence =
    let
      val parser = advance parser
      val rule = getRule (T.kind (#previous parser))
      val canAssign = precedence <= Prec.assignment
      val parser =
        case #prefix rule of
          SOME prefixRule => prefixRule parser chunk canAssign
        | NONE => (error (parser, "Expect expression."); parser)
      fun parseInfix parser =
        let
          val rule = getRule (T.kind (#current parser))
        in
          if precedence <= #precedence rule then
            case #infix_ rule of
              SOME infixRule =>
                parseInfix (infixRule (advance parser) chunk canAssign)
            | NONE => raise Fail "missing infix rule"
          else
            parser
        end
      val parser = parseInfix parser
    in
      if canAssign andalso check (parser, T.Equal) then
        errorAtCurrent (parser, "Invalid assignment target.")
      else
        ();
      parser
    end

  and program parser chunk =
    case match (parser, T.Eof) of
      SOME parser' => parser'
    | NONE => program (declaration parser chunk) chunk

  and declaration parser chunk =
    let
      val parser =
        case match (parser, T.Var) of
          SOME parser' => varDeclaration parser' chunk
        | NONE => statement parser chunk
    in
      if panicMode parser then synchronize parser else parser
    end

  and varDeclaration parser chunk =
    let
      val (parser, global) =
        parseVariable (parser, chunk) "Expect variable name."
      val parser =
        case match (parser, T.Equal) of
          SOME parser' => expression parser' chunk
        | NONE => (emitOpcode (parser, chunk) Op.Nil; parser)
      val parser = consume
        (parser, T.Semicolon, "Expect ';' after variable declaration.")
    in
      defineVariable (parser, chunk) global;
      parser
    end

  and statement parser chunk =
    case match (parser, T.Print) of
      SOME parser' => printStatement parser' chunk
    | NONE => expressionStatement parser chunk

  and expressionStatement parser chunk =
    let
      val parser = expression parser chunk
      val parser = consume (parser, T.Semicolon, "Expect ';' after expression.")
    in
      emitOpcode (parser, chunk) Op.Pop;
      parser
    end

  and printStatement parser chunk =
    let
      val parser = expression parser chunk
      val parser = consume (parser, T.Semicolon, "Expect ';' after value.")
    in
      emitOpcode (parser, chunk) Op.Print;
      parser
    end

  and expression parser chunk =
    parsePrecedence parser chunk Prec.assignment

  and binary parser chunk _ =
    let
      val emitOp = emitOpcode (parser, chunk)
      val operatorKind = T.kind (#previous parser)
      val rule = getRule operatorKind
      val parser' = parsePrecedence parser chunk (#precedence rule + 1)
    in
      case operatorKind of
        T.BangEqual => (emitOp Op.Equal; emitOp Op.Not)
      | T.EqualEqual => emitOp Op.Equal
      | T.Greater => emitOp Op.Greater
      | T.GreaterEqual => (emitOp Op.Less; emitOp Op.Not)
      | T.Less => emitOp Op.Less
      | T.LessEqual => (emitOp Op.Greater; emitOp Op.Not)
      | T.Plus => emitOp Op.Add
      | T.Minus => emitOp Op.Subtract
      | T.Star => emitOp Op.Multiply
      | T.Slash => emitOp Op.Divide
      | _ => raise Fail "expected binary operator";
      parser'
    end

  and unary parser chunk _ =
    let
      val operatorKind = T.kind (#previous parser)
      val parser' = parsePrecedence parser chunk Prec.unary
    in
      case operatorKind of
        T.Bang => emitOpcode (parser, chunk) Op.Not
      | T.Minus => emitOpcode (parser, chunk) Op.Negate
      | _ => raise Fail "expected unary operator";
      parser'
    end

  and number parser chunk _ =
    let val n = valOf (Real.fromString (T.lexeme (#previous parser)))
    in emitConstant (parser, chunk) (Value.Number n); parser
    end

  and string parser chunk _ =
    let
      val lexeme = T.lexeme (#previous parser)
      val s = String.substring (lexeme, 1, size lexeme - 2)
    in
      emitConstant (parser, chunk) (Value.String s);
      parser
    end

  and variable parser chunk canAssign =
    namedVariable (parser, chunk) (#previous parser, canAssign)

  and namedVariable (args as (parser, chunk)) (name, canAssign) =
    let
      val constant = identifierConstant args name
    in
      if canAssign andalso check (parser, T.Equal) then
        let val parser = expression (advance parser) chunk
        in emitConstInstr (parser, chunk) (Op.SetGlobal, constant); parser
        end
      else
        (emitConstInstr (parser, chunk) (Op.GetGlobal, constant); parser)
    end

  and literal parser chunk _ =
    let
      val emitOp = emitOpcode (parser, chunk)
    in
      case T.kind (#previous parser) of
        T.Nil => emitOp Op.Nil
      | T.True => emitOp Op.True
      | T.False => emitOp Op.False
      | _ => raise Fail "expected literal";
      parser
    end

  and grouping parser chunk _ =
    consume
      (expression parser chunk, T.RightParen, "Expect ')' after expression.")

  fun compile source =
    let
      val compiler = newCompiler ()
      val chunk = Chunk.new ()
      val parser = newParser source
      val parser = program (advance parser) chunk
    in
      emitReturn (parser, chunk);
      if Debug.printCode then Debug.disassembleChunk (chunk, "code") else ();
      if hadError parser then NONE else SOME chunk
    end
end
