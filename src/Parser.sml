structure Parser:
sig
  val parse: SourceToken.t list -> Stmt.t list
end =
struct
  structure T = Token

  fun error (st, msg, sts) =
    (Error.errorAt (st, msg); Error.ParserError sts)

  fun advance ([]: SourceToken.t list) = raise Fail "no more tokens"
    | advance (st :: sts) = (st, sts)

  fun match (tokens, sts) =
    let
      val (st, sts') = advance sts
    in
      if List.exists (fn t => Token.sameType (t, #token st)) tokens then
        SOME (st, sts')
      else
        NONE
    end

  fun check (tokens, sts) =
    Option.isSome (match (tokens, sts))

  fun consume (token, msg) sts =
    let
      val r as (st, _) = advance sts
    in
      if Token.sameType (token, #token st) then r
      else raise error (st, msg, sts)
    end

  fun syncronize sts =
    let
      val ({token, ...}, sts') = advance sts
    in
      (* stop after semicolon or at start of statement *)
      case token of
        T.EOF => sts
      | T.SEMICOLON => sts'
      | T.CLASS => sts
      | T.FOR => sts
      | T.FUN => sts
      | T.IF => sts
      | T.PRINT => sts
      | T.RETURN => sts
      | T.VAR => sts
      | T.WHILE => sts
      | _ => syncronize sts'
    end

  (* binary -> operand ( operator operand )* *)
  fun binaryOp cons (operators, operand) sts =
    let
      fun tail (left, sts) =
        case match (operators, sts) of
          SOME (operator, sts) =>
            let val (right, sts) = operand sts
            in tail (cons (left, operator, right), sts)
            end
        | NONE => (left, sts)
      val (left, sts) = operand sts
      val (expr, sts) = tail (left, sts)
    in
      (expr, sts)
    end

  val binary = binaryOp Expr.Binary
  val logical = binaryOp Expr.Logical

  (* program -> declaration* EOF *)
  fun program (decs, sts) =
    if check ([T.EOF], sts) then rev decs
    else program (maybeDeclaration (decs, sts))

  (* parse a declaration into decs or recover from an error *)
  and maybeDeclaration (decs, sts) =
    let val (dec, sts') = declaration sts
    in (dec :: decs, sts')
    end
    handle Error.ParserError sts' => (decs, syncronize sts')

  (* declaration -> varDecl | statement *)
  and declaration sts =
    case match ([T.VAR], sts) of
      SOME (_, sts') => varDeclaration sts'
    | NONE => statement sts

  (* varDecl -> "var" IDENTIFIER ( "=" expression )? ";" *)
  and varDeclaration sts =
    let
      val (name, sts) = consume (T.IDENTIFIER, "Expect variable name.") sts
      val (initializer, sts) =
        case match ([T.EQUAL], sts) of
          SOME (_, sts') => expression sts'
        | NONE => (Expr.Nil, sts)
      val (_, sts) =
        consume (T.SEMICOLON, "Expect ';' after variable declaration.") sts
    in
      (Stmt.Var {name = name, initializer = initializer}, sts)
    end

  (* statement -> exprStmt | ifStmt | printStmt | whileStmt | block *)
  and statement sts =
    let
      val (st, sts') = advance sts
    in
      case #token st of
        T.IF => ifStatement sts'
      | T.PRINT => printStatement sts'
      | T.WHILE => whileStatement sts'
      | T.LEFT_BRACE =>
          let val (decs, sts') = block ([], sts')
          in (Stmt.Block decs, sts')
          end
      | _ => expressionStatement sts
    end

  (* exprStmt -> expression ";" *)
  and expressionStatement sts =
    let
      val (expr, sts) = expression sts
      val (_, sts) = consume (T.SEMICOLON, "Expect ';' after expression.") sts
    in
      (Stmt.Expression expr, sts)
    end

  (* ifStmt -> "if" "(" expression ")" statement ( "else" statement )? *)
  and ifStatement sts =
    let
      val (_, sts) = consume (T.LEFT_PAREN, "Expect '(' after 'if'.") sts
      val (cond, sts) = expression sts
      val (_, sts) =
        consume (T.RIGHT_PAREN, "Expect ')' after if condition.") sts
      val (then_, sts) = statement sts
      val (else_, sts) =
        case match ([T.ELSE], sts) of
          NONE => (NONE, sts)
        | SOME (_, sts') =>
            let val (elseBranch, sts) = statement sts'
            in (SOME elseBranch, sts)
            end
    in
      (Stmt.If {condition = cond, thenBranch = then_, elseBranch = else_}, sts)
    end

  (* printStmt -> "print" expression ";" *)
  and printStatement sts =
    let
      val (value, sts) = expression sts
      val (_, sts) = consume (T.SEMICOLON, "Expect ';' after value.") sts
    in
      (Stmt.Print value, sts)
    end

  (* whileStmt -> "while" "(" expression ")" statement *)
  and whileStatement sts =
    let
      val (_, sts) = consume (T.LEFT_PAREN, "Expect '(' after 'while'.") sts
      val (cond, sts) = expression sts
      val (_, sts) = consume (T.RIGHT_PAREN, "Expect ')' after condition.") sts
      val (body, sts) = statement sts
    in
      (Stmt.While {condition = cond, body = body}, sts)
    end

  (* block -> "{" declaration* "}" *)
  and block (decs, sts) =
    if check ([T.RIGHT_BRACE, T.EOF], sts) then
      let val (_, sts) = consume (T.RIGHT_BRACE, "Expect '}' after block.") sts
      in (rev decs, sts)
      end
    else
      block (maybeDeclaration (decs, sts))

  and expression sts = assignment sts

  (* assignment -> IDENTIFIER "=" assignment | logic_or *)
  and assignment sts =
    let
      val (expr, sts) = or sts
    in
      case match ([T.EQUAL], sts) of
        SOME (equals, sts) =>
          let
            val (value, sts) = assignment sts
          in
            case expr of
              Expr.Variable name => (Expr.Assign (name, value), sts)
            | _ =>
                (error (equals, "Invalid assignment target.", sts); (expr, sts))
          end
      | NONE => (expr, sts)
    end

  and or sts =
    logical ([T.OR], and_) sts

  and and_ sts =
    logical ([T.AND], equality) sts

  and equality sts =
    binary ([T.BANG_EQUAL, T.EQUAL_EQUAL], comparison) sts

  and comparison sts =
    binary ([T.GREATER, T.GREATER_EQUAL, T.LESS, T.LESS_EQUAL], term) sts

  and term sts =
    binary ([T.MINUS, T.PLUS], factor) sts

  and factor sts =
    binary ([T.SLASH, T.STAR], unary) sts

  (* unary -> ( "!" | "-" ) unary | primary *)
  and unary sts =
    case match ([T.BANG, T.MINUS], sts) of
      SOME (operator, sts') =>
        let val (expr, sts') = unary sts'
        in (Expr.Unary (operator, expr), sts')
        end
    | NONE => primary sts

  (* primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" *)
  and primary sts =
    let
      val (st, sts') = advance sts
    in
      case #token st of
        T.FALSE => (Expr.Boolean false, sts')
      | T.TRUE => (Expr.Boolean true, sts')
      | T.NIL => (Expr.Nil, sts')
      | T.NUMBER n => (Expr.Number n, sts')
      | T.STRING s => (Expr.String s, sts')
      | T.IDENTIFIER => (Expr.Variable st, sts')
      | T.LEFT_PAREN =>
          let
            val (expr, sts') = expression sts'
            val (_, sts') =
              consume (T.RIGHT_PAREN, "Expect ')' after expression.") sts'
          in
            (Expr.Grouping expr, sts')
          end
      | _ => raise error (st, "Expect expression.", sts)
    end

  fun parse sts = program ([], sts)
end
