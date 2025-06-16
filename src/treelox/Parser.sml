structure Parser:
sig
  val parse: SourceToken.t list -> Stmt.t list
end =
struct
  structure T = Token

  fun error (st, msg, sts) =
    (Error.errorAt (st, msg); Error.ParserError sts)

  fun advance ([]: SourceToken.t list) = raise Empty
    | advance (st :: sts) = (st, sts)

  fun match (tokens, sts) =
    let
      val (st, sts') = advance sts
    in
      if List.exists (fn t => T.sameType (t, #token st)) tokens then
        SOME (st, sts')
      else
        NONE
    end

  fun check (tokens, sts) =
    Option.isSome (match (tokens, sts))

  fun consume (token, msg, sts) =
    let val r as (st, _) = advance sts
    in if T.sameType (token, #token st) then r else raise error (st, msg, sts)
    end

  fun syncronize sts =
    let
      val stmtStart =
        [T.CLASS, T.FOR, T.FUN, T.IF, T.PRINT, T.RETURN, T.VAR, T.WHILE]
      val (st, sts') = advance sts
    in
      (* stop after semicolon or at start of statement *)
      case #token st of
        T.EOF => sts
      | T.SEMICOLON => sts'
      | _ => if check (stmtStart, sts') then sts' else syncronize sts'
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

  (* declaration -> classDecl | funDecl | varDecl | statement
   * funDecl -> "fun" function
   *)
  and declaration sts =
    let
      val (st, sts') = advance sts
    in
      case #token st of
        T.CLASS => classDeclaration sts'
      | T.FUN =>
          let val (func, sts') = function ("function", sts')
          in (Stmt.Function func, sts')
          end
      | T.VAR => varDeclaration sts'
      | _ => statement sts
    end

  (* classDecl -> "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" *)
  and classDeclaration sts =
    let
      fun parseMethods (acc, sts) =
        if check ([T.RIGHT_BRACE, T.EOF], sts) then
          (rev acc, sts)
        else
          let val (method, sts) = function ("method", sts)
          in parseMethods (method :: acc, sts)
          end
      val (name, sts) = consume (T.IDENTIFIER, "Expect class name.", sts)
      val (superclass, sts) =
        case match ([T.LESS], sts) of
          SOME (_, sts') =>
            let
              val (superclass, sts') =
                consume (T.IDENTIFIER, "Expect superclass name.", sts')
            in
              (SOME superclass, sts')
            end
        | NONE => (NONE, sts)
      val (_, sts) = consume
        (T.LEFT_BRACE, "Expect '{' before class body.", sts)
      val (methods, sts) = parseMethods ([], sts)
      val (_, sts) = consume
        (T.RIGHT_BRACE, "Expect '}' after class body.", sts)
      val class =
        Stmt.Class {name = name, superclass = superclass, methods = methods}
    in
      (class, sts)
    end

  (* function -> IDENTIFIER "(" parameters? ")" block *)
  and function (kind, sts) =
    let
      fun parseParams (params, sts) =
        let
          val (name, sts) =
            consume (T.IDENTIFIER, "Expect parameter name.", sts)
          val params = name :: params
        in
          if length params > 255 then
            ignore (error (name, "Can't have more than 255 parameters.", sts))
          else
            ();
          case match ([T.COMMA], sts) of
            SOME (_, sts') => parseParams (params, sts')
          | NONE => (rev params, sts)
        end
      val (name, sts) = consume (T.IDENTIFIER, "Expect " ^ kind ^ " name.", sts)
      val (_, sts) = consume
        (T.LEFT_PAREN, "Expect '(' after " ^ kind ^ " name.", sts)
      val (params, sts) =
        if check ([T.RIGHT_PAREN], sts) then ([], sts)
        else parseParams ([], sts)
      val (_, sts) = consume
        (T.RIGHT_PAREN, "Expect ')' after parameters.", sts)
      val (_, sts) = consume
        (T.LEFT_BRACE, "Expect '{' before " ^ kind ^ " body.", sts)
      val (body, sts) = block ([], sts)
    in
      ({name = name, params = params, body = body}, sts)
    end

  (* varDecl -> "var" IDENTIFIER ( "=" expression )? ";" *)
  and varDeclaration sts =
    let
      val (name, sts) = consume (T.IDENTIFIER, "Expect variable name.", sts)
      val (initializer, sts) =
        case match ([T.EQUAL], sts) of
          SOME (_, sts') => expression sts'
        | NONE => (Expr.Nil, sts)
      val (_, sts) = consume
        (T.SEMICOLON, "Expect ';' after variable declaration.", sts)
    in
      (Stmt.Var {name = name, initializer = initializer}, sts)
    end

  (* statement -> exprStmt | forStmt | ifStmt | printStmt | returnStmt
   *            | whileStmt | block
   *)
  and statement sts =
    let
      val (st, sts') = advance sts
    in
      case #token st of
        T.FOR => forStatement sts'
      | T.IF => ifStatement sts'
      | T.PRINT => printStatement sts'
      | T.RETURN => returnStatement (st, sts')
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
      val (_, sts) = consume (T.SEMICOLON, "Expect ';' after expression.", sts)
    in
      (Stmt.Expression expr, sts)
    end

  (* forStmt -> "for" "(" ( varDecl | exprStmt | ";" ) expression? ";"
   * expression? ")" statement
   *)
  and forStatement sts =
    let
      val (_, sts) = consume (T.LEFT_PAREN, "Expect '(' after 'for'.", sts)
      val (st, sts') = advance sts
      val (initializer, sts) =
        case #token st of
          T.SEMICOLON => (NONE, sts')
        | t =>
            let
              val (stmt, sts) =
                case t of
                  T.VAR => varDeclaration sts'
                | _ => expressionStatement sts
            in
              (SOME stmt, sts)
            end
      val (condition, sts) =
        if check ([T.SEMICOLON], sts) then (Expr.Boolean true, sts)
        else let val (expr, sts) = expression sts in (expr, sts) end
      val (_, sts) = consume
        (T.SEMICOLON, "Expect ';' after loop condition.", sts)
      val (increment, sts) =
        if check ([T.RIGHT_PAREN], sts) then (NONE, sts)
        else let val (expr, sts) = expression sts in (SOME expr, sts) end
      val (_, sts) = consume
        (T.RIGHT_PAREN, "Expect ')' after for clauses.", sts)
      val (body, sts) = statement sts
      val body =
        case increment of
          SOME increment => Stmt.Block [body, Stmt.Expression increment]
        | NONE => body
      val body = Stmt.While {condition = condition, body = body}
      val body =
        case initializer of
          SOME initializer => Stmt.Block [initializer, body]
        | NONE => body
    in
      (body, sts)
    end

  (* ifStmt -> "if" "(" expression ")" statement ( "else" statement )? *)
  and ifStatement sts =
    let
      val (_, sts) = consume (T.LEFT_PAREN, "Expect '(' after 'if'.", sts)
      val (cond, sts) = expression sts
      val (_, sts) = consume
        (T.RIGHT_PAREN, "Expect ')' after if condition.", sts)
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
      val (_, sts) = consume (T.SEMICOLON, "Expect ';' after value.", sts)
    in
      (Stmt.Print value, sts)
    end

  (* returnStmt -> "return" expression? ";" *)
  and returnStatement (keyword, sts) =
    let
      val (value, sts) =
        if check ([T.SEMICOLON], sts) then (Expr.Nil, sts) else expression sts
      val (_, sts) = consume
        (T.SEMICOLON, "Expect ';' after return value.", sts)
    in
      (Stmt.Return (keyword, value), sts)
    end

  (* whileStmt -> "while" "(" expression ")" statement *)
  and whileStatement sts =
    let
      val (_, sts) = consume (T.LEFT_PAREN, "Expect '(' after 'while'.", sts)
      val (cond, sts) = expression sts
      val (_, sts) = consume (T.RIGHT_PAREN, "Expect ')' after condition.", sts)
      val (body, sts) = statement sts
    in
      (Stmt.While {condition = cond, body = body}, sts)
    end

  (* block -> "{" declaration* "}" *)
  and block (decs, sts) =
    if check ([T.RIGHT_BRACE, T.EOF], sts) then
      let val (_, sts) = consume (T.RIGHT_BRACE, "Expect '}' after block.", sts)
      in (rev decs, sts)
      end
    else
      block (maybeDeclaration (decs, sts))

  and expression sts = assignment sts

  (* assignment -> ( call "." )? IDENTIFIER "=" assignment | logic_or *)
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
            | Expr.Get (object, name) => (Expr.Set (object, name, value), sts)
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

  (* unary -> ( "!" | "-" ) unary | call *)
  and unary sts =
    case match ([T.BANG, T.MINUS], sts) of
      SOME (operator, sts') =>
        let val (expr, sts') = unary sts'
        in (Expr.Unary (operator, expr), sts')
        end
    | NONE => call sts

  (* call -> primary ( "(" arguments? ")" | "." IDENTIFIER )* *)
  and call sts =
    let
      fun tail (expr, sts) =
        let
          val (st, sts') = advance sts
        in
          case #token st of
            T.LEFT_PAREN => tail (finishCall (expr, sts'))
          | T.DOT =>
              let
                val (name, sts') = consume
                  (T.IDENTIFIER, "Expect property name after '.'.", sts')
              in
                tail (Expr.Get (expr, name), sts')
              end
          | _ => (expr, sts)
        end
    in
      tail (primary sts)
    end

  and finishCall (callee, sts) =
    let
      fun parseArgs (args, sts) =
        let
          val _ =
            if length args >= 255 then
              ignore (error
                (hd sts, "Can't have more than 255 arguments.", sts))
            else
              ()
          val (arg, sts) = expression sts
          val args = arg :: args
        in
          case match ([T.COMMA], sts) of
            SOME (_, sts') => parseArgs (args, sts')
          | NONE => (rev args, sts)
        end
      val (arguments, sts) =
        if check ([T.RIGHT_PAREN], sts) then ([], sts) else parseArgs ([], sts)
      val (paren, sts) =
        consume (T.RIGHT_PAREN, "Expect ')' after arguments.", sts)
    in
      (Expr.Call {callee = callee, paren = paren, arguments = arguments}, sts)
    end

  (* primary -> "true" | "false" | "nil" | "this"
   *          | NUMBER | STRING | IDENTIFIER | "(" expression ")"
   *          | "super" "." IDENTIFIER
   *)
  and primary sts =
    let
      val (st, sts') = advance sts
    in
      case #token st of
        T.TRUE => (Expr.Boolean true, sts')
      | T.FALSE => (Expr.Boolean false, sts')
      | T.NIL => (Expr.Nil, sts')
      | T.THIS => (Expr.This st, sts')
      | T.NUMBER n => (Expr.Number n, sts')
      | T.STRING s => (Expr.String s, sts')
      | T.IDENTIFIER => (Expr.Variable st, sts')
      | T.LEFT_PAREN =>
          let
            val (expr, sts') = expression sts'
            val (_, sts') = consume
              (T.RIGHT_PAREN, "Expect ')' after expression.", sts')
          in
            (Expr.Grouping expr, sts')
          end
      | T.SUPER =>
          let
            val (_, sts') = consume (T.DOT, "Expect '.' after 'super'.", sts')
            val (method, sts') = consume
              (T.IDENTIFIER, "Expect superclass method name.", sts')
          in
            (Expr.Super {keyword = st, method = method}, sts')
          end
      | _ => raise error (st, "Expect expression.", sts)
    end

  fun parse sts = program ([], sts)
end
