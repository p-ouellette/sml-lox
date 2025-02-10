structure Parser:
sig
  val parse: SourceToken.t list -> Expr.t
end =
struct
  structure T = Token

  fun error (st, msg) =
    (Error.errorAt (st, msg); Error.ParserError)

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

  fun consume (token, msg) sts =
    let val r as (st, _) = advance sts
    in if Token.sameType (token, #token st) then r else raise error (st, msg)
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
  fun binary (operators, operand) sts =
    let
      fun tail (left, sts) =
        case match (operators, sts) of
          SOME (operator, sts) =>
            let val (right, sts) = operand sts
            in tail (Expr.Binary (left, operator, right), sts)
            end
        | NONE => (left, sts)
      val (left, sts) = operand sts
      val (expr, sts) = tail (left, sts)
    in
      (expr, sts)
    end

  fun expression sts = equality sts

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
      val (st, sts) = advance sts
    in
      case #token st of
        T.FALSE => (Expr.Boolean false, sts)
      | T.TRUE => (Expr.Boolean true, sts)
      | T.NIL => (Expr.Nil, sts)
      | T.NUMBER n => (Expr.Number n, sts)
      | T.STRING s => (Expr.String s, sts)
      | T.LEFT_PAREN =>
          let
            val (expr, sts) = expression sts
            val (_, sts) =
              consume (T.RIGHT_PAREN, "Expect ')' after expression.") sts
          in
            (Expr.Grouping expr, sts)
          end
      | _ => raise error (st, "Expect expression.")
    end

  fun parse sts =
    let val (expr, _) = expression sts
    in expr
    end
end
