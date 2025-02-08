structure Token:
sig
  datatype t =
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR

  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL

  | IDENTIFIER
  | STRING of string
  | NUMBER of real

  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE

  | EOF

  val sameType: t * t -> bool

  val toString: t -> string
end =
struct
  datatype t =
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR

  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL

  | IDENTIFIER
  | STRING of string
  | NUMBER of real

  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE

  | EOF

  val sameType =
    fn (LEFT_PAREN, LEFT_PAREN) => true
     | (RIGHT_PAREN, RIGHT_PAREN) => true
     | (LEFT_BRACE, LEFT_BRACE) => true
     | (RIGHT_BRACE, RIGHT_BRACE) => true
     | (COMMA, COMMA) => true
     | (DOT, DOT) => true
     | (MINUS, MINUS) => true
     | (PLUS, PLUS) => true
     | (SEMICOLON, SEMICOLON) => true
     | (SLASH, SLASH) => true
     | (STAR, STAR) => true
     | (BANG, BANG) => true
     | (BANG_EQUAL, BANG_EQUAL) => true
     | (EQUAL, EQUAL) => true
     | (EQUAL_EQUAL, EQUAL_EQUAL) => true
     | (GREATER, GREATER) => true
     | (GREATER_EQUAL, GREATER_EQUAL) => true
     | (LESS, LESS) => true
     | (LESS_EQUAL, LESS_EQUAL) => true
     | (IDENTIFIER, IDENTIFIER) => true
     | (STRING _, STRING _) => true
     | (NUMBER _, NUMBER _) => true
     | (AND, AND) => true
     | (CLASS, CLASS) => true
     | (ELSE, ELSE) => true
     | (FALSE, FALSE) => true
     | (FUN, FUN) => true
     | (FOR, FOR) => true
     | (IF, IF) => true
     | (NIL, NIL) => true
     | (OR, OR) => true
     | (PRINT, PRINT) => true
     | (RETURN, RETURN) => true
     | (SUPER, SUPER) => true
     | (THIS, THIS) => true
     | (TRUE, TRUE) => true
     | (VAR, VAR) => true
     | (WHILE, WHILE) => true
     | (EOF, EOF) => true
     | _ => false

  val toString =
    fn LEFT_PAREN => "LEFT_PAREN"
     | RIGHT_PAREN => "RIGHT_PAREN"
     | LEFT_BRACE => "LEFT_BRACE"
     | RIGHT_BRACE => "RIGHT_BRACE"
     | COMMA => "COMMA"
     | DOT => "DOT"
     | MINUS => "MINUS"
     | PLUS => "PLUS"
     | SEMICOLON => "SEMICOLON"
     | SLASH => "SLASH"
     | STAR => "STAR"
     | BANG => "BANG"
     | BANG_EQUAL => "BANG_EQUAL"
     | EQUAL => "EQUAL"
     | EQUAL_EQUAL => "EQUAL_EQUAL"
     | GREATER => "GREATER"
     | GREATER_EQUAL => "GREATER_EQUAL"
     | LESS => "LESS"
     | LESS_EQUAL => "LESS_EQUAL"
     | IDENTIFIER => "IDENTIFIER"
     | STRING s => "STRING " ^ s
     | NUMBER n => "NUMBER " ^ Real.toString n
     | AND => "AND"
     | CLASS => "CLASS"
     | ELSE => "ELSE"
     | FALSE => "FALSE"
     | FUN => "FUN"
     | FOR => "FOR"
     | IF => "IF"
     | NIL => "NIL"
     | OR => "OR"
     | PRINT => "PRINT"
     | RETURN => "RETURN"
     | SUPER => "SUPER"
     | THIS => "THIS"
     | TRUE => "TRUE"
     | VAR => "VAR"
     | WHILE => "WHILE"
     | EOF => "EOF"
end

structure SourceToken:
sig
  type t = {token: Token.t, lexeme: string, line: int}

  val toString: t -> string
end =
struct
  type t = {token: Token.t, lexeme: string, line: int}

  fun toString {token, lexeme, line = _} =
    Token.toString token ^ " " ^ lexeme
end
