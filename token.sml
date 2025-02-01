structure Token:
sig
  datatype ty =
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

  | IDENTIFIER of string
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

  type t = {ty: ty, lexeme: string, line: int}

  val tyToString: ty -> string
  val toString: t -> string
end =
struct
  datatype ty =
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

  | IDENTIFIER of string
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

  type t = {ty: ty, lexeme: string, line: int}

  val tyToString =
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
     | IDENTIFIER id => "IDENTIFIER " ^ id
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

  fun toString {ty, lexeme, line = _} =
    tyToString ty ^ " " ^ lexeme
end
