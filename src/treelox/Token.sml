structure Token:
sig
  datatype t =
  (* single-character tokens *)
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  (* one or two character tokens *)
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  (* literals *)
  | Identifier
  | String of string
  | Number of real
  (* keywords *)
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While

  | Eof

  val sameType: t * t -> bool
end =
struct
  datatype t =
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star

  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual

  | Identifier
  | String of string
  | Number of real

  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While

  | Eof

  val sameType =
    fn (LeftParen, LeftParen) => true
     | (RightParen, RightParen) => true
     | (LeftBrace, LeftBrace) => true
     | (RightBrace, RightBrace) => true
     | (Comma, Comma) => true
     | (Dot, Dot) => true
     | (Minus, Minus) => true
     | (Plus, Plus) => true
     | (Semicolon, Semicolon) => true
     | (Slash, Slash) => true
     | (Star, Star) => true
     | (Bang, Bang) => true
     | (BangEqual, BangEqual) => true
     | (Equal, Equal) => true
     | (EqualEqual, EqualEqual) => true
     | (Greater, Greater) => true
     | (GreaterEqual, GreaterEqual) => true
     | (Less, Less) => true
     | (LessEqual, LessEqual) => true
     | (Identifier, Identifier) => true
     | (String _, String _) => true
     | (Number _, Number _) => true
     | (And, And) => true
     | (Class, Class) => true
     | (Else, Else) => true
     | (False, False) => true
     | (Fun, Fun) => true
     | (For, For) => true
     | (If, If) => true
     | (Nil, Nil) => true
     | (Or, Or) => true
     | (Print, Print) => true
     | (Return, Return) => true
     | (Super, Super) => true
     | (This, This) => true
     | (True, True) => true
     | (Var, Var) => true
     | (While, While) => true
     | (Eof, Eof) => true
     | _ => false
end
