structure Compiler:
sig
  val compile: string -> unit
end =
struct
  fun compile source =
    let
      fun loop (cs, line) =
        let
          val (token, cs) = Scanner.scan cs
        in
          if #line token <> line then print (Int.toString (#line token))
          else print "|";
          print (#lexeme token ^ "\n");
          if #kind token <> Token.Kind.EOF then loop (cs, #line token) else ()
        end
    in
      loop (Scanner.init source, ~1)
    end
end
