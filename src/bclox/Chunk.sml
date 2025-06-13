structure Chunk:
sig
  type t
  val new: unit -> t
  val write: t * Word8.word * int -> unit
  val addConstant: t * Value.t -> int
  val disassemble: t * string -> unit
  val disassembleInstruction: t * int -> int
end =
struct
  structure OP = Opcode

  type t =
    { count: int ref
    , code: Word8Array.array ref
    , lines: IntArray.array ref
    , constants: ValueArray.t
    }

  fun new () =
    { count = ref 0
    , code = ref (Word8Array.array (8, 0w0))
    , lines = ref (IntArray.array (8, 0))
    , constants = ValueArray.new ()
    }

  fun count (chunk: t) =
    !(#count chunk)

  fun sub (chunk: t, i) =
    Word8Array.sub (!(#code chunk), i)

  fun getLine (chunk: t, i) =
    IntArray.sub (!(#lines chunk), i)

  fun growWord8Array (array, count) =
    Word8Array.tabulate (count * 2, fn i =>
      if i >= count then Word8Array.sub (array, i) else 0w0)

  fun growIntArray (array, count) =
    IntArray.tabulate (count * 2, fn i =>
      if i >= count then IntArray.sub (array, i) else 0)

  fun write ({count, code, lines, ...}: t, byte, line) =
    let
      val capacity = Word8Array.length (!code)
    in
      if capacity < !count + 1 then
        ( code := growWord8Array (!code, capacity)
        ; lines := growIntArray (!lines, capacity)
        )
      else
        ();
      Word8Array.update (!code, !count, byte);
      IntArray.update (!lines, !count, line);
      count := !count + 1
    end

  fun addConstant ({constants, ...}: t, value) =
    (ValueArray.write (constants, value); ValueArray.count constants - 1)

  fun disassemble (chunk: t, name) =
    let
      fun loop offset =
        if offset < count chunk then
          loop (disassembleInstruction (chunk, offset))
        else
          ()
    in
      print ("== " ^ name ^ " ==\n");
      loop 0
    end

  and disassembleInstruction (chunk, offset) =
    let
      fun constantInstruction (name, chunk, offset) =
        let
          val constant = Word8.toInt (sub (chunk, offset + 1))
        in
          print (StringCvt.padRight #" " 16 name);
          print " ";
          print (StringCvt.padLeft #" " 4 (Int.toString constant));
          print " '";
          print (Value.toString (ValueArray.sub (#constants chunk, constant)));
          print "'\n";
          offset + 2
        end

      fun simpleInstruction (name, offset) =
        (print (name ^ "\n"); offset + 1)

      val line = getLine (chunk, offset)
    in
      print (StringCvt.padLeft #"0" 4 (Int.toString offset));
      print " ";
      if offset > 0 andalso line = getLine (chunk, offset - 1) then
        print "   | "
      else
        print (StringCvt.padLeft #" " 4 (Int.toString line) ^ " ");

      case OP.decode (sub (chunk, offset)) of
        OP.CONSTANT => constantInstruction ("OP_CONSTANT", chunk, offset)
      | OP.RETURN => simpleInstruction ("OP_RETURN", offset)
    end
end
