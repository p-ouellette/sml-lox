structure Chunk:
sig
  type t
  val new: unit -> t
  val write: t * Word8.word -> unit
  val addConstant: t * Value.t -> int
  val disassemble: t * string -> unit
  val disassembleInstruction: t * int -> int
end =
struct
  structure A = Word8Array
  structure OP = Opcode

  type t = {count: int ref, code: A.array ref, constants: ValueArray.t}

  fun new () =
    { count = ref 0
    , code = ref (A.array (8, 0w0))
    , constants = ValueArray.new ()
    }

  fun count (chunk: t) =
    !(#count chunk)

  fun sub (chunk: t, i) =
    A.sub (!(#code chunk), i)

  fun growArray (array, oldlen, newlen) =
    A.tabulate (newlen, fn i => if i >= oldlen then A.sub (array, i) else 0w0)

  fun write ({count, code, constants = _}, byte) =
    let
      val capacity = A.length (!code)
    in
      if capacity < !count + 1 then
        code := growArray (!code, capacity, capacity * 2)
      else
        ();
      A.update (!code, !count, byte);
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
          print (StringCvt.padLeft #"0" 4 (Int.toString constant));
          print " '";
          print (Value.toString (ValueArray.sub (#constants chunk, constant)));
          print "'\n";
          offset + 2
        end

      fun simpleInstruction (name, offset) =
        (print (name ^ "\n"); offset + 1)
    in
      print (StringCvt.padLeft #"0" 4 (Int.toString offset));
      print " ";

      case OP.decode (sub (chunk, offset)) of
        OP.CONSTANT => constantInstruction ("OP_CONSTANT", chunk, offset)
      | OP.RETURN => simpleInstruction ("OP_RETURN", offset)
    end
end
