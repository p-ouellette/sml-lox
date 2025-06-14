structure Chunk:
sig
  type t
  val new: unit -> t
  val count: t -> int
  val sub: t * int -> Word8.word
  val getLine: t * int -> int
  val write: t * Word8.word * int -> unit
  val addConstant: t * Value.t -> int
end =
struct
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
end
