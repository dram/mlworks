structure Word8VectorSlice :> MONO_VECTOR_SLICE where type elem = Word8.word where type vector = Word8Vector.vector =
  struct
    type elem = Word8.word
    type vector = Word8Vector.vector
    type slice = (vector * int * int)

    fun length (_, _, n) = n

    fun sub ((vec, i0, _), i) = Word8Vector.sub (vec, i0 + i)

    fun full vec = (vec, 0, Word8Vector.length vec)

    fun slice (vec, i, sz) =
      case sz of
        NONE => (vec, i, Word8Vector.length vec - i)
      | SOME n => (vec, i, n)

    fun subslice ((vec, i0, n0), i, sz) =
      case sz of
        NONE => (vec, i0 + i, n0 - i)
      | SOME n => (vec, i0 + i, n)

    fun base s = s

    fun vector (vec, i0, n) = Word8Vector.tabulate (n, fn i => Word8Vector.sub (vec, i0 + i))

    val concat = Word8Vector.concat o List.map vector

    fun isEmpty (_, _, n) = n = 0

    fun getItem (vec, i, n) = if n = 0 then NONE else SOME (Word8Vector.sub (vec, i), (vec, i + 1, n - 1))

    fun appi f = Word8Vector.appi f o vector

    fun app f = Word8Vector.app f o vector

    fun mapi f = Word8Vector.mapi f o vector

    fun map f = Word8Vector.map f o vector

    fun foldli f init = Word8Vector.foldli f init o vector

    fun foldri f init = Word8Vector.foldri f init o vector

    fun foldl f init = Word8Vector.foldl f init o vector

    fun foldr f init = Word8Vector.foldr f init o vector

    fun findi pred = Word8Vector.findi pred o vector

    fun find pred = Word8Vector.find pred o vector

    fun exists pred = Word8Vector.exists pred o vector

    fun all pred = Word8Vector.all pred o vector

    fun collate cmp (s1, s2) = Word8Vector.collate cmp (vector s1, vector s2)
  end
