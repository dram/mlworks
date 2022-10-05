require "vector_slice";
require "__list";
require "__vector";

structure VectorSlice :> VECTOR_SLICE =
  struct
    type 'a slice = ('a Vector.vector * int * int)

    fun length (_, _, n) = n

    fun sub ((vec, i0, _), i) = Vector.sub (vec, i0 + i)

    fun full vec = (vec, 0, Vector.length vec)

    fun slice (vec, i, sz) =
      case sz of
        NONE => (vec, i, Vector.length vec - i)
      | SOME n => (vec, i, n)

    fun subslice ((vec, i0, n0), i, sz) =
      case sz of
        NONE => (vec, i0 + i, n0 - i)
      | SOME n => (vec, i0 + i, n)

    fun base s = s

    fun vector (vec, i0, n) = Vector.tabulate (n, fn i => Vector.sub (vec, i0 + i))

    fun concat l = Vector.concat (List.map vector l)

    fun isEmpty (_, _, n) = n = 0

    fun getItem (vec, i, n) = if n = 0 then NONE else SOME (Vector.sub (vec, i), (vec, i + 1, n - 1))

    fun appi f = Vector.appi f o vector

    fun app f = Vector.app f o vector

    fun mapi f = Vector.mapi f o vector

    fun map f = Vector.map f o vector

    fun foldli f init = Vector.foldli f init o vector

    fun foldri f init = Vector.foldri f init o vector

    fun foldl f init = Vector.foldl f init o vector

    fun foldr f init = Vector.foldr f init o vector

    fun findi pred = Vector.findi pred o vector

    fun find pred = Vector.find pred o vector

    fun exists pred = Vector.exists pred o vector

    fun all pred = Vector.all pred o vector

    fun collate cmp (s1, s2) = Vector.collate cmp (vector s1, vector s2)
  end
