(*  ==== INITIAL BASIS : REAL VECTOR ====
 *
 *  Copyright 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *  
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  Implementation
 *  --------------
 *  Real vectors are identified with MLWorks FloatArrays.
 *
 *  Revision Log
 *  ------------
 *  $Log: __real_vector.sml,v $
 *  Revision 1.4  1999/02/17 14:40:20  mitchell
 *  [Bug #190507]
 *  Modify to satisfy CM constraints.
 *
 *  Revision 1.3  1998/02/19  19:55:03  mitchell
 *  [Bug #30349]
 *  Fix to avoid non-unit sequence warnings
 *
 *  Revision 1.2  1997/08/08  10:56:53  brucem
 *  [Bug #30086]
 *  Add map and mapi.
 *
 *  Revision 1.1  1997/01/30  14:51:17  andreww
 *  new unit
 *  new real vector stuff which uses builtin floatarray primitives.
 *
 *
 *
 *)


require "mono_vector";
require "__pre_real";

structure RealVector :> MONO_VECTOR where type elem = PreReal.real =
  struct
    structure F = MLWorks.Internal.FloatArray

    type elem = PreReal.real
    type vector = F.floatarray

    val maxLen = F.maxLen

    fun checkSize l = if l<0 orelse l>maxLen then raise Size else l

    fun fromList l =
      if length l>maxLen then raise Size
      else F.from_list l

    fun tabulate (n,f) = F.tabulate(checkSize n,f)

    val length = F.length
    val sub = F.sub

    fun check_slice (vector,i,SOME j) =
      if i < 0 orelse j < 0 orelse i + j > length vector
        then raise Subscript
      else j
      | check_slice (vector,i,NONE) =
        let
          val l = length vector
        in
          if i < 0 orelse i > l
            then raise Subscript
          else l - i
        end


    fun extract(vector,i,j) =
      let
        val len = check_slice(vector,i,j)
      in
        F.tabulate(len,fn n => sub(vector,n+i))
      end

    
    fun concat [] = F.array(0,0.0)
      | concat l =
        let 
          fun addSizes ([],acc) = acc
            | addSizes (h::t,acc) = 
            let val l = length h + acc
            in  if l>maxLen then raise Size
              else addSizes(t,l)
            end
         
          val f = F.array(addSizes(l,0),0.0)
          
          fun copyAll(_,[]) = ()
            | copyAll(idx,h::t) =
            let
              val l = length h
              val _ = F.copy(h,0,l,f,idx)
            in
              copyAll(idx+l,t)
            end
        in
          copyAll(0,l); f
        end


    fun appi f vector =
      let
	val l = length vector
	fun iterate n =
	  if n = l then
	    ()
	  else
	    (ignore(f(n, sub(vector, n)));
	     iterate(n+1))
      in
	iterate 0
      end


    fun app f vector =
      let
	val l = length vector
	fun iterate n =
	  if n = l then
	    ()
	  else
	    (ignore(f(sub(vector, n)));
	     iterate(n+1))
      in
	iterate 0
      end


    fun foldl f b vector =
      let
	val l = length vector
	fun reduce(n, x) =
	  if n = l then
	    x
	  else
	    reduce(n+1, f(sub(vector, n), x))
      in
	reduce(0, b)
      end

    fun foldr f b vector =
      let
	val l = length vector
	fun reduce(n, x) =
	  if n < 0 then
	    x
	  else
	    reduce(n-1, f(sub(vector, n), x))
      in
	reduce(l-1, b)
      end

    fun foldli f b vector =
      let
	val l = length vector
	fun reduce(n, x) =
	  if n = l then
	    x
	  else
	    reduce(n+1, f(n, sub(vector, n), x))
      in
	reduce (0, b)
      end

    fun foldri f b vector =
      let
	val l = length vector
	fun reduce(n, x) =
	  if n < 0 then
	    x
	  else
	    reduce(n-1, f(n, sub(vector, n), x))
      in
	reduce (l - 1, b)
      end

    fun map f v =
      let
        val l = length v
        fun f' i = f (sub(v, i))
      in
        tabulate (l, f')
      end

   fun mapi f v =
     let 
       val l = length v
       fun f' i = f (i, sub (v, i))
     in
       tabulate (l, f')
     end

    fun findi pred vec =
      let
        val l = length vec
        fun iterate n =
          if n = l then
            NONE
          else
            let val a = sub (vec, n) in if pred (n, a) then SOME (n, a) else iterate (n + 1) end
      in
        iterate 0
      end

    fun find pred vec =
      let
        val l = length vec
        fun iterate n =
          if n = l then
            NONE
          else
            let val a = sub (vec, n) in if pred a then SOME a else iterate (n + 1) end
      in
        iterate 0
      end

    fun exists pred vec =
      let
        val l = length vec
        fun iterate n = if n = l then false else if pred (sub (vec, n)) then true else iterate (n + 1)
      in
        iterate 0
      end

    fun all pred vec =
      let
        val l = length vec
        fun iterate n = if n = l then true else if pred (sub (vec, n)) then iterate (n + 1) else false
      in
        iterate 0
      end

    fun collate cmp (v1, v2) =
      let
        val l1 = length v1
        val l2 = length v2
        fun aux n =
          if n = l1 andalso n = l2 then
            EQUAL
          else if n = l1 then
            LESS
          else if n = l2 then
            GREATER
          else
            case cmp (sub (v1, n), sub (v2, n)) of
              EQUAL => aux (n + 1)
            | order => order
      in
        aux 0
      end

  end
