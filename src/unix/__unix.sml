(* Copyright 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * See ./unix.sml
 *
 * Revision Log
 * ------------
 *
 * $Log: __unix.sml,v $
 * Revision 1.5  1999/05/13 15:27:02  daveb
 * [Bug #190553]
 * Added exit_status and fromStatus.
 *
 *  Revision 1.4  1999/03/24  10:40:21  mitchell
 *  [Bug #30286]
 *  Fix typo
 *
 *  Revision 1.3  1999/03/23  10:17:21  mitchell
 *  [Bug #30286]
 *  Remove redundant require statement
 *
 *  Revision 1.2  1999/03/22  17:02:22  mitchell
 *  [Bug #30286]
 *  Flesh out the Unix structure
 *
 *  Revision 1.1  1999/01/25  17:54:50  johnh
 *  new unit
 *  New Unix structure.
 *
 *)

require "^.basis.__text_io";
require "^.basis.__text_prim_io";
require "^.basis.__io";
require "^.basis.__word8";
require "__os";
require "__os_exit";
require "__unixos";
require "unix";

structure Unix : UNIX = 
struct

  datatype proc = 
    PROC of {id: int,
	     pipeIn:  TextIO.instream, 
	     pipeOut: TextIO.outstream}

  type signal = Word8.word

  datatype exit_status = datatype OSExit.exit_status

  val fromStatus = OSExit.fromStatus

  fun exit st = OSExit.os_exit (OSExit.W_EXITSTATUS st)

  local
    fun fdWriter (name, raw_fd) = 
      let 
	val fd = MLWorks.Internal.IO.FILE_DESC raw_fd
      in
	TextPrimIO.WR 
	  {name = name,
	   chunkSize = 1,
	   writeVec = SOME (fn {buf, i, sz} => 
			case sz of 
			   NONE => MLWorks.Internal.IO.write (fd, buf, i, (size buf) - i)
			 | SOME szi => MLWorks.Internal.IO.write (fd, buf, i, szi)),
	   writeArr = NONE,
	   writeVecNB = NONE,
	   writeArrNB = NONE,
	   block = NONE,
	   canOutput = NONE,
	   getPos = NONE,
	   setPos = NONE,
	   endPos = NONE,
	   verifyPos = NONE,
	   close = fn () => MLWorks.Internal.IO.close fd,
	   ioDesc = SOME (UnixOS_.FileSys.fdToIOD fd)}
      end

    fun fdReader (name, raw_fd) = 
      let 
	val fd = MLWorks.Internal.IO.FILE_DESC raw_fd
      in
	TextPrimIO.RD
	  {name = name,
	   chunkSize = 1,
	   readVec = SOME (fn i => MLWorks.Internal.IO.read (fd, i)),
	   readArr = NONE,
	   readVecNB = NONE,
	   readArrNB = NONE,
	   block = NONE,
	   canInput = 
	     SOME (fn _ => 
	        case MLWorks.Internal.IO.can_input fd of 
		   0 => false
		 | _ => true),
	   avail = (fn () => 
	        case MLWorks.Internal.IO.can_input fd of
		    0 => NONE
		  | i => SOME i),
	   getPos = NONE,
	   setPos = NONE,
	   endPos = NONE,
	   verifyPos = NONE,
	   close = fn () => MLWorks.Internal.IO.close fd,
	   ioDesc = SOME (UnixOS_.FileSys.fdToIOD fd)}
      end

    fun openOutFD (name, fd) =
	  TextIO.mkOutstream (
	    TextIO.StreamIO.mkOutstream (
	      fdWriter (name, fd), IO.BLOCK_BUF));

    fun openInFD (name, fd) =
	  TextIO.mkInstream (
	    TextIO.StreamIO.mkInstream (
	      fdReader (name, fd), ""));

    val env = MLWorks.Internal.Runtime.environment 
    val pipe : unit -> int * int = env "system os unix pipe"

    fun execute_with_env (s, sl, env_opt) = 
      let 
        val (p11, p12) = pipe()
        val (p21, p22) = pipe()

        val pipeIn = openInFD ("pipe_in", p11)
        val pipeOut = openOutFD ("pipe_out", p22)

	(* !!! other execute functions in UnixOS should be changed to match 
         * these ones in terms of the arguments they take *)
        val pid = 
            case env_opt of
              NONE => UnixOS_.fork_execv (s, sl, p21, p12, 2)
            | SOME env => UnixOS_.fork_execve (s, sl, env, p21, p12, 2)

        val _ = MLWorks.Internal.IO.close (MLWorks.Internal.IO.FILE_DESC p21)
        val _ = MLWorks.Internal.IO.close (MLWorks.Internal.IO.FILE_DESC p12)
      in
        PROC {id = pid, pipeIn = pipeIn, pipeOut = pipeOut}
      end  (* fun execute *)
  in
    fun execute (s, sl) = execute_with_env (s, sl, NONE) 
    fun executeInEnv (s, sl, environ) = 
      execute_with_env (s, sl, SOME environ)
  end (* local *)

  fun textInstreamOf (PROC {pipeIn, ...}) = pipeIn

  fun textOutstreamOf (PROC {pipeOut, ...}) = pipeOut

  fun streamsOf (PROC {id, pipeIn, pipeOut}) = (pipeIn, pipeOut)

  val wait : int -> OS.Process.status = 
    MLWorks.Internal.Runtime.environment "system os unix wait"

  fun reap (PROC{id, pipeIn, pipeOut}) = 
    (TextIO.closeIn pipeIn;
     TextIO.closeOut pipeOut;
     wait id)

  fun kill (PROC {id, pipeIn, pipeOut}, sign) = 
        UnixOS_.kill (id, Word8.toInt sign)
  
end
