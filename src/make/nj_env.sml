(*  New Jersey emulation of runtime environment
 *
 *  Copyright (C) 1996 Harlequin Ltd
 *
 *  $Log: nj_env.sml,v $
 *  Revision 1.1  1996/01/23 10:41:34  matthew
 *  new unit
 *  Emulation of runtime environment
 *
 *)


(* Also needed for MLWorks.Internal.Types.time *)
structure MLWTime =
  struct
    datatype time = TIME of int * int * int
  end

local
    (* A handful of environment functions that we need *)
    (* We only need the functions that actually get called by the
    compiler here *)

    (* http://www.standardml.org/Basis/os-process.html#SIG:OS_PROCESS.getEnv:VAL *)
    fun environment () : string list =
	(print "D: os unix environment called\n";
	 Posix.ProcEnv.environ ())

    (* http://www.standardml.org/Basis/os-file-sys.html#SIG:OS_FILE_SYS.chDir:VAL *)
    val setwd = OS.FileSys.chDir

    (* http://www.standardml.org/Basis/os-file-sys.html#SIG:OS_FILE_SYS.getDir:VAL *)
    val getwd = OS.FileSys.getDir

    (* http://www.standardml.org/Basis/os-file-sys.html#SIG:OS_FILE_SYS.realPath:VAL *)
    val realpath = OS.FileSys.realPath

    (* stat is a pain to emulate *)
    local
	(* layouts must match definitions in unix/_unixos.sml *)
	type stat = {dev	: int,
		     ino	: int,
		     mode	: int,
		     nlink	: int,
		     uid	: int,
		     gid	: int,
		     rdev	: int,
		     size	: Position.int,
		     atime	: MLWTime.time,
		     mtime	: MLWTime.time,
		     ctime	: MLWTime.time,
		     blksize	: int,
		     blocks	: int}
	fun wrap (s: Posix.FileSys.ST.stat) : stat =
	    {dev       = 0,
	     ino       = 0,
	     mode      = 0,
	     nlink     = if Posix.FileSys.ST.isDir s then 0 else 1,
	     uid       = 0,
	     gid       = 0,
	     rdev      = 0,
	     size      = Posix.FileSys.ST.size s,
	     atime     = MLWTime.TIME (0, 0, 0),
	     mtime     = MLWTime.TIME (0, 0, 0),
	     ctime     = MLWTime.TIME (0, 0, 0),
	     blksize   = 4096,
	     blocks    = 0
	    }

    in
    fun stat (pathname: string): stat = wrap (Posix.FileSys.stat pathname)
    fun fstat (fd): stat = wrap (Posix.FileSys.fstat fd)
    fun isdir (s: stat) = let val {nlink, ...} = s in nlink = 0 end
    fun mkdir (pathname: string, mode: int): unit =
	Posix.FileSys.mkdir (pathname, Posix.FileSys.S.fromWord (SysWord.fromInt mode))
    end

    val o_rdonly = 0w1
    val o_wronly = 0w2
    val o_append = 0w4
    val o_creat  = 0w8
    val o_trunc  = 0w16
    fun open' (pathname: string, flags: word, mode: int) =
	let val om = if Word.andb (flags, o_rdonly) <> 0w0 then Posix.FileSys.O_RDONLY
		     else if Word.andb (flags, o_wronly) <> 0w0 then Posix.FileSys.O_WRONLY
		     else raise (Fail "open'")
	    val f = if Word.andb (flags, o_append) <> 0w0 then Posix.FileSys.O.append
                    else if Word.andb (flags, o_trunc) <> 0w0 then Posix.FileSys.O.trunc
                    else Posix.FileSys.O.fromWord 0w0
            val m = Posix.FileSys.S.fromWord (Word32.fromInt mode)
	in
	    if Word.andb (flags, o_creat) <> 0w0 then Posix.FileSys.createf (pathname, om, f, m)
	    else Posix.FileSys.openf (pathname, om, f)
	end

    (* http://www.smlnj.org/doc/SMLofNJ/pages/unsafe.html#SIG:UNSAFE.cast:VAL *)
    type T = int ref
    val tcast : 'a -> T = Unsafe.cast

    val env_refs = ref [] : (string * T) list ref

    fun add_env_function (name,f) =
	env_refs := (name,tcast f) :: !env_refs

    (* These may be all we need *)
    val _ =
	(add_env_function ("system os unix environment",environment);
	 add_env_function ("system os unix setwd",setwd);
	 add_env_function ("system os unix getwd",getwd);
	 add_env_function ("system os unix realpath",realpath);
	 add_env_function ("POSIX.FileSys.fstat", fstat);
	 add_env_function ("POSIX.FileSys.stat", stat);
	 add_env_function ("POSIX.FileSys.ST.isdir", isdir);
	 add_env_function ("POSIX.FileSys.mkdir", mkdir);
	 add_env_function ("system os unix open", open');
	 add_env_function ("system os unix o_rdonly", o_rdonly);
	 add_env_function ("system os unix o_wronly", o_wronly);
	 add_env_function ("system os unix o_append", o_append);
	 add_env_function ("system os unix o_creat", o_creat);
	 add_env_function ("system os unix o_trunc", o_trunc);
	 add_env_function ("OS.FileSys.fullPath", OS.FileSys.fullPath);
	 add_env_function ("POSIX.FileSys.getcwd", Posix.FileSys.getcwd);
	 add_env_function ("POSIX.FileSys.access", Posix.FileSys.access);
	 add_env_function ("POSIX.FileSys.unlink", Posix.FileSys.unlink);
	 add_env_function ("Time.toReal", fn _ => 0.0);
	 add_env_function ("Time.fromReal", fn _ => MLWTime.TIME (0, 0, 0));
	 add_env_function ("Time.-", fn _ => MLWTime.TIME (0, 0, 0));
	 add_env_function ("Time.+", fn _ => MLWTime.TIME (0, 0, 0));
	 add_env_function ("real split", Real.split)
	)

    exception UnimplementedEnv of string
    fun unimplemented name =
	(TextIO.output (TextIO.stdOut, "unimplemented env function: "
				       ^ name ^ "\n");
	 raise UnimplementedEnv name)

in
fun nj_environment name =
    let
	fun trap _ = unimplemented ("Environment function " ^ name)
	fun lookup [] = tcast trap
	  | lookup ((name', f)::rest) =
	    if name' = name then f else lookup rest
    in
	TextIO.print ("D: nj_environment lookup: " ^ name ^ "\n");
	lookup (!env_refs)
    end
end
