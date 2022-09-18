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

datatype time = TIME of int * int * int

local
  (* A handful of environment functions that we need *)
  (* We only need the functions that actually get called by the compiler here *)

  (* http://www.standardml.org/Basis/os-file-sys.html#SIG:OS_FILE_SYS.chDir:VAL *)
  val setwd = OS.FileSys.chDir

  (* http://www.standardml.org/Basis/os-file-sys.html#SIG:OS_FILE_SYS.getDir:VAL *)
  val getwd = OS.FileSys.getDir

  (* http://www.standardml.org/Basis/os-file-sys.html#SIG:OS_FILE_SYS.realPath:VAL *)
  val realpath = OS.FileSys.realPath

  (* http://www.smlnj.org/doc/SMLofNJ/pages/unsafe.html#SIG:UNSAFE.cast:VAL *)
  type T = int ref
  val tcast : 'a -> T = Unsafe.cast

  (* based on `ST.stat` in src/unix/_unixos.sml *)
  type stat =
    {dev     : int,
     ino     : int,
     mode    : int,
     nlink   : int,
     uid     : int,
     gid     : int,
     rdev    : int,
     size    : Position.int,
     atime   : time,
     mtime   : time,
     ctime   : time,
     blksize : int,
     blocks  : int}

  fun wrapStat (s: Posix.FileSys.ST.stat) : stat =
    {dev     = 0,
     ino     = 0,
     mode    = if Posix.FileSys.ST.isDir s then 0 else 1,
     nlink   = 0,
     uid     = 0,
     gid     = 0,
     rdev    = 0,
     size    = Posix.FileSys.ST.size s,
     atime   = TIME (0, 0, 0),
     mtime   = TIME (0, 0, 0),
     ctime   = TIME (0, 0, 0),
     blksize = 4096,
     blocks  = 0}

  val env_refs = ref [] : (string * T) list ref

  fun add_env_function (name,f) =
    env_refs := (name,tcast f) :: !env_refs

  (* These may be all we need *)
  val _ =
    (add_env_function ("system os unix environment", Posix.ProcEnv.environ);
     add_env_function ("system os unix setwd",setwd);
     add_env_function ("system os unix getwd",getwd);
     add_env_function ("system os unix realpath",realpath);
     add_env_function ("OS.FileSys.fullPath", OS.FileSys.fullPath);
     add_env_function ("POSIX.FileSys.O.append", SysWord.toInt (Posix.FileSys.O.toWord Posix.FileSys.O.append));
     add_env_function ("POSIX.FileSys.O.trunc", SysWord.toInt (Posix.FileSys.O.toWord Posix.FileSys.O.trunc));
     add_env_function ("POSIX.FileSys.ST.isdir", fn (s: stat) => #mode s = 0);
     add_env_function ("POSIX.FileSys.access", Posix.FileSys.access);
     add_env_function ("POSIX.FileSys.createf", fn (s: string, om: int, f: int, m: int) =>
                                                  Posix.FileSys.createf (s,
                                                                         case om of 0 => Posix.FileSys.O_RDONLY
                                                                                  | _ => Posix.FileSys.O_WRONLY,
                                                                         Posix.FileSys.O.fromWord (SysWord.fromInt f),
                                                                         Posix.FileSys.S.fromWord (SysWord.fromInt m)));
     add_env_function ("POSIX.FileSys.fstat", wrapStat o Posix.FileSys.fstat);
     add_env_function ("POSIX.FileSys.getcwd", Posix.FileSys.getcwd);
     add_env_function ("POSIX.FileSys.mkdir", fn (s: string, m: int) =>
                                                Posix.FileSys.mkdir (s, Posix.FileSys.S.fromWord (SysWord.fromInt m)));
     add_env_function ("POSIX.FileSys.openf", fn (s: string, om: int, f: int) =>
                                                Posix.FileSys.openf (s,
                                                                     case om of 0 => Posix.FileSys.O_RDONLY
                                                                              | _ => Posix.FileSys.O_WRONLY,
                                                                     Posix.FileSys.O.fromWord (SysWord.fromInt f)));
     add_env_function ("POSIX.FileSys.stat", wrapStat o Posix.FileSys.stat);
     add_env_function ("POSIX.FileSys.unlink", Posix.FileSys.unlink);
     add_env_function ("Time.-", fn _ => TIME (0, 0, 0));
     add_env_function ("Time.fromReal", fn _ => TIME (0, 0, 0));
     add_env_function ("Time.toReal", fn _ => 0.0))

  exception UnimplementedEnv of string
  fun unimplemented name =
    (TextIO.output (TextIO.stdOut, "unimplemented env function: " ^ name ^ "\n");
     raise UnimplementedEnv name)

in
  fun nj_environment e =
    let
      fun lookup [] = tcast (fn _ => unimplemented ("Environment function " ^ e))
        | lookup ((a,b)::c) =
          if e = a then b else lookup c
    in
      lookup (!env_refs)
    end
end
