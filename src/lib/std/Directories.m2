(* Ulm's Modula-2 Library
   Copyright (C) 1984-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.

   Ulm's Modula-2 Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: Directories.m2,v 0.3 1997/02/28 15:49:58 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Directories.m2,v $
   Revision 0.3  1997/02/28  15:49:58  borchert
   header fixed

   Revision 0.2  1997/02/21  19:29:13  borchert
   removal of old copyright

   Revision 0.1  1997/02/21  19:18:22  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Directories;

   (* this version bases on getdents(2) and
      works for Solaris 2.x
      but possibly not for other releases
   *)

   FROM Sys IMPORT getdents;
   FROM SysClose IMPORT Close;
   FROM SysLseek IMPORT Lseek;
   FROM SysOpen IMPORT Open;
   FROM SysStat IMPORT StatBuf, Fstat, IfDir;
   FROM SystemTypes IMPORT DirSize, OFF, rdonly;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM Strings IMPORT StrCpy, StrCat;
   FROM SYSTEM IMPORT UNIXCALL, ADR, ADDRESS;

   (* (* exported from definition module *)
   TYPE 
      FileName = ARRAY [0..DirSize-1] OF CHAR;
      Direct = 
         RECORD 
            ino: CARDINAL;
            name: FileName;
         END;
   *)

   TYPE
      Entry = POINTER TO EntryRec;
      EntryRec =
	 RECORD
	    fileno: CARDINAL;
	    offset: OFF;
	    reclen1, reclen2: CHAR;	(* 2-byte record length *)
	    name1, name2: CHAR;         (* first two chars of name *)
	    name: FileName;		(* 0C-terminated *)
	 END;
      DIR = POINTER TO DirRec;
      DirRec =
	 RECORD
	    fd: CARDINAL;
	    blocksize: OFF;	(* blocksize *)
	    nbytes: OFF;	(* number of bytes returned *)
	    offset: OFF;	(* current offset in block *)
	    block: ADDRESS;	(* current block of directory *)
	    entry: Entry;	(* current entry *)
	    basep: OFF;
	 END;

   PROCEDURE OpenDir(VAR dirp: DIR; filename: ARRAY OF CHAR) : BOOLEAN;
      VAR
	 filedesc: CARDINAL;
	 statbuf: StatBuf;
   BEGIN
      IF Open(filedesc, filename, rdonly) AND Fstat(filedesc, statbuf) AND
	 (statbuf.mode * IfDir = IfDir) THEN
	 NEW(dirp);
	 WITH dirp^ DO
	    fd := filedesc;
	    blocksize := statbuf.blksize;
	    ALLOCATE(block, blocksize);
	    entry := NIL;
	    basep := 0;
	 END;
	 RETURN TRUE
      ELSE
	 RETURN FALSE
      END;
   END OpenDir;

   PROCEDURE ReadDir(dirp: DIR; VAR direct: Direct) : BOOLEAN;
      VAR
	 d0, d1: CARDINAL;
	 reclen: CARDINAL;
   BEGIN
      WITH dirp^ DO
	 REPEAT
	    IF entry = NIL THEN
	       IF UNIXCALL(getdents, d0, d1, fd, block, blocksize) THEN
		  nbytes := d0;
		  IF nbytes = 0 THEN
		     RETURN FALSE
		  END;
		  entry := block; offset := 0;
	       ELSE
		  RETURN FALSE
	       END;
	    END;
	    WITH entry^ DO
	       direct.ino := fileno;
	       direct.name[0] := name1;
	       direct.name[1] := name2;
	       direct.name[2] := 0C;
	       IF (name1 # 0C) & (name2 # 0C) THEN
		  StrCat(direct.name, name);
	       END;
	       reclen := ORD(reclen1) * 100H + ORD(reclen2);
	    END;
	    INC(offset, reclen);
	    IF offset < nbytes THEN
	       entry := block + ORD(offset);
	    ELSE
	       entry := NIL;
	    END;
	 UNTIL direct.ino # 0;
	 RETURN TRUE
      END;
   END ReadDir;

   PROCEDURE TellDir(dirp: DIR; VAR offset: OFF) : BOOLEAN;
   BEGIN
      WITH dirp^ DO
	 offset := basep + offset;
      END;
      RETURN TRUE
   END TellDir;

   PROCEDURE SeekDir(dirp: DIR; pos: OFF) : BOOLEAN;
   BEGIN
      WITH dirp^ DO
	 IF (pos >= basep) & (pos <= basep + nbytes) THEN
	    offset := pos - basep;
	 ELSIF Lseek(fd, pos, 0) THEN
	    basep := pos;
	    entry := NIL;
	 ELSE
	    RETURN FALSE
	 END;
      END;
      RETURN TRUE
   END SeekDir;

   PROCEDURE RewindDir(dirp: DIR) : BOOLEAN;
   BEGIN
      RETURN SeekDir(dirp, 0)
   END RewindDir;

   PROCEDURE CloseDir(VAR dirp: DIR);
   BEGIN
      WITH dirp^ DO
	 IF NOT Close(fd) THEN END;
	 DEALLOCATE(block, blocksize);
      END;
      DISPOSE(dirp);
   END CloseDir;

END Directories.
