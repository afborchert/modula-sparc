(* Ulm Modula-2 Library
   Copyright (C) 1984-2024 by Andreas F. Borchert
   ----------------------------------------------------------------------------
   Ulm Modula-2 Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.

   Ulm Modula-2 Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@andreas-borchert.de
   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysStat; (* Solaris 11 *)

   (* the implementation but not the interface
      depends on storage allocation of C *)

   FROM Errno IMPORT errno;
   FROM Sys IMPORT fstatat;
   FROM SystemTypes IMPORT TIME, OFF;
   FROM SystemTypes IMPORT atFDCWD;
   FROM UnixString IMPORT Copy, Buffer;
   FROM SYSTEM IMPORT UNIXCALL, ADR, ADDRESS, BYTE;

   (* (* from definition module *)
   TYPE
      (* this is not binary compatible with struct stat of C *)
      StatBuf =
         RECORD
            dev: CARDINAL;
            ino: CARDINAL;
            mode: BITSET;
            nlink: CARDINAL;
            uid: CARDINAL;
            gid: CARDINAL;
            rdev: CARDINAL;
            size: OFF;
            atime: TIME;
            mtime: TIME;
            ctime: TIME;
	    blksize : CARDINAL;
	    blocks : CARDINAL;
         END;
   CONST
      (* bit masks for mode; bits 0..15 used *)
      FileType = { 0..3 };
      (* IF mask * FileType = ... *)
      IfDir = { 1 };      (* directory *)
      IfChr = { 2 };      (* character special *)
      IfBlk = { 1..2 };   (* block special *)
      IfReg = { 0 };      (* regular *)
      IfLnk = { 0, 2 };   (* symbolic link *)
      IfSock = { 0..1 };  (* socket *)
      IfFifo = { 3 };     (* fifo *)
      (* IF ... <= mask THEN *)
      IsUid =  { 4 };     (* set user id on execution *)
      IsGid =  { 5 };     (* set group id on execution *)
      IsVtx =  { 6 };     (* save swapped text even after use *)
      (* permissions on file *)
      OwnerRead = { 7 };  (* read permission, owner *)
      OwnerWrite = { 8 }; (* write permission, owner *)
      OwnerExec = { 9 };  (* execute/search permission, owner *)
      GroupRead = { 10 };
      GroupWrite = { 11 };
      GroupExec = { 12 };
      WorldRead = { 13 };
      WorldWrite = { 14 };
      WorldExec = { 15 };
   *)

   (* following record definition has been taken from SysLib of
      the MOCKA compiler, Karlsruhe
   *)
   CONST
      STFSTYPSZ = 16;
   TYPE
      inoT      = LONGCARD;
      offT      = INTEGER;
      devT      = LONGCARD;
      timeT     = INTEGER;
      modeT     = LONGCARD;
      uidT      = LONGCARD;
      gidT      = uidT;
      nlinkT    = LONGCARD;
      StructStat =
         RECORD
            stDev    : devT;
            stPad1   : ARRAY [0..2] OF LONGCARD; (* reserved for network id *)
            stIno    : inoT;
            stMode   : modeT;
            stNlink  : nlinkT;
            stUid    : uidT;
            stGid    : gidT;
            stRdev   : devT;
            stPad2   : ARRAY [0..1] OF LONGCARD;
            stSize   : offT;
            stPad3   : LONGCARD;		(* future offT expansion *)
            stAtime  : timeT;
            stSpare1 : INTEGER;
            stMtime  : timeT;
            stSpare2 : INTEGER;
            stCtime  : timeT;
            stSpare3 : INTEGER;
            stBlksize: INTEGER;
            stBlocks : INTEGER;
            stFstype : ARRAY [0..STFSTYPSZ-1] OF CHAR;
            stPad4   : ARRAY [0..7] OF LONGCARD;
         END;

   PROCEDURE Expand(VAR from: StructStat; VAR to: StatBuf);
      VAR
	 card: CARDINAL;
   BEGIN
      WITH from DO
         WITH to DO
	    dev := stDev;
	    ino := stIno;
	    card := stMode; card := card * 10000H;
	    mode := BITSET(card);
	    nlink := stNlink;
	    uid := stUid;
	    gid := stGid;
	    rdev := stRdev;
	    size := stSize;
	    atime := stAtime;
	    mtime := stMtime;
	    ctime := stCtime;
	    blksize := stBlksize;
	    blocks := stBlocks;
         END;
      END;
   END Expand;

   (* see https://github.com/Arquivotheca/solaris11/blob/trunk/usr/src/lib/libc/port/sys/stat.c *)

   PROCEDURE Stat(file: ARRAY OF CHAR; VAR buf: StatBuf) : BOOLEAN;
      VAR sb : StructStat; Buf: Buffer; r0, r1: CARDINAL;
   BEGIN
      Copy(Buf, file);
      IF UNIXCALL(fstatat, r0, r1, atFDCWD, ADR(Buf), ADR(sb), 0) THEN
	 Expand(sb, buf);
         RETURN TRUE
      ELSE
	 errno := r0;
         RETURN FALSE
      END;
   END Stat;

   PROCEDURE Fstat(fd: CARDINAL; VAR buf: StatBuf) : BOOLEAN;
      VAR  sb: StructStat; r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(fstatat, r0, r1, fd, NIL, ADR(sb), 0) THEN
	 Expand(sb, buf);
         RETURN TRUE
      ELSE
	 errno := r0;
         RETURN FALSE
      END;
   END Fstat;

END SysStat.
