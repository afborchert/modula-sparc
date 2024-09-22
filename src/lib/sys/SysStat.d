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

DEFINITION MODULE SysStat;

   FROM SystemTypes IMPORT TIME, OFF;

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
      (* IF Ifxxx = mode * FileType *)
      IfDir = { 1 };      (* directory *)
      IfChr = { 2 };      (* character special *)
      IfBlk = { 1..2 };   (* block special *)
      IfReg = { 0 };      (* regular *)
      IfLnk = { 0,2 };	  (* symbolic link *)
      IfSock = { 0..1 };  (* socket *)
      IfFifo = { 3 };     (* fifo *)
      (* IF Isxxx <= mode THEN *)
      IsUid =  { 4 };     (* set user id on execution *)
      IsGid =  { 5 };     (* set group id on execution *)
      IsVtx =  { 6 };     (* save swapped text even after use *)
      (* permissions on file: IF ... <= mode *)
      OwnerRead = { 7 };  (* read permission, owner *)
      OwnerWrite = { 8 }; (* write permission, owner *)
      OwnerExec = { 9 };  (* execute/search permission, owner *)
      GroupRead = { 10 };
      GroupWrite = { 11 };
      GroupExec = { 12 };
      WorldRead = { 13 };
      WorldWrite = { 14 };
      WorldExec = { 15 };

   PROCEDURE Stat(file: ARRAY OF CHAR; VAR buf: StatBuf) : BOOLEAN;

   PROCEDURE Fstat(fd: CARDINAL; VAR buf: StatBuf) : BOOLEAN;

END SysStat.
