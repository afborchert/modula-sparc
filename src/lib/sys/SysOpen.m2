(* Ulm Modula-2 Library
   Copyright (C) 1984-2024 by Andreas F. Borchert
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
   E-mail contact: modula@andreas-borchert.de
   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysOpen;

   FROM SYSTEM IMPORT UNIXCALL, ADR, WORD;
   FROM Errno IMPORT errno;
   FROM Sys IMPORT openat;
   FROM SystemTypes IMPORT atFDCWD;
   FROM UnixString IMPORT Copy, Buffer;

   (* mode = 0 : read, 1 : write, 2 : both *)

   PROCEDURE Open(VAR fd: CARDINAL; filename: ARRAY OF CHAR;
                  oflag: WORD) : BOOLEAN;
   BEGIN
      RETURN OpenCreat(fd, filename, oflag, 0);
   END Open;

   PROCEDURE OpenCreat(VAR fd: CARDINAL; filename: ARRAY OF CHAR;
		       oflag: WORD; mode: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
          Buf: Buffer;
   BEGIN
      Copy(Buf, filename);
      (* adapted to Solaris 11 where open has been replaced by openat *)
      IF UNIXCALL(openat, r0, r1, atFDCWD, ADR(Buf), oflag, mode) THEN
         fd := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END OpenCreat;

END SysOpen.
