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
   $Id: SysOpen.m2,v 0.2 1997/02/28 15:47:49 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysOpen.m2,v $
   Revision 0.2  1997/02/28  15:47:49  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:38  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysOpen;

   FROM SYSTEM IMPORT UNIXCALL, ADR, WORD;
   FROM Sys IMPORT open;
   FROM Errno IMPORT errno;
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
      IF UNIXCALL(open, r0, r1, ADR(Buf), oflag, mode) THEN
         fd := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END OpenCreat;

END SysOpen.
