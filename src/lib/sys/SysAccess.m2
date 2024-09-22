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

IMPLEMENTATION MODULE SysAccess;

   FROM Errno IMPORT errno;
   FROM Sys IMPORT faccessat;
   FROM SystemTypes IMPORT atFDCWD;
   FROM UnixString IMPORT Buffer, Copy;
   FROM SYSTEM IMPORT UNIXCALL, ADR;

   PROCEDURE Access(filename: ARRAY OF CHAR; mode: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
          Buf: Buffer;
   BEGIN
      Copy(Buf, filename);
      IF UNIXCALL(faccessat, r0, r1, atFDCWD, ADR(Buf), mode, 0) THEN
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Access;

END SysAccess.
