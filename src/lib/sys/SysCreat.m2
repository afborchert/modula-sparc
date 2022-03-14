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
   $Id: SysCreat.m2,v 0.2 1997/02/28 15:47:28 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysCreat.m2,v $
   Revision 0.2  1997/02/28  15:47:28  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:30  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysCreat;

   FROM Errno IMPORT errno;
   FROM Sys IMPORT creat;
   FROM UnixString IMPORT Buffer, Copy;
   FROM SYSTEM IMPORT UNIXCALL, ADR;

   PROCEDURE Creat(VAR fd: CARDINAL; filename: ARRAY OF CHAR;
                   mode: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
          Buf: Buffer;
   BEGIN
      Copy(Buf, filename);
      IF UNIXCALL(creat, r0, r1, ADR(Buf), mode) THEN
         fd := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Creat;

END SysCreat.
