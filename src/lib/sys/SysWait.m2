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

IMPLEMENTATION MODULE SysWait;

   FROM Sys IMPORT waitid;
   FROM Errno IMPORT errno;
   FROM SystemTypes IMPORT ALL;
   FROM SYSTEM IMPORT ADR, UNIXCALL;

   CONST
      (* see /usr/include/sys/wait.h *)
      WEXITED = 1;
      WTRAPPED = 2;

   PROCEDURE Wait(VAR pid, status: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      (* see https://github.com/Arquivotheca/solaris11/blob/trunk/usr/src/lib/libc/port/gen/waitpid.c *)
      IF UNIXCALL(waitid, r0, r1, ALL, -1, NIL, WEXITED + WTRAPPED) THEN
         pid := r0;
	 status := r1;
         RETURN TRUE
      ELSE
         errno := r0;
	 RETURN FALSE
      END;
   END Wait;

END SysWait.
