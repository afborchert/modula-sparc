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
   $Id: SysSetuid.m2,v 0.2 1997/02/28 15:47:57 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysSetuid.m2,v $
   Revision 0.2  1997/02/28  15:47:57  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:42  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysSetuid;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT setuid, setgid;
   FROM Errno IMPORT errno;

   PROCEDURE Setuid(uid: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(setuid, r0, r1, uid, uid) THEN
	 RETURN TRUE
      ELSE
	 errno := r0;
	 RETURN FALSE
      END;
   END Setuid;

   PROCEDURE Setgid(gid: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(setgid, r0, r1, gid, gid) THEN
	 RETURN TRUE
      ELSE
	 errno := r0;
	 RETURN FALSE
      END;
   END Setgid;

END SysSetuid.
