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
   $Id: SysGetuid.m2,v 0.2 1997/02/28 15:47:39 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysGetuid.m2,v $
   Revision 0.2  1997/02/28  15:47:39  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:34  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysGetuid;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT getuid, getgid;

   PROCEDURE Getuid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getuid, r0, r1) THEN END;
      RETURN r0
   END Getuid;

   PROCEDURE Geteuid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getuid, r0, r1) THEN END;	(*same as geteuid *)
      RETURN r1
   END Geteuid;

   PROCEDURE Getgid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getgid, r0, r1) THEN END;
      RETURN r0
   END Getgid;

   PROCEDURE Getegid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getgid, r0, r1) THEN END;	(*same as getegid *)
      RETURN r1
   END Getegid;

END SysGetuid.
