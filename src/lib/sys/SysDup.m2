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
   $Id: SysDup.m2,v 0.2 1997/02/28 15:47:29 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysDup.m2,v $
   Revision 0.2  1997/02/28  15:47:29  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:31  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysDup;

   FROM Sys IMPORT dup;
   FROM Errno IMPORT errno, EBADF;
   FROM SYSTEM IMPORT UNIXCALL;
   FROM SysClose IMPORT Close;
   FROM SysFcntl IMPORT Fcntl, dupfd;

   PROCEDURE Dup(fd: CARDINAL; VAR newfd: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(dup, r0, r1, fd) THEN
         newfd := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Dup;

   PROCEDURE Dup2(fd, newfd: CARDINAL) : BOOLEAN;
      VAR fd2: CARDINAL;
   BEGIN
      fd2 := newfd;
      IF NOT Close(fd2) THEN END;
      IF Fcntl(fd, dupfd, fd2) THEN
	 IF fd2 = newfd THEN
	    RETURN TRUE;
	 ELSE
	    errno := EBADF;
	    IF NOT Close(fd2) THEN END;
	    RETURN FALSE;
	 END;
      ELSE
	 RETURN FALSE;
      END;
   END Dup2;

END SysDup.
