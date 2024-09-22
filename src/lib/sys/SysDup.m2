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

IMPLEMENTATION MODULE SysDup;

   FROM Errno IMPORT errno, EBADF;
   FROM SysClose IMPORT Close;
   FROM SysFcntl IMPORT Fcntl, dupfd, dup2fd;
   FROM SYSTEM IMPORT UNIXCALL;

   PROCEDURE Dup(fd: CARDINAL; VAR newfd: CARDINAL) : BOOLEAN;
   BEGIN
      RETURN Fcntl(fd, dupfd, newfd);
   END Dup;

   PROCEDURE Dup2(fd, newfd: CARDINAL) : BOOLEAN;
      VAR fd2: CARDINAL;
   BEGIN
      RETURN Fcntl(fd, dup2fd, newfd);
   END Dup2;

END SysDup.
