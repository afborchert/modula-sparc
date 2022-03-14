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
   $Id: SysPerror.m2,v 0.3 1997/02/28 15:50:44 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysPerror.m2,v $
   Revision 0.3  1997/02/28  15:50:44  borchert
   header fixed

   Revision 0.2  1997/02/28  15:48:08  borchert
   implementation takes now advantage of Errno.message

   Revision 0.1  1997/02/21  19:18:38  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysPerror; (* AFB 2/84 *)

   FROM Errno IMPORT errno, message, maxerror;
   FROM FtdIO IMPORT FwriteString, FwriteInt, FwriteLn;
   FROM StdIO IMPORT stderr;
   FROM Strings IMPORT StrCpy;

   CONST
      unknownError = "unknown error code";

   PROCEDURE Perror(str: ARRAY OF CHAR);
   BEGIN
      FwriteString(stderr, str);
      FwriteString(stderr, ": ");
      IF (errno <= maxerror) & (message[errno][0] # 0C) THEN
	 FwriteString(stderr, message[errno]);
      ELSE
	 FwriteString(stderr, unknownError);
	 FwriteString(stderr, " (");
	 FwriteInt(stderr, errno, 1);
	 FwriteString(stderr, ")");
      END;
      FwriteLn(stderr);
   END Perror;

   PROCEDURE GetErrorString(errno: CARDINAL; VAR buf: ARRAY OF CHAR);
   BEGIN
      IF (errno <= maxerror) & (message[errno][0] # 0C) THEN
	 StrCpy(buf, message[errno]);
      ELSE
	 StrCpy(buf, unknownError);
      END;
   END GetErrorString;

END SysPerror.
