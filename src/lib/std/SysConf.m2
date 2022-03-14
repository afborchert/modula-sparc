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
   $Id: SysConf.m2,v 0.2 1999/01/25 09:46:04 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysConf.m2,v $
   Revision 0.2  1999/01/25  09:46:04  borchert
   release updated

   Revision 0.1  1997/03/04  19:31:58  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysConf; (* AFB 2/97 *)

   (* configuration parameters of the installation *)

   FROM Strings IMPORT StrCpy;

   CONST
      (* === following parameters are updated automatically === *)
      libdir = "/usr/local/lib/modula";
      release = "3.0b6";
      (* === end of parameter section === *)

   PROCEDURE GetLibDir(VAR libdirBuf: ARRAY OF CHAR);
      (* GetLibDir returns the directory where the Modula-2 library
         has been installed to; it does not honour the MODLIB
	 environment variable
      *)
   BEGIN
      StrCpy(libdirBuf, libdir);
   END GetLibDir;

   PROCEDURE GetRelease(VAR releaseBuf: ARRAY OF CHAR);
      (* returns the release of the Modula-2 installation *)
   BEGIN
      StrCpy(releaseBuf, release);
   END GetRelease;

END SysConf.
