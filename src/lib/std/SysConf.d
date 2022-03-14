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
   $Id: SysConf.d,v 0.1 1997/03/04 19:31:24 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysConf.d,v $
   Revision 0.1  1997/03/04  19:31:24  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE SysConf; (* AFB 2/97 *)

   (* configuration parameters of the installation *)

   PROCEDURE GetLibDir(VAR libdir: ARRAY OF CHAR);
      (* GetLibDir returns the directory where the Modula-2 library
         has been installed to; it does not honour the MODLIB
	 environment variable
      *)

   PROCEDURE GetRelease(VAR release: ARRAY OF CHAR);
      (* returns the release of the Modula-2 installation *)

END SysConf.
