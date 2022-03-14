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
   $Id: UnixString.d,v 0.3 1997/02/28 15:48:11 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: UnixString.d,v $
   Revision 0.3  1997/02/28  15:48:11  borchert
   header fixed

   Revision 0.2  1997/02/28  15:46:21  borchert
   comment changed

   Revision 0.1  1997/02/21  19:05:26  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE UnixString;

   CONST
      BufSiz = 512;
   TYPE
      Buffer = ARRAY[0..BufSiz-1] OF CHAR;

   PROCEDURE Copy(VAR buf: Buffer; str: ARRAY OF CHAR);
      (* copy str to Buf and guarantee 0C-termination;
         str is silently truncated on copying if necessary
      *)

END UnixString.
