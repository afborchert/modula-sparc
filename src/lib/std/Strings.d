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
   $Id: Strings.d,v 0.2 1997/02/28 15:50:41 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Strings.d,v $
   Revision 0.2  1997/02/28  15:50:41  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:15  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Strings; (* AFB 7/84 *)

   PROCEDURE StrLen(s: ARRAY OF CHAR) : CARDINAL;

   PROCEDURE StrCat(VAR s: ARRAY OF CHAR; s1: ARRAY OF CHAR);

   PROCEDURE StrCmp(s1, s2: ARRAY OF CHAR) : INTEGER;

   PROCEDURE StrCpy(VAR s: ARRAY OF CHAR; s1: ARRAY OF CHAR);

END Strings.