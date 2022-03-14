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
   $Id: Keywords.d,v 0.2 1997/02/28 15:59:49 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Keywords.d,v $
   Revision 0.2  1997/02/28  15:59:49  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:33  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Keywords; (* WS 5/88 *)

   VAR
      ok : BOOLEAN;

   PROCEDURE DefineKey (string: ARRAY OF CHAR; key: INTEGER);
      (* Define string to be a keyword with value key *)

   PROCEDURE IsKey (string: ARRAY OF CHAR; VAR key: INTEGER) : BOOLEAN;
      (* Returns TRUE if string is a keyword; key holds the value *)

END Keywords. 
