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
   $Id: StrToNum.d,v 0.2 1997/02/28 15:50:39 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: StrToNum.d,v $
   Revision 0.2  1997/02/28  15:50:39  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:14  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE StrToNum; (* mh 5/85; rev afb 4/86: StrToOct/StrToHex *)

   PROCEDURE StrToCard(str: ARRAY OF CHAR; VAR card: CARDINAL): BOOLEAN;
	(* converts str to the CARDINAL card. Leading spaces, tabs and new-
	 * lines are ignored. Returns FALSE if str is not of the syntax:
	 *   [+] {digit} , or if the resulting number exceeds CARDINAL range.
	 *)

   PROCEDURE StrToInt(str: ARRAY OF CHAR; VAR integ: INTEGER): BOOLEAN;
	(* converts str to the INTEGER integ in analogue manner.
	 * Required syntax of str here:  [+|-] {digit} .
	 *)

   PROCEDURE StrToOct(str: ARRAY OF CHAR; VAR card: CARDINAL) : BOOLEAN;

   PROCEDURE StrToHex(str: ARRAY OF CHAR; VAR card: CARDINAL) : BOOLEAN;

END StrToNum.
