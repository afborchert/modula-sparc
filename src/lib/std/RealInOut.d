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
   $Id: RealInOut.d,v 0.2 1997/02/28 15:50:29 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: RealInOut.d,v $
   Revision 0.2  1997/02/28  15:50:29  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:10  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE RealInOut; (* AFB 6/84 * rev. wsc 2/85 *)

   FROM StdIO IMPORT FILE;

   VAR
      Done: BOOLEAN;

   (*
    *	Read REAL number x according to syntax:
    *
    *	["+" | "-"] digit { digit } ["." digit { digit } ]
    *	["E" ["+" | "-"] digit [digit] ]
    *
    *	Done := "a number was read".
    *
    *	at most 16 digits are significant, leading zeroes not
    *	counting. Maximum exponent is 76. Input terminates
    *	with a blank or any control character.
    *)

   PROCEDURE ReadReal(VAR x: REAL);

   PROCEDURE FreadReal(f: FILE; VAR x: REAL);

   (*
    *	Write x using n characters. If fewer than n characters
    *	are needed, leading blanks are inserted.
    *)

   PROCEDURE WriteReal(x: REAL; n: CARDINAL);

   PROCEDURE FwriteReal(f: FILE; x: REAL; n: CARDINAL);

   (*
    *	Write x in fixed point notation using pd digits in front
    *	of decimal point and dp digits behind decial point. If
    *	fewer than pd digits are needed, leading blanks are
    *	inserted.
    *)

   PROCEDURE WriteFloat(x: REAL; pd: CARDINAL; dp: CARDINAL);

   PROCEDURE FwriteFloat(f: FILE; x: REAL; pd: CARDINAL; dp: CARDINAL);

   (*
    *	Write x in octal/hexadecimal form with exponent and mantissa
    *)

   PROCEDURE WriteRealOct(x: REAL);

   PROCEDURE FwriteRealOct(f: FILE; x: REAL);

   PROCEDURE WriteRealHex(x: REAL);

   PROCEDURE FwriteRealHex(f: FILE; x: REAL);

END RealInOut.
