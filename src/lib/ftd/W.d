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
   $Id: W.d,v 0.2 1997/02/28 16:00:13 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: W.d,v $
   Revision 0.2  1997/02/28  16:00:13  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:38  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

(* 
 *    W - formatted output to windows (ws 3/89)
 *    =========================================
 *)

DEFINITION MODULE W;

   FROM SYSTEM IMPORT BYTE;
   FROM Printf IMPORT FmtExitCode;
   FROM Windows IMPORT Window;

   PROCEDURE done () : BOOLEAN;

   PROCEDURE success() : FmtExitCode;

   PROCEDURE setmode (mode : BITSET);

   PROCEDURE getmode (VAR mode : BITSET);

   PROCEDURE printf0 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR);

   PROCEDURE printf1 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1 : ARRAY OF BYTE);

   PROCEDURE printf2 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1,i2 : ARRAY OF BYTE);

   PROCEDURE printf3 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1,i2, i3 : ARRAY OF BYTE );

   PROCEDURE printf4 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1, i2, i3, i4 : ARRAY OF BYTE);

   PROCEDURE printf5 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1, i2, i3, i4, i5 : ARRAY OF BYTE);

   PROCEDURE printf6 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1, i2, i3, i4, i5, i6 : ARRAY OF BYTE);

   PROCEDURE printf7 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1, i2, i3, i4, i5, i6, i7 : ARRAY OF BYTE);

   PROCEDURE printf8 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1, i2, i3, i4, i5, i6, i7 , i8 : ARRAY OF BYTE);

   PROCEDURE append0 (w : Window; fmt : ARRAY OF CHAR);

   PROCEDURE append1 (w : Window; fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   PROCEDURE append2 (w : Window; fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF BYTE);

   PROCEDURE append3 (w : Window; fmt : ARRAY OF CHAR; i1,i2, i3 : ARRAY OF 
      BYTE );

   PROCEDURE append4 (w : Window; fmt : ARRAY OF CHAR; i1, i2, i3, i4 : 
      ARRAY OF BYTE);

   PROCEDURE append5 (w : Window; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5 : 
      ARRAY OF BYTE);

   PROCEDURE append6 (w : Window; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6 
      : ARRAY OF BYTE);

   PROCEDURE append7 (w : Window; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6,
      i7 : ARRAY OF BYTE);

   PROCEDURE append8 (w : Window; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6,
      i7 , i8 : ARRAY OF BYTE);

END W. 
