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
   $Id: P.d,v 0.2 1997/02/28 15:59:57 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: P.d,v $
   Revision 0.2  1997/02/28  15:59:57  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:35  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

(* 
 *    P - formatted output to stdout  (ws 6/88)
 *    =========================================
 *)

DEFINITION MODULE P;

   FROM Printf IMPORT FmtExitCode;
   FROM SYSTEM IMPORT BYTE;

   (* --- diagnostic --- *)

   PROCEDURE done () : BOOLEAN;

   PROCEDURE success() : FmtExitCode;

   (* --- output procedures --- *)

   PROCEDURE rintf0 (fmt : ARRAY OF CHAR);

   PROCEDURE rintf1 (fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   PROCEDURE rintf2 (fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF BYTE);

   PROCEDURE rintf3 (fmt : ARRAY OF CHAR; i1,i2, i3 : ARRAY OF BYTE );

   PROCEDURE rintf4 (fmt : ARRAY OF CHAR; i1, i2, i3, i4 : ARRAY OF BYTE);

   PROCEDURE rintf5 (fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5 : ARRAY OF BYTE);

   PROCEDURE rintf6 (fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6 : ARRAY OF 
      BYTE);

   PROCEDURE rintf7 (fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6, i7 : 
      ARRAY OF BYTE);

   PROCEDURE rintf8 (fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6, i7 , i8 : 
      ARRAY OF BYTE);

   (* --- set mode for errorhandling (default is "FmtError.Default") --- *)

   PROCEDURE setmode (mode : BITSET);

   PROCEDURE getmode (VAR mode : BITSET);

END P. 
