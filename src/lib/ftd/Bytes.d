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
   $Id: Bytes.d,v 0.2 1997/02/28 15:59:41 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Bytes.d,v $
   Revision 0.2  1997/02/28  15:59:41  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:30  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Bytes; (* WS *)

   FROM SYSTEM IMPORT ADDRESS, WORD;

   PROCEDURE ByteNCopy(to,from : ADDRESS; no : CARDINAL);

   PROCEDURE PINC(VAR ptr : ADDRESS; inc : CARDINAL);

   PROCEDURE PDEC(VAR ptr : ADDRESS; dec : CARDINAL);

END Bytes.
