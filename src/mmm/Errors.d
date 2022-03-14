(* Ulm's Modula-2 System: Makefile Generator
   Copyright (C) 1987-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Makefile Generator for Modula-2 is free software; you can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   Ulm's Makefile Generator for Modula-2 is distributed in the hope
   that it will be useful, but WITHOUT ANY WARRANTY; without even the
   implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: Errors.d,v 0.1 1997/02/24 08:36:04 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Errors.d,v $
   Revision 0.1  1997/02/24  08:36:04  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Errors; (* AFB 3/87 *)

   PROCEDURE Warning(text1, text2: ARRAY OF CHAR);

   PROCEDURE Error(text1, text2: ARRAY OF CHAR);

   PROCEDURE Fatal(text1, text2: ARRAY OF CHAR);

   PROCEDURE Syntax(fn: ARRAY OF CHAR; line: CARDINAL; text: ARRAY OF CHAR);

   PROCEDURE Errors() : CARDINAL;

END Errors.
