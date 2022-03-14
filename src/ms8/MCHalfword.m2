(* Ulm's Modula-2 Compiler    Solaris 2.x/SPARCv8
   Copyright (C) 1983-1996 Universitaet Ulm, SAI, 89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Modula-2 has been designed and developed by Niklaus Wirth
   at the Institut fuer Informatik, ETH Zuerich, Switzerland
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Compiler is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   Ulm's Modula-2 Compiler is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   $Id: MCHalfword.m2,v 0.1 1997/02/21 18:40:22 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCHalfword.m2,v $
   Revision 0.1  1997/02/21  18:40:22  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCHalfword; (* AFB 3/84 *)

   FROM StdIO IMPORT FILE, Fputc, Fgetc;
   FROM MCFatal IMPORT IOFault;

   PROCEDURE WriteHalfword(f: FILE; c: CARDINAL);
   BEGIN
      IF NOT Fputc(CHR(c DIV 400B), f) OR NOT Fputc(CHR(c MOD 400B), f) THEN
	 IOFault("WriteHalfword");
      END;
   END WriteHalfword;

   PROCEDURE ReadHalfword(f: FILE; VAR c: CARDINAL);
      VAR c1, c2: CHAR;
   BEGIN
      IF NOT Fgetc(c1, f) OR NOT Fgetc(c2, f) THEN
	 IOFault("ReadHalfword");
      END;
      c := ORD(c1)*400B + ORD(c2);
   END ReadHalfword;

END MCHalfword.
