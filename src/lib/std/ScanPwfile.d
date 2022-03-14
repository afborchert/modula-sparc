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
   $Id: ScanPwfile.d,v 0.2 1997/02/28 15:50:30 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: ScanPwfile.d,v $
   Revision 0.2  1997/02/28  15:50:30  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:11  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE ScanPwfile;
(*
 *	utility functions for modules Passwd and EtcGroup
 *
 *	Martin Hasch, University of Ulm, Dec-06-1988
 *)

   FROM StdIO IMPORT FILE;
   FROM ASCII IMPORT nl;

   CONST
      fieldsep = ":";
      linesep = nl;

   PROCEDURE ReRead(f: FILE): BOOLEAN;

   PROCEDURE GetText(f: FILE; VAR text: ARRAY OF CHAR; sepchar: CHAR): BOOLEAN;

   PROCEDURE GetNumber(f: FILE; VAR number: CARDINAL; sepchar: CHAR): BOOLEAN;

END ScanPwfile.
