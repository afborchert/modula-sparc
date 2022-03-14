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
   $Id: Suffix.d,v 0.1 1997/02/24 08:36:10 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Suffix.d,v $
   Revision 0.1  1997/02/24  08:36:10  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Suffix; (* 3/87 *)

   FROM FileNames IMPORT maxsuffix;
   FROM SymTab IMPORT SourceKind;

   CONST
      (* suffixes of source files *)
      Sm2impl = "m2"; Sm2def  = "d";
      Smrimpl = "mr"; Smrdef  = "dr";

      (* suffixes of compiler output files *)
      Om2impl = "o";  Om2def  = "sy"; Om2main = "o";
      Omrimpl = "o";  Omrdef  = "sy"; Omrmain = "o";

      Archive = "a";
      Ref = "r";


   PROCEDURE SuffixToSourceKind(suffix: ARRAY OF CHAR) : SourceKind;
      (* aborts on unknown suffixes *)

   PROCEDURE SourceKindToSuffix(sourcekind: SourceKind;
				VAR suffix: ARRAY OF CHAR);
      (* NOT the inversion of SuffixToSourceKind; but the *)
      (* suffix of the output file after compiling a source *)
      (* of that kind *)

END Suffix.
