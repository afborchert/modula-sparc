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
   $Id: Suffix.m2,v 0.1 1997/02/24 08:36:20 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Suffix.m2,v $
   Revision 0.1  1997/02/24  08:36:20  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Suffix; (* 3/87 *)

   FROM FileNames IMPORT maxsuffix;
   FROM SymTab IMPORT SourceKind;
   FROM Errors IMPORT Fatal;
   FROM Strings IMPORT StrCmp, StrCpy;

   PROCEDURE SuffixToSourceKind(suffix: ARRAY OF CHAR) : SourceKind;
      (* aborts on unknown suffixes *)
   BEGIN
      IF StrCmp(suffix, Sm2impl) = 0 THEN RETURN m2impl
      ELSIF StrCmp(suffix, Sm2def) = 0 THEN RETURN m2def
      ELSIF StrCmp(suffix, Smrimpl) = 0 THEN RETURN mrimpl
      ELSIF StrCmp(suffix, Smrdef) = 0 THEN RETURN mrdef
      ELSE Fatal(suffix, "unknown suffix")
      END;
   END SuffixToSourceKind;

   PROCEDURE SourceKindToSuffix(sourcekind: SourceKind;
				VAR suffix: ARRAY OF CHAR);
      (* NOT the inversion of SuffixToSourceKind; but the *)
      (* suffix of the output file after compiling a source *)
      (* of that kind *)
   BEGIN
      CASE sourcekind OF
      | m2impl:	StrCpy(suffix, Om2impl);
      | m2def:	StrCpy(suffix, Om2def);
      | m2main:	StrCpy(suffix, Om2main);
      | mrimpl:	StrCpy(suffix, Omrimpl);
      | mrdef:	StrCpy(suffix, Omrdef);
      | mrmain:	StrCpy(suffix, Omrmain);
      END;
   END SourceKindToSuffix;

END Suffix.
