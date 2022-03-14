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
   $Id: Scan.m2,v 0.1 1997/02/24 08:36:18 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Scan.m2,v $
   Revision 0.1  1997/02/24  08:36:18  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Scan; (* 3/87 *)

   FROM FileNames IMPORT FileName, filenmlen, ConstructFileName;
   FROM Errors IMPORT Fatal;
   FROM StdIO IMPORT FILE, Fopen, Fclose, read, stderr, Feof, Fgetc, Fungetc;
   FROM ASCII IMPORT tab, nl;
   FROM Strings IMPORT StrCmp;
   FROM SysPerror IMPORT Perror;

   (* (* from definition module *)
   TYPE
      Symbol =
	 (modulesy, definitionsy, implementationsy,
	 fromsy, importsy, systemsy, ident, sem, eop, illegal);
   *)

   VAR
      fp: FILE;
      fileopen: BOOLEAN;
      eof: BOOLEAN;

   PROCEDURE Read(VAR ch: CHAR) : BOOLEAN;
   BEGIN
      IF eof THEN RETURN FALSE END;
      IF NOT Fgetc(ch, fp) THEN eof := TRUE; RETURN FALSE END;
      IF ch = nl THEN INC(line) END;
      RETURN TRUE;
   END Read;

   PROCEDURE ReadAgain(ch: CHAR);
   BEGIN
      IF NOT Fungetc(ch, fp) THEN END;
   END ReadAgain;

   PROCEDURE ReadIdent(VAR id: Identifier) : BOOLEAN;
      TYPE
	 CharSet = SET OF CHAR;
      CONST
	 WhiteSpace = CharSet{' ', tab, nl};
	 AlphaNum = CharSet{'a'..'z', 'A'..'Z', '0'..'9'};

      VAR
	 ch: CHAR;
	 index: CARDINAL; (* into id *)

      PROCEDURE SkipTo(stop: CHAR);
	 VAR ch: CHAR;
      BEGIN
	 WHILE Read(ch) AND (ch <> stop) DO END;
      END SkipTo;

      PROCEDURE CheckComment;
	 VAR ch: CHAR;
      BEGIN
	 IF Read(ch) AND (ch = '*') THEN
	    IF NOT Read(ch) THEN RETURN END;
	    LOOP
	       WHILE (ch <> '*') AND (ch <> '(') DO
		  IF NOT Read(ch) THEN RETURN END;
	       END;
	       CASE ch OF
	       | '(': CheckComment; IF NOT Read(ch) THEN RETURN END;
	       | '*': IF Read(ch) AND (ch = ')') THEN RETURN END;
	       END;
	    END;
	 ELSE
	    ReadAgain(ch);
	 END;
      END CheckComment;

   BEGIN
      LOOP
	 WHILE Read(ch) AND (ch IN WhiteSpace) DO END;
	 IF eof THEN RETURN FALSE END;
	 CASE ch OF
	 | '"', "'": SkipTo(ch);
	 | '(': CheckComment;
	 | ';', ',': id[0] := ch; id[1] := 0C; RETURN TRUE;
	 | 'a'..'z', 'A'..'Z':
	    id[0] := ch; index := 1;
	    WHILE Read(ch) AND (ch IN AlphaNum) DO
	       IF index <= HIGH(id) THEN
		  id[index] := ch;
	       END;
	       INC(index);
	    END;
	    IF index <= HIGH(id) THEN
	       id[index] := 0C;
	    END;
	    ReadAgain(ch);
	    RETURN TRUE;
	 ELSE
	 END;
      END;
   END ReadIdent;

   PROCEDURE OpenScan(fn: FileName);
      VAR
	 file: ARRAY [0..filenmlen-1] OF CHAR;
   BEGIN
      IF fileopen THEN
	 IF NOT Fclose(fp) THEN END;
	 fileopen := FALSE;
      END;
      ConstructFileName(file, fn);
      IF NOT Fopen(fp, file, read, (* buffered = *) TRUE) THEN
	 Fatal(file, "cannot open");
      END;
      fileopen := TRUE;
      eof := FALSE;
      line := 1;
   END OpenScan;

   PROCEDURE GetSy(VAR sy: Symbol; VAR id: Identifier);
   BEGIN
      IF NOT fileopen OR eof OR NOT ReadIdent(id) THEN
	 sy := eop;
	 id := "eof";
	 RETURN
      END;
      IF StrCmp(id, "MODULE") = 0 THEN sy := modulesy
      ELSIF StrCmp(id, "DEFINITION") = 0 THEN sy := definitionsy
      ELSIF StrCmp(id, "IMPLEMENTATION") = 0 THEN sy := implementationsy
      ELSIF StrCmp(id, "FROM") = 0 THEN sy := fromsy
      ELSIF StrCmp(id, "IMPORT") = 0 THEN sy := importsy
      ELSIF StrCmp(id, "SYSTEM") = 0 THEN sy := systemsy
      ELSIF StrCmp(id, ";") = 0 THEN sy := sem
      ELSIF StrCmp(id, ",") = 0 THEN sy := comma
      ELSIF StrCmp(id, "BEGIN") = 0 THEN sy := eop
      ELSIF StrCmp(id, "CONST") = 0 THEN sy := eop
      ELSIF StrCmp(id, "END") = 0 THEN sy := eop
      ELSIF StrCmp(id, "EXPORT") = 0 THEN sy := eop
      ELSIF StrCmp(id, "PROCEDURE") = 0 THEN sy := eop
      ELSIF StrCmp(id, "TRANSACTION") = 0 THEN sy := eop
      ELSIF StrCmp(id, "TYPE") = 0 THEN sy := eop
      ELSIF StrCmp(id, "VAR") = 0 THEN sy := eop
      ELSIF (id[0] >= 'a') AND (id[0] <= 'z') OR
	    (id[0] >= 'A') AND (id[0] <= 'Z') THEN
	 sy := ident;
      ELSE
	 sy := illegal;
      END;
   END GetSy;

BEGIN
   fileopen := FALSE;
END Scan.
