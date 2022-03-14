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
   $Id: Out.m2,v 0.1 1997/02/24 08:36:18 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Out.m2,v $
   Revision 0.1  1997/02/24  08:36:18  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Out; (* 3/87 *)

   FROM Errors IMPORT Fatal, Error;
   FROM FileNames IMPORT filenmlen;
   FROM FtdIO IMPORT FwriteString, FwriteLn, FwriteChar;
   FROM StdIO IMPORT FILE, Fopen, Fclose, write, stdout;
   FROM ASCII IMPORT tab;
   FROM Strings IMPORT StrLen, StrCpy, StrCat;

   CONST
      linelength = 78;
   TYPE
      Buffer = ARRAY [0..linelength-1] OF CHAR;
   VAR
      fp: FILE; outfile: ARRAY[0..filenmlen-1] OF CHAR;
      column: CARDINAL;
      white: BOOLEAN;
      append: Buffer;

   PROCEDURE WriteString(text: ARRAY OF CHAR);
      VAR
	 len: CARDINAL;
	 breakok: BOOLEAN;
   BEGIN
      len := StrLen(text);
      IF len > 0 THEN
	 INC(column, len);
	 breakok := white OR (text[0] = " ");
	 IF breakok THEN
	    IF column >= linelength THEN
	       IF append[0] <> 0C THEN
		  FwriteString(fp, append);
		  append[0] := 0C;
	       END;
	       IF NOT white THEN FwriteChar(fp, " ") END;
	       FwriteString(fp, "\"); FwriteLn(fp); column := 1; Tab;
	       FwriteString(fp, text);
	       INC(column, len);
	    ELSE
	       FwriteString(fp, append);
	       StrCpy(append, text);
	    END;
	 ELSIF (column >= linelength) AND (append[0] <> 0C) THEN
	    IF append[0] = " " THEN FwriteChar(fp, " ") END;
	    FwriteString(fp, "\"); FwriteLn(fp); column := 1; Tab;
	    FwriteString(fp, append); append[0] := 0C;
	    FwriteString(fp, text);
	    INC(column, len + StrLen(append));
	 ELSE
	    StrCat(append, text);
	 END;
	 white := text[len-1] = " ";
      END;
   END WriteString;

   PROCEDURE WriteLn;
   BEGIN
      IF append[0] <> 0C THEN FwriteString(fp, append); append[0] := 0C END;
      FwriteLn(fp); column := 1; white := TRUE;
   END WriteLn;

   PROCEDURE Write(ch: CHAR);
      VAR array: ARRAY [0..0] OF CHAR;
   BEGIN
      IF ch = " " THEN
	 IF NOT white THEN
	    IF append[0] <> 0C THEN
	       FwriteString(fp, append);
	       append[0] := 0C;
	    END;
	    FwriteChar(fp, ch);
	    INC(column);
	 END;
	 white := TRUE;
      ELSE
	 array[0] := ch; WriteString(array);
      END;
   END Write;

   PROCEDURE Tab;
      CONST
	 tabs = 8;
	 tab1 = tabs+1;
	 tab2 = 2*tabs+1;
   BEGIN
      IF column < tab1 THEN
	 FwriteChar(fp, tab);
      END;
      IF column < tab2 THEN
	 FwriteChar(fp, tab);
	 column := tab2;
      ELSE
	 FwriteChar(fp, " ");
	 INC(column);
      END;
      white := TRUE;
   END Tab;

   PROCEDURE WriteTab;
   BEGIN
      IF append[0] <> 0C THEN FwriteString(fp, append); append[0] := 0C END;
      Tab;
   END WriteTab;

   PROCEDURE OpenMakefile(filename: ARRAY OF CHAR);
   BEGIN
      IF NOT Fopen(fp, filename, write, (* buffered = *) TRUE) THEN
	 Fatal(filename, "cannot open");
      END;
      StrCpy(outfile, filename);
   END OpenMakefile;

   PROCEDURE CloseMakefile;
   BEGIN
      IF NOT Fclose(fp) THEN
	 Error(outfile, "write error");
      END;
   END CloseMakefile;

BEGIN
   fp := stdout;
   column := 1;
   white := TRUE;
   append[0] := 0C;
END Out.
