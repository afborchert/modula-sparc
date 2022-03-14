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
   $Id: FtdIO.m2,v 0.2 1997/02/28 15:50:05 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: FtdIO.m2,v $
   Revision 0.2  1997/02/28  15:50:05  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:25  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE FtdIO;

   FROM SYSTEM IMPORT WORD, ADR;
   FROM StdIO IMPORT FILE, Fputc, Fgetc, Feof, Ferror;
   FROM ReadIntCard IMPORT Read, Type;
   FROM Conversions IMPORT ConvertInteger, ConvertCardinal;
   IMPORT ReadIntCard, StdIO;

   (* (* from definition module *)
   VAR Done: BOOLEAN;
       termCH: CHAR;
   *)
   CONST
      nl = 12C;
      tab = 11C;

   VAR
      fp: FILE;

   PROCEDURE ReadChar(VAR ch: CHAR);
   BEGIN
      IF NOT Fgetc(ch, fp) THEN ch := 0C END;
      termCH := ch;
   END ReadChar;

   PROCEDURE Blanks(fp: FILE; VAR n: CARDINAL; min: CARDINAL; VAR ok: BOOLEAN);
      (* if n > min then write n-min blanks *)
   BEGIN
      ok := TRUE;
      WHILE (n > min) AND ok DO
         FwriteChar(fp, " ");
         ok := Done;
         DEC(n);
      END;
   END Blanks;

   PROCEDURE FreadInt(f: FILE; VAR arg: INTEGER);
   BEGIN
      fp := f;
      Read(arg, int, ReadChar);
      Done := ReadIntCard.Done;
   END FreadInt;

   PROCEDURE FwriteInt(f: FILE; arg: INTEGER; w: CARDINAL);
      VAR
         field: ARRAY[0..10] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(f, w, HIGH(field)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertInteger(arg, w, field);
      FwriteString(f, field);
   END FwriteInt;

   PROCEDURE FreadCard(f: FILE; VAR arg: CARDINAL);
   BEGIN
      fp := f;
      Read(arg, card, ReadChar);
      Done := ReadIntCard.Done;
   END FreadCard;

   PROCEDURE FwriteCard(f: FILE; arg: CARDINAL; w: CARDINAL);
      VAR
         field: ARRAY[0..10] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(f, w, HIGH(field)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertCardinal(arg, w, field);
      FwriteString(f, field);
   END FwriteCard;

   PROCEDURE FreadString(f: FILE; VAR str: ARRAY OF CHAR);
      VAR ch: CHAR;
          index: CARDINAL;
   BEGIN
      Done := TRUE;
      WHILE Fgetc(ch, f) AND ((ch = ' ') OR (ch = tab)) DO END;
      IF Feof(f) OR Ferror(f) THEN Done := FALSE; RETURN END;
      index := 0;
      LOOP
         IF ch <= ' ' THEN
            EXIT
	 END;
	 str[index] := ch;
	 INC(index);
	 IF index > HIGH(str) THEN
	    EXIT
	 END;
         IF NOT Fgetc(ch, f) THEN
            Done := FALSE;
            EXIT
         END;
      END; (* LOOP *)
      termCH := ch;
      IF index <= HIGH(str) THEN str[index] := 0C END;
   END FreadString;

   PROCEDURE FwriteString(f: FILE; str: ARRAY OF CHAR);
      VAR cnt: CARDINAL;
   BEGIN
      cnt := 0;
      WHILE (cnt <= HIGH(str)) AND (str[cnt] <> 0C) DO
         INC(cnt);
      END;
      (* use Fwrite for efficiency *)
      Done := (cnt = 0) OR StdIO.Fwrite(ADR(str), SIZE(CHAR), cnt, f);
   END FwriteString;

   PROCEDURE FwriteLn(f: FILE);
   BEGIN
      Done := Fputc(nl, f);
   END FwriteLn;

   PROCEDURE Fread(f: FILE; VAR arr: ARRAY OF WORD);
      VAR cnt: CARDINAL;
   BEGIN
      cnt := 1;
      Done := StdIO.Fread(ADR(arr), SIZE(arr), cnt, f) AND (cnt = 1);
   END Fread;

   PROCEDURE Fwrite(f: FILE; arr: ARRAY OF WORD);
      VAR cnt: CARDINAL;
   BEGIN
      cnt := 1;
      Done := StdIO.Fwrite(ADR(arr), SIZE(arr), cnt, f) AND (cnt = 1);
   END Fwrite;

   PROCEDURE FreadWord(f: FILE; VAR w: WORD);
      VAR cnt: CARDINAL;
   BEGIN
      cnt := 1;
      Done := StdIO.Fread(ADR(w), SIZE(w), cnt, f) AND (cnt = 1);
   END FreadWord;

   PROCEDURE FwriteWord(f: FILE; w: WORD);
      VAR cnt: CARDINAL;
   BEGIN
      cnt := 1;
      Done := StdIO.Fwrite(ADR(w), SIZE(w), cnt, f) AND (cnt = 1);
   END FwriteWord;

   PROCEDURE FreadChar(f: FILE; VAR ch: CHAR);
   BEGIN
      Done := Fgetc(ch, f);
   END FreadChar;

   PROCEDURE FwriteChar(f: FILE; ch: CHAR);
   BEGIN
      Done := Fputc(ch, f);
   END FwriteChar;

END FtdIO.
