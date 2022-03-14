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
   $Id: InOut.m2,v 0.2 1997/02/28 15:50:12 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: InOut.m2,v $
   Revision 0.2  1997/02/28  15:50:12  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:27  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE InOut; (* stripped version: AFB 4/84 *)

   FROM Conversions IMPORT ConvertInteger, ConvertCardinal,
      ConvertOctal, ConvertHex;
   FROM ReadIntCard IMPORT int, card;
   IMPORT ReadIntCard;
   IMPORT Terminal;

   (* (* from definition module *)
   CONST
      EOL = 12C;
   VAR
      Done: BOOLEAN; (* on eof true *)
      termCH: CHAR; (* set in ReadString and numeric input procs *)

   *)

   CONST
      tab = 11C;

   PROCEDURE Read(VAR ch: CHAR);
   BEGIN
      Terminal.Read(ch);
      Done := Terminal.Done;
   END Read;

   PROCEDURE ReadString(VAR str: ARRAY OF CHAR);
      VAR ch: CHAR;
	  index: CARDINAL;
   BEGIN
      REPEAT
         Terminal.Read(ch);
         IF NOT Terminal.Done THEN str[0] := 0C; Done := FALSE; RETURN; END;
      UNTIL (ch <> ' ') AND (ch <> tab);
      index := 0;
      LOOP
         IF ch <= ' ' THEN
	    str[index] := 0C;
	    termCH := ch;
            Done := TRUE;
	    EXIT
	 END;
	 str[index] := ch;
	 INC(index);
	 IF index > HIGH(str) THEN
	    Terminal.Read(termCH);
            Done := Terminal.Done;
	    EXIT
	 END;
	 Terminal.Read(ch);
         IF NOT Terminal.Done THEN str[index] := 0C; Done := FALSE; RETURN; END;
      END; (* LOOP *)
   END ReadString;

   PROCEDURE ReadChar(VAR ch: CHAR);
   BEGIN
      Terminal.Read(ch);
      IF NOT Terminal.Done THEN ch := 0C END;
      termCH := ch;
   END ReadChar;

   PROCEDURE ReadCard(VAR arg: CARDINAL);
   BEGIN
      ReadIntCard.Read(arg, card, ReadChar);
      Done := ReadIntCard.Done;
   END ReadCard;

   PROCEDURE ReadInt(VAR arg: INTEGER);
   BEGIN
      ReadIntCard.Read(arg, int, ReadChar);
      Done := ReadIntCard.Done;
   END ReadInt;

   PROCEDURE Write(ch: CHAR);
   BEGIN
      Terminal.Write(ch);
      Done := Terminal.Done;
   END Write;

   PROCEDURE WriteLn;
   BEGIN
      Terminal.WriteLn;
      Done := Terminal.Done;
   END WriteLn;

   PROCEDURE WriteString(s: ARRAY OF CHAR);
   BEGIN
      Terminal.WriteString(s);
      Done := Terminal.Done;
   END WriteString;

   (* n: minimum field width *)

   PROCEDURE Blanks(VAR n: CARDINAL; min: CARDINAL; VAR ok: BOOLEAN);
      (* if n > min then write n-min blanks *)
   BEGIN
      ok := TRUE;
      WHILE (n > min) AND ok DO
         Terminal.Write(" ");
         ok := Terminal.Done;
         DEC(n);
      END;
   END Blanks;

   PROCEDURE WriteInt(x: INTEGER; n: CARDINAL);
      VAR
         buf: ARRAY[0..10] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(n, HIGH(buf)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertInteger(x, n, buf);
      Terminal.WriteString(buf);
      Done := Terminal.Done;
   END WriteInt;

   PROCEDURE WriteCard(x: CARDINAL; n: CARDINAL);
      VAR
         buf: ARRAY[0..10] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(n, HIGH(buf)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertCardinal(x, n, buf);
      Terminal.WriteString(buf);
      Done := Terminal.Done;
   END WriteCard;

   PROCEDURE WriteOct(x: CARDINAL; n: CARDINAL);
      VAR
         buf: ARRAY[0..10] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(n, HIGH(buf)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertOctal(x, n, buf);
      Terminal.WriteString(buf);
      Done := Terminal.Done;
   END WriteOct;

   PROCEDURE WriteHex(x: CARDINAL; n: CARDINAL);
      VAR
         buf: ARRAY[0..8] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(n, HIGH(buf)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertHex(x, n, buf);
      Terminal.WriteString(buf);
      Done := Terminal.Done;
   END WriteHex;

END InOut.
