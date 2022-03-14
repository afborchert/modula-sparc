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
   $Id: StrToNum.m2,v 0.2 1997/02/28 15:50:40 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: StrToNum.m2,v $
   Revision 0.2  1997/02/28  15:50:40  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:37  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE StrToNum; (* mh 5/85, afb 4/86, mh 2/97 *)

   FROM ASCII IMPORT tab, nl;

   TYPE
      CharSet = SET OF CHAR;

   CONST
      whitespace = CharSet{' ', tab, nl};

   PROCEDURE CharToDigit(ch: CHAR; base: CARDINAL; VAR d: CARDINAL): BOOLEAN;
   BEGIN
      IF ('0' <= ch) AND (ch <= '9') THEN
	 d := ORD(ch) - ORD('0');
	 RETURN d < base
      END;
      IF (base = 10H) AND ('A' <= CAP(ch)) AND (CAP(ch) <= 'F') THEN
	 d := ORD(CAP(ch)) - ORD('A') + 0AH;
	 RETURN TRUE
      END;
      RETURN FALSE
   END CharToDigit;

   PROCEDURE StrToCardinal(str: ARRAY OF CHAR; base: CARDINAL;
			   VAR card: CARDINAL): BOOLEAN;
      VAR
	 index, aux: CARDINAL;
   BEGIN
      index := 0;
      WHILE (index <= HIGH(str)) & (str[index] IN whitespace) DO
	 INC(index);
      END;
      IF (index <= HIGH(str)) & (str[index] = '+') THEN
	 INC(index);
      END;

      IF (index > HIGH(str)) OR ~CharToDigit(str[index], base, card) THEN
	 RETURN FALSE			(* no digit at all *)
      END;
      INC(index);
      WHILE (index <= HIGH(str)) & ~(str[index] IN whitespace + CharSet{0C}) DO
	 IF ~CharToDigit(str[index], base, aux) THEN
	    RETURN FALSE		(* bad syntax *)
	 END;
	 aux := base * card + aux;
	 IF aux < card THEN
	    RETURN FALSE		(* overflow *)
	 END;
	 card := aux;
	 INC(index);
      END;
      WHILE (index <= HIGH(str)) & (str[index] # 0C) DO
	 IF ~(str[index] IN whitespace) THEN
	    RETURN FALSE
	 END;
	 INC(index);
      END;
      RETURN TRUE
   END StrToCardinal;

   (* converts str to the INTEGER integ in analogue manner.
    * Required syntax of str here:  [+|-] digit {digit} .
    *)
   PROCEDURE StrToInt(str: ARRAY OF CHAR; VAR integ: INTEGER): BOOLEAN;
      VAR
	 index, digit: CARDINAL;
	 aux: INTEGER;
	 neg: BOOLEAN;
   BEGIN
      index := 0;
      WHILE (index <= HIGH(str)) & (str[index] IN whitespace) DO
	 INC(index);
      END;
      neg := FALSE;
      IF index <= HIGH(str) THEN
	 IF str[index] = '+' THEN
	    INC(index);
	 ELSIF str[index] = '-' THEN
	    neg := TRUE;
	    INC(index);
	 END;
      END;

      IF (index > HIGH(str)) OR ~CharToDigit(str[index], 10, digit) THEN
	 RETURN FALSE			(* no digit at all *)
      END;
      INC(index);
      IF neg THEN
	 integ := -INTEGER(digit);
      ELSE
	 integ := digit;
      END;
      WHILE (index <= HIGH(str)) & ~(str[index] IN whitespace + CharSet{0C}) DO
	 IF ~CharToDigit(str[index], 10, digit) THEN
	    RETURN FALSE		(* bad syntax *)
	 END;
	 IF neg THEN
	    aux := 10 * integ - INTEGER(digit);
	    IF aux > integ THEN
	       RETURN FALSE		(* overflow *)
	    END;
	 ELSE
	    aux := 10 * integ + INTEGER(digit);
	    IF aux < integ THEN
	       RETURN FALSE		(* overflow *)
	    END;
	 END;
	 integ := aux;
	 INC(index);
      END;
      WHILE (index <= HIGH(str)) & (str[index] # 0C) DO
	 IF ~(str[index] IN whitespace) THEN
	    RETURN FALSE
	 END;
	 INC(index);
      END;
      RETURN TRUE
   END StrToInt;

   (* converts str to the CARDINAL card. Leading spaces, tabs and new-
    * lines are ignored. Returns FALSE if str is not of the syntax:
    *   [+] digit {digit}
    * or if the resulting number exceeds CARDINAL range.
    *)
   PROCEDURE StrToCard(str: ARRAY OF CHAR; VAR card: CARDINAL): BOOLEAN;
   BEGIN
      RETURN StrToCardinal(str, 10, card)
   END StrToCard;

   PROCEDURE StrToOct(str: ARRAY OF CHAR; VAR card: CARDINAL) : BOOLEAN;
   BEGIN
      RETURN StrToCardinal(str, 10B, card)
   END StrToOct;

   PROCEDURE StrToHex(str: ARRAY OF CHAR; VAR card: CARDINAL) : BOOLEAN;
   BEGIN
      RETURN StrToCardinal(str, 10H, card)
   END StrToHex;

END StrToNum.
