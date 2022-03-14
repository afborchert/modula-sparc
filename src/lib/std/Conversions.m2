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
   $Id: Conversions.m2,v 0.2 1997/02/28 15:49:57 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Conversions.m2,v $
   Revision 0.2  1997/02/28  15:49:57  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:22  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

(****************************************
*                                       *
*     Conversions:                      *
*                                       * 
*     Number to string conversion       *
*                                       * 
*     Version of 26.02.81               *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

IMPLEMENTATION MODULE Conversions;      (* LG *)

(* $T- *)

   TYPE 
      Basetype = (oct, dec, hex);

   PROCEDURE ConvertNumber(num, len: CARDINAL; btyp: Basetype; neg: BOOLEAN;
      VAR str: ARRAY OF CHAR);

      (* conversion of a number into a string of characters *)
      (* num must get the absolute value of the number      *)
      (* len is the minimal length of the generated string  *)
      (* neg means: "the number is negative" for btyp = dec *)

      CONST
	 NumberLen = 11;
      VAR 
         digits          : ARRAY [1..NumberLen] OF CHAR;
         base            : CARDINAL;
         cnt, ix, maxlen : CARDINAL;
         dig             : CARDINAL;

   BEGIN 
      FOR ix := 1 TO NumberLen DO 
         digits[ix] := '0'
      END;                              (* initialisation *)
      IF btyp = oct THEN 
         base := 10B;
      ELSIF btyp = dec THEN 
         base := 10;
      ELSIF btyp = hex THEN 
         base := 10H;
      END;
      cnt := 0;
      REPEAT 
         INC(cnt);
         dig := num MOD base;
         num := num DIV base;
         IF dig < 10 THEN 
            dig := dig + ORD('0');
         ELSE 
            dig := dig - 10 + ORD('A');
         END;
         digits[cnt] := CHR(dig);
      UNTIL num = 0;
      (* (* i don't like this *)
      IF btyp = oct THEN 
         cnt := 11;
      ELSIF btyp = hex THEN 
         cnt := 8;
      ELSIF neg THEN 
      *)
      IF neg THEN
         INC(cnt);
         digits[cnt] := '-';
      END;
      maxlen := HIGH(str) + 1;          (* get maximal length *)
      IF len > maxlen THEN 
         len := maxlen 
      END;
      IF cnt > maxlen THEN 
         cnt := maxlen 
      END;
      ix := 0;
      WHILE len > cnt DO 
         str[ix] := ' ';
         INC(ix);
         DEC(len);
      END;
      WHILE cnt > 0 DO 
         str[ix] := digits[cnt];
         INC(ix);
         DEC(cnt);
      END;
      IF ix < maxlen THEN 
         str[ix] := 0C 
      END;
   END ConvertNumber;

   PROCEDURE ConvertOctal(num, len: CARDINAL; VAR str: ARRAY OF CHAR);
   (* conversion of an octal number to a string *)
   BEGIN 
      ConvertNumber(num,len,oct,FALSE,str);
   END ConvertOctal;

   PROCEDURE ConvertHex(num, len: CARDINAL; VAR str: ARRAY OF CHAR);
   (* conversion of a hexadecimal number to a string *)
   BEGIN 
      ConvertNumber(num,len,hex,FALSE,str);
   END ConvertHex;

   PROCEDURE ConvertCardinal(num, len: CARDINAL; VAR str: ARRAY OF CHAR);
   (* conversion of a cardinal decimal number to a string *)
   BEGIN 
      ConvertNumber(num,len,dec,FALSE,str);
   END ConvertCardinal;

   PROCEDURE ConvertInteger(num: INTEGER; len: CARDINAL; VAR str: ARRAY OF 
      CHAR);
   (* conversion of an integer decimal number to a string *)
   BEGIN 
      IF num = MIN(INTEGER) THEN (* ABS(MIN(INTEGER)) = MIN(INTEGER) *)
	 (* assume 2-complement *)
	 ConvertNumber(ORD(MAX(INTEGER))+1,len,dec,num < 0,str);
      ELSE
	 ConvertNumber(ABS(num),len,dec,num < 0,str);
      END;
   END ConvertInteger;

END Conversions. 
