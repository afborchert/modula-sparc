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
   $Id: Chars.m2,v 0.2 1997/02/28 15:59:43 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Chars.m2,v $
   Revision 0.2  1997/02/28  15:59:43  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:39  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Chars; (* WS *)

   PROCEDURE Lower (VAR ch : CHAR);

   BEGIN
      IF ch IN UpperS THEN
	 ch := CHR(ORD(ch) + ORD('a') - ORD('A'));
      END;
   END Lower;

   PROCEDURE Upper (VAR ch : CHAR);

   BEGIN
      IF ch IN LowerS THEN
	 ch := CHR(ORD(ch) + ORD('A') - ORD('a'));
      END;
   END Upper;

   PROCEDURE Class(ch : CHAR) : CharClass;

   BEGIN 
      CASE ch OF 
      | 'a'..'z','A'..'Z': 
            RETURN letter;
      | '0' ..'9' : 
            RETURN digit;
      | nul : 
            RETURN nullc;
      ELSE 
         IF ch IN PunctS THEN 
            RETURN punct 
         ELSIF ch IN SpaceS THEN 
            RETURN space;
	 ELSIF ch IN ControlS THEN
	    RETURN control
         ELSE 
            RETURN nonascii 
         END;
      END;

   END Class;

END Chars. 
