(* Ulm's Modula-2 Library
   Copyright (C) 1984-1999 by University of Ulm, SAI, D-89069 Ulm, Germany
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
   $Id$
   ----------------------------------------------------------------------------
   $Log$
   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE StrToReal;

   FROM RealConv IMPORT Done, ReadReal;
   FROM Strings IMPORT StrCpy;

   CONST
      buflen = 512;
   VAR
      readbuf: ARRAY [0..buflen-1] OF CHAR;
      index: [0..buflen];

   PROCEDURE Read(VAR ch: CHAR);
      (* return characters from read buffer *)
   BEGIN
      IF index <= HIGH(readbuf) THEN
	 ch := readbuf[index]; INC(index);
      ELSE
	 ch := 0C;
      END;
   END Read;

   PROCEDURE StrToReal(str: ARRAY OF CHAR; VAR real: REAL) : BOOLEAN;
      (* converts str to the REAL real, leading white space is
	 ignored, returns FALSE if str does not conform to following
	 syntax:
	    ["+" | "-"] digit { digit } ["." digit { digit } ]
	    ["E" ["+" | "-"] digit [digit] ]
      *)
   BEGIN
      StrCpy(readbuf, str); index := 0; (* setup read buffer *)
      ReadReal(Read, real);
      RETURN Done
   END StrToReal;

END StrToReal.
