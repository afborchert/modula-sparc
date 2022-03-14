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
   $Id: SysPanic.m2,v 0.2 1997/02/28 15:47:50 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysPanic.m2,v $
   Revision 0.2  1997/02/28  15:47:50  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:39  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysPanic;

   (* don't use the StdIO or FtdIO (else reference cycle problems) *)

   FROM SysWrite IMPORT Write;
   FROM SysExit IMPORT Exit;
   FROM SYSTEM IMPORT ADR;

   PROCEDURE Panic(text: ARRAY OF CHAR);
      CONST
         stderr = 2;
      VAR
         index: CARDINAL;
         count: CARDINAL;
   BEGIN
      count := 1;
      index := 0;
      WHILE (count = 1) AND (index <= HIGH(text)) AND (text[index] <> 0C)
            AND Write(stderr, ADR(text[index]), count) DO
         INC(index);
      END;
      IF count = 1 THEN
         text[0] := 12C; (* nl *)
         IF Write(stderr, ADR(text), count) THEN END;
      END;
      Exit(255);
   END Panic;

END SysPanic.
