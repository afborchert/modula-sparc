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
   $Id: Terminal.m2,v 0.2 1997/02/28 15:50:47 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Terminal.m2,v $
   Revision 0.2  1997/02/28  15:50:47  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:39  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Terminal; (* AFB 8/84 *)

  (* version using StdIO *)

   FROM SYSTEM IMPORT ADR;
   FROM StdIO IMPORT stdin, stdout, Fputc, Fgetc, Fungetc, Fwrite;

   VAR
      (* Done: BOOLEAN; *)
      oldch: CHAR;
      readAgain: BOOLEAN;

   PROCEDURE Read(VAR ch: CHAR);
   BEGIN
      IF NOT Fgetc(ch, stdin) THEN
         Done := FALSE;
         ch := 0C;
      ELSE
         Done := TRUE;
      END;
      readAgain := FALSE;
      oldch := ch;
   END Read;

   PROCEDURE ReadAgain;
   BEGIN
      IF readAgain THEN
         Done := FALSE;
      ELSE
         Done := Fungetc(oldch, stdin);
         readAgain := TRUE;
      END;
   END ReadAgain;

   PROCEDURE Write(ch: CHAR);
   BEGIN
      Done := Fputc(ch, stdout);
   END Write;

   PROCEDURE WriteLn;
      CONST nl = 12C;
   BEGIN
      Write(nl);
   END WriteLn;

   PROCEDURE WriteString(s: ARRAY OF CHAR);
      VAR i: CARDINAL;
   BEGIN
      i := 0;
      WHILE (i <= HIGH(s)) AND (s[i] <> 0C) DO
         INC(i);
      END;
      (* use Fwrite for efficiency *)
      Done := (i = 0) OR Fwrite(ADR(s), SIZE(CHAR), i, stdout);
   END WriteString;

BEGIN
   readAgain := FALSE;
   Done := TRUE;
END Terminal.
