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
   $Id: Errors.m2,v 0.1 1997/02/24 08:36:13 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Errors.m2,v $
   Revision 0.1  1997/02/24  08:36:13  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Errors; (* 3/87 *)

   FROM FtdIO IMPORT FwriteString, FwriteLn, FwriteChar, FwriteCard;
   FROM StdIO IMPORT Fflush, stderr;
   FROM SysExit IMPORT Exit;

   VAR
      errors: CARDINAL;

   PROCEDURE Warning(text1, text2: ARRAY OF CHAR);
   BEGIN
      Out("warning", text1, text2);
   END Warning;

   PROCEDURE Error(text1, text2: ARRAY OF CHAR);
   BEGIN
      Out("error", text1, text2);
      INC(errors);
   END Error;

   PROCEDURE Fatal(text1, text2: ARRAY OF CHAR);
   BEGIN
      Out("fatal", text1, text2);
      Exit(errors+1);
   END Fatal;

   PROCEDURE Syntax(fn: ARRAY OF CHAR; line: CARDINAL; text: ARRAY OF CHAR);
   BEGIN
      FwriteString(stderr, 'syntax error in "');
      FwriteString(stderr, fn);
      FwriteString(stderr, '", line ');
      FwriteCard(stderr, line, 1);
      FwriteString(stderr, ' near "');
      FwriteString(stderr, text);
      FwriteChar(stderr, '"');
      FwriteLn(stderr);
      Exit(errors+1);
   END Syntax;

   PROCEDURE Errors() : CARDINAL;
   BEGIN
      RETURN errors;
   END Errors;

   PROCEDURE Out(text1, text2, text3: ARRAY OF CHAR);
   BEGIN
      FwriteString(stderr, text1); FwriteString(stderr, ": ");
      FwriteChar(stderr, '"'); FwriteString(stderr, text2);
      FwriteChar(stderr, '"'); FwriteString(stderr, ": ");
      FwriteString(stderr, text3); FwriteLn(stderr);
      IF NOT Fflush(stderr) THEN END;
   END Out;

BEGIN
   errors := 0;
END Errors.
