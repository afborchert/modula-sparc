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
   $Id: FtdIO.d,v 0.2 1997/02/28 15:50:04 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: FtdIO.d,v $
   Revision 0.2  1997/02/28  15:50:04  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:02  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE FtdIO;

   FROM SYSTEM IMPORT WORD;
   FROM StdIO IMPORT FILE;

   VAR Done: BOOLEAN;
       termCH: CHAR;

   PROCEDURE FreadInt(f: FILE; VAR int: INTEGER);

   PROCEDURE FwriteInt(f: FILE; int: INTEGER; w: CARDINAL);

   PROCEDURE FreadCard(f: FILE; VAR card: CARDINAL);

   PROCEDURE FwriteCard(f: FILE; card: CARDINAL; w: CARDINAL);

   PROCEDURE FreadString(f: FILE; VAR str: ARRAY OF CHAR);

   PROCEDURE FwriteString(f: FILE; str: ARRAY OF CHAR);

   PROCEDURE FwriteLn(f: FILE);

   PROCEDURE Fread(f: FILE; VAR arr: ARRAY OF WORD);

   PROCEDURE Fwrite(f: FILE; arr: ARRAY OF WORD);

   PROCEDURE FreadWord(f: FILE; VAR w: WORD);

   PROCEDURE FwriteWord(f: FILE; w: WORD);

   PROCEDURE FreadChar(f: FILE; VAR ch: CHAR);

   PROCEDURE FwriteChar(f: FILE; ch: CHAR);

END FtdIO.
