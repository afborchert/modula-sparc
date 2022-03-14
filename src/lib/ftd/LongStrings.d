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
   $Id: LongStrings.d,v 0.2 1997/02/28 15:59:52 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: LongStrings.d,v $
   Revision 0.2  1997/02/28  15:59:52  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:34  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

(* 
 *    LongStrings- Allocate and handle strings of variable length (ws 6/88)
 *    ======================================================================
 *
 *
 *)

DEFINITION MODULE LongStrings;

FROM SYSTEM IMPORT ADDRESS;
FROM StdIO IMPORT FILE;

CONST
   LastChar = -1;
   NotFound = -2;

TYPE 
   Long;

   PROCEDURE Alloc (VAR long : Long);

   PROCEDURE Dispose (VAR long : Long);

   PROCEDURE Free (VAR long : Long);

   PROCEDURE CutLong(long : Long; newsize : CARDINAL);

   PROCEDURE ClearLong (long : Long);

   PROCEDURE ValidLong(long : Long) : BOOLEAN;

   PROCEDURE StrAdr(long : Long) : ADDRESS;

   PROCEDURE StrSize(long : Long) : CARDINAL;
   

   PROCEDURE AddString(long : Long; text : ARRAY OF CHAR);

   PROCEDURE AddChar(long : Long; char : CHAR);

   PROCEDURE AddBytes(long : Long; add : ADDRESS; n : CARDINAL);
   (* AddBytes must not be used for appending long to itself!!!*)

   PROCEDURE GetChar (long : Long; index : INTEGER) : CHAR;

   PROCEDURE CountChar (long : Long; char : CHAR) : CARDINAL;

   PROCEDURE FindChar(long : Long; char : CHAR; offset, count : INTEGER) 
      : INTEGER;
 
   PROCEDURE Lwrite(long : Long; file : FILE) : BOOLEAN;

   PROCEDURE LwritePart(long : Long; from, to : INTEGER; file : FILE) 
      : BOOLEAN;

   (* Echo all output via Lwrite or LwritePart to echofile. *)

   PROCEDURE Echo(origin,echo : FILE) : BOOLEAN;

   PROCEDURE NoEcho(origin,echo : FILE) : BOOLEAN;

END LongStrings.
