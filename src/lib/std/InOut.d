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
   $Id: InOut.d,v 0.2 1997/02/28 15:50:11 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: InOut.d,v $
   Revision 0.2  1997/02/28  15:50:11  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:04  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE InOut; (* stripped version: AFB 4/84 *)

   CONST
      EOL = 12C;
   VAR
      Done: BOOLEAN; (* on eof true *)
      termCH: CHAR; (* set in ReadString and numeric input procs *)

   PROCEDURE Read(VAR ch: CHAR);

   PROCEDURE ReadString(VAR s: ARRAY OF CHAR);

   PROCEDURE ReadCard(VAR c: CARDINAL);

   PROCEDURE ReadInt(VAR i: INTEGER);

   PROCEDURE Write(ch: CHAR);

   PROCEDURE WriteLn;

   PROCEDURE WriteString(s: ARRAY OF CHAR);

   (* n: minimum field width *)

   PROCEDURE WriteInt(x: INTEGER; n: CARDINAL);

   PROCEDURE WriteCard(x: CARDINAL; n: CARDINAL);

   PROCEDURE WriteOct(x: CARDINAL; n: CARDINAL);

   PROCEDURE WriteHex(x: CARDINAL; n: CARDINAL);

END InOut.
