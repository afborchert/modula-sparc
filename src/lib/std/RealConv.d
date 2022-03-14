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
   $Id: RealConv.d,v 0.2 1997/02/28 15:50:27 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: RealConv.d,v $
   Revision 0.2  1997/02/28  15:50:27  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:10  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE RealConv; (* AFB 6/84 * rev. wsc 2/85 *)

   TYPE
      ReadProc = PROCEDURE(VAR CHAR);
   VAR
      Done: BOOLEAN;
      termCH: CHAR;

   PROCEDURE ReadReal(Read: ReadProc; VAR x: REAL);
   (* convention: Read returns 0C on eof or error *)

   PROCEDURE WriteFloat(VAR f: ARRAY OF CHAR; x: REAL; base: CARDINAL;
                        dp: CARDINAL);
   PROCEDURE WriteFix(VAR f: ARRAY OF CHAR; x: REAL; base: CARDINAL;
                      VAR dp: CARDINAL);

END RealConv.
