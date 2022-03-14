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
   $Id: TermInfo.t,v 0.2 1997/02/28 15:53:34 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: TermInfo.t,v $
   Revision 0.2  1997/02/28  15:53:34  borchert
   header fixed

   Revision 0.1  1997/02/21  19:43:19  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

(*%p+*)
DEFINITION MODULE TermInfo; (* AFB 5/88 *)

   FROM SYSTEM IMPORT BYTE;

   CONST
      stringlen = 128;
   TYPE
      String = ARRAY [0..stringlen-1] OF CHAR;
      OutProc = PROCEDURE (CHAR);
      DelayProc = PROCEDURE (CARDINAL);
      Term =
	 RECORD
	 END;

   PROCEDURE SetupTerm(tname: ARRAY OF CHAR; VAR tinfo: Term) : BOOLEAN;

   PROCEDURE Tparm(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR);

   PROCEDURE Tparm1(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		    arg1: ARRAY OF BYTE);

   PROCEDURE Tparm2(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		    arg1: ARRAY OF BYTE;
		    arg2: ARRAY OF BYTE);

   PROCEDURE Tparm3(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		    arg1: ARRAY OF BYTE;
		    arg2: ARRAY OF BYTE;
		    arg3: ARRAY OF BYTE);

   PROCEDURE Tparm4(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		    arg1: ARRAY OF BYTE;
		    arg2: ARRAY OF BYTE;
		    arg3: ARRAY OF BYTE;
		    arg4: ARRAY OF BYTE);

   PROCEDURE Tparm9(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		    arg1: ARRAY OF BYTE;
		    arg2: ARRAY OF BYTE;
		    arg3: ARRAY OF BYTE;
		    arg4: ARRAY OF BYTE;
		    arg5: ARRAY OF BYTE;
		    arg6: ARRAY OF BYTE;
		    arg7: ARRAY OF BYTE;
		    arg8: ARRAY OF BYTE;
		    arg9: ARRAY OF BYTE);

   PROCEDURE Tputs(str: ARRAY OF CHAR; affcnt: CARDINAL;
		   outc: OutProc);

   PROCEDURE TputsDelay(str: ARRAY OF CHAR; affcnt: CARDINAL;
			outc: OutProc; delay: DelayProc);

END TermInfo.
