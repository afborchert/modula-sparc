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
   $Id: Delay.m2,v 0.2 1997/02/28 15:59:15 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Delay.m2,v $
   Revision 0.2  1997/02/28  15:59:15  borchert
   header fixed

   Revision 0.1  1997/02/21  19:43:14  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Delay; (* AFB 6/88 *)

   FROM StdIO IMPORT FILE, stdout, Fputc;

   (* (* exported from definition module *)
   TYPE
      OutProc = PROCEDURE (CHAR);
   *)
   TYPE
      OutMode = (procedure, filep);
      DelayRec =
	 RECORD
	    baudrate: CARDINAL;
	    padch: CHAR;
	    CASE outmode: OutMode OF
	    | procedure: outc: OutProc;
	    | filep:     fp: FILE;
	    END;
	 END;
   VAR
      delay: DelayRec;

   PROCEDURE InitDelay(baudrate: CARDINAL; padch: CHAR; outc: OutProc);
   BEGIN
      delay.baudrate := baudrate;
      delay.padch := padch;
      delay.outmode := procedure;
      delay.outc := outc;
   END InitDelay;

   PROCEDURE InitDelayFile(baudrate: CARDINAL; padch: CHAR; fp: FILE);
   BEGIN
      delay.baudrate := baudrate;
      delay.padch := padch;
      delay.outmode := filep;
      delay.fp := fp;
   END InitDelayFile;

   PROCEDURE Delay(tenthofmillisecs: CARDINAL);
      CONST
	 second = 10000; (* in tenth of millisecs *)
      VAR
	 cnt: CARDINAL;  (* number of pad chars to be printed *)
	 i: CARDINAL;
   BEGIN
      WITH delay DO
	 IF (tenthofmillisecs > 0) AND (baudrate > 0) THEN
	    cnt := baudrate * tenthofmillisecs DIV second;
	    INC(cnt);
	    FOR i := 1 TO cnt DO
	       IF outmode = filep THEN
		  IF NOT Fputc(padch, fp) THEN END;
	       ELSE (* outmode = procedure *)
		  outc(padch);
	       END;
	    END;
	 END;
      END;
   END Delay;

BEGIN
   InitDelayFile(9600, 0C, stdout);
END Delay.
