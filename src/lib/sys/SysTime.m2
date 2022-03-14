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
   $Id: SysTime.m2,v 0.2 1997/02/28 15:48:04 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SysTime.m2,v $
   Revision 0.2  1997/02/28  15:48:04  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:44  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysTime;

   FROM SYSTEM IMPORT UNIXCALL, ADR;
   FROM Sys IMPORT gettimeofday;	(*Nixdorf: time *)
   FROM SystemTypes IMPORT TIME;
   FROM Errno IMPORT errno;

TYPE
   TIMEVAL = RECORD
		tvsec : CARDINAL;
		tvusec : CARDINAL;
	     END;
   TIMEZONE = RECORD
		tzminwes : INTEGER;
		tzdsttim : INTEGER;
	      END;

   PROCEDURE Time(VAR t: TIME) : BOOLEAN;
      VAR d0, d1: CARDINAL;
	   tp : TIMEVAL;
	   tzp : TIMEZONE;
   BEGIN
      IF UNIXCALL(gettimeofday, d0, d1, ADR(tp), ADR(tzp)) THEN
	 t := tp.tvsec;
	 RETURN TRUE
      ELSE
	 errno := d0;
	 RETURN FALSE
      END;
   END Time;

END SysTime.
