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
   $Id: Clock.m2,v 0.2 1997/02/28 15:49:53 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Clock.m2,v $
   Revision 0.2  1997/02/28  15:49:53  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:20  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Clock;

   FROM SYSTEM IMPORT ADR, UNIXCALL;
   FROM Sys IMPORT times, gettimeofday;
   FROM SysTime IMPORT Time;
   FROM SystemTypes IMPORT TIME;

   TYPE
      TimesRec =
         RECORD
            utime: TIME;        (* CPU time while in user mode *)
            stime: TIME;        (* CPU time while in system mode *)
            cutime: TIME;       (* user time of all children *)
            cstime: TIME;       (* system time of all children *)
         END;
      TimeVal =
         RECORD
            tvsec: TIME;
            tvusec: TIME;
         END;

   VAR
      real: TimeVal;
      cpu: TIME;

   PROCEDURE GetProcessTimes(VAR timebuf: TimesRec);
      VAR
         d0, d1: INTEGER;

   BEGIN
      IF ~UNIXCALL(times, d0, d1, ADR(timebuf)) THEN
	 WITH timebuf DO
	    utime := 0; stime := 0; cutime := 0; cstime := 0;
	 END;
      END;
   END GetProcessTimes;

   PROCEDURE GetTimeVal(VAR timeval: TimeVal);
      VAR
         d0, d1: INTEGER;
   BEGIN
      IF ~UNIXCALL(gettimeofday, d0, d1, ADR(timeval), 0) THEN
         timeval.tvsec := 0;
         timeval.tvusec := 0;
      END;
   END GetTimeVal;

   PROCEDURE RealTime(reset: BOOLEAN) : TIME;
      (* return elapsed real time in units elapsed since the
         start of the process or since the last call with
         argument TRUE
      *)
      VAR
         result: TIME;
         buf   : TimeVal;

      PROCEDURE DiffInUnits(tval1, tval2: TimeVal) : TIME;
         VAR
            diff: TIME;
      BEGIN
         RETURN (tval2.tvsec - tval1.tvsec) * UnitsPerSecond + (tval2.tvusec
            - tval1.tvusec) DIV 100 * UnitsPerSecond DIV 10000;
      END DiffInUnits;

   BEGIN
      GetTimeVal(buf);
      result := DiffInUnits(real, buf);
      IF reset THEN
         real := buf;
      END;
      RETURN result
   END RealTime;

   PROCEDURE CPUTime (reset: BOOLEAN): TIME;
      VAR
         result: TIME;
         buf   : TimesRec;
   BEGIN
      GetProcessTimes(buf);
      result := buf.utime + buf.stime + buf.cutime + buf.cstime - cpu;
      IF reset THEN
         INC(cpu, result);
      END;
      RETURN result
   END CPUTime;

BEGIN
   GetTimeVal(real);
   cpu := 0;
END Clock.
