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
   $Id: Calendar.d,v 0.2 1997/02/28 15:49:49 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Calendar.d,v $
   Revision 0.2  1997/02/28  15:49:49  borchert
   header fixed

   Revision 0.1  1997/02/21  19:17:57  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Calendar;

   FROM SystemTypes IMPORT TIME;

(*
 *	Date calculations with
 *	(a)	Julius Caesar's calendar since Jan 01, 0001
 *	(b)	the Gregorian calendar	 since Oct 15, 1582
 *	(c)	Xelos system time.
 *
 *	Martin Hasch, University of Ulm, Jan 1988
 *)

   TYPE
      Time         = TIME;    			(* consecutive seconds *)
      Date         = LONGCARD;			(* consecutive days *)

      Year         = CARDINAL;
      Month        = [1..12];
      Day          = [1..31];
      Hour         = [0..23];
      Minute       = [0..59];
      Second       = [0..59];
      Weekday      = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      Week         = [1..53];
      Yearday      = [1..366];

      Daytime      = RECORD
			hour:    Hour;
			minute:  Minute;
			second:  Second;
		     END;
      Calendarday  = RECORD
			year:    Year;
			month:   Month;
			day:     Day;
		     END;
      CalendarInfo = RECORD
			weekday: Weekday;
			week:	 Week;
			yearday: Yearday;
		     END;

   PROCEDURE CurrentTime(): Time;
   (*
    *	returns actual system time = seconds since Jan 1, 1970, 00:00:00 GMT
    *)

   PROCEDURE ConvertTime(time: Time; VAR date: Date; VAR daytime: Daytime);
   PROCEDURE ConvertDate(date: Date; VAR calendarday: Calendarday);
   PROCEDURE ConvertCald(calendarday: Calendarday; VAR info: CalendarInfo);

   PROCEDURE CTime   (date: Date; daytime: Daytime):       Time;
   PROCEDURE CDate   (year: Year; month: Month; day: Day): Date;
   PROCEDURE CUltimo (year: Year; month: Month):	   Date;
   PROCEDURE CWeekday(date: Date):			   Weekday;

   PROCEDURE DateOK(year, month, day: CARDINAL): BOOLEAN;

   PROCEDURE TimeToString(time: Time; VAR string: ARRAY OF CHAR);
   (*
    *	converts time to a string, e.g. "Sun Sep 16 01:03:52 1973 GMT"
    *)

   PROCEDURE SetFirstOfWeek(weekday: Weekday);
   (*
    *	important for week of year calculation in ConvertCald; default is Mon.
    *)

   PROCEDURE GetTimezone (VAR tzname: ARRAY OF CHAR);
   PROCEDURE SetTimezone (    tzname: ARRAY OF CHAR);
   PROCEDURE GetLocaltime(VAR delay: Time);
   PROCEDURE SetLocaltime(    delay: Time);
   (*
    *	important for CTime, ConvertTime and TimeToString.
    *)

END Calendar.
