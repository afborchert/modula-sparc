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
   $Id: TimeIO.d,v 0.3 1997/02/28 15:50:49 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: TimeIO.d,v $
   Revision 0.3  1997/02/28  15:50:49  borchert
   header fixed

   Revision 0.2  1997/02/21  19:30:01  borchert
   removal of old copyright

   Revision 0.1  1997/02/21  19:18:17  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE TimeIO; (* AFB 9/88 *)

   FROM Calendar IMPORT Time, Date;
   FROM StdIO IMPORT FILE;

   TYPE
      Style = (date,	(* date(1) and ctime(3) standard format *)
	       ls,	(* like the ls-command *)
	       env);	(* see for TIMEFMT in environment *)
   VAR
      Done: BOOLEAN;
      termCH: CHAR;

   PROCEDURE WriteTime(format: ARRAY OF CHAR; time: Time);
      (* the output format is very close to date(1): *)
      (* each field descriptor is preceded by % and will be *)
      (* replaced in the output by its corresponding value. *)
      (* WriteTime does not append a newline automatically  *)
      (* like date(1).                                      *)
      (* output is directed to StdIO.stdout                 *)

   PROCEDURE FwriteTime(file: FILE; format: ARRAY OF CHAR; time: Time);
      (* like WriteTime but output is directed to file      *)

   PROCEDURE SwriteTime(VAR string: ARRAY OF CHAR;
			format: ARRAY OF CHAR;
			time: Time);
      (* like WriteTime but output is put into string       *)

   PROCEDURE WriteTimeLike(style: Style; time: Time);
      (* write time to StdIO.stdout according to the given  *)
      (* style.                                             *)

   PROCEDURE FwriteTimeLike(file: FILE; style: Style; time: Time);

   PROCEDURE SwriteTimeLike(VAR string: ARRAY OF CHAR;
			    style: Style; time: Time);

   PROCEDURE ReadTime(VAR time: Time);
      (* read time from StdIO.stdin *)

   PROCEDURE FreadTime(file: FILE; VAR time: Time);

   PROCEDURE SreadTime(string: ARRAY OF CHAR; VAR time: Time);

   PROCEDURE WriteDate(format: ARRAY OF CHAR; date: Date);

   PROCEDURE FwriteDate(file: FILE; format: ARRAY OF CHAR; date: Date);

   PROCEDURE SwriteDate(VAR string: ARRAY OF CHAR;
			format: ARRAY OF CHAR; date: Date);

   PROCEDURE ReadDate(VAR date: Date);

   PROCEDURE FreadDate(file: FILE; VAR date: Date);

   PROCEDURE SreadDate(string: ARRAY OF CHAR; VAR date: Date);

   (* Reading  depends on a set of pattern describing valid      *)
   (* input formats. This formats are stored in an ordered list. *)
   (* If more than one pattern matches the input the first will  *)
   (* be chosen.                                                 *)
   (* Pattern consists of a sequence of letters and some special *)
   (* chars which must match the input. Whitespace (except nl)   *)
   (* is skipped by ReadTime and must not be given inside a      *)
   (* pattern.                                                   *)
   (* Legal Letters:                                             *)
   (*   'y': year, 'm': month,  'd': day                         *)
   (*   'H': hour, 'M': minute, 'S': second                      *)
   (* Examples:                                                  *)
   (*   m/d/yH:M:S        us-date, matches 10/22/86 13:12:14     *)
   (*   d.m.yH:M:S        german date, matches 22.10.86 13:12:14 *)
   (*   md,y              matches Oct 22, 1986                   *)

   PROCEDURE Append(pattern: ARRAY OF CHAR);
      (* appends a new pattern to the end of the list *)

   PROCEDURE Insert(pattern: ARRAY OF CHAR);
      (* inserts a pattern before the beginning of the list *)

   PROCEDURE ReleaseList;
      (* causes the list to be emptied *)

   PROCEDURE DefaultList;
      (* appends a list of standard patterns to the list *)
      (* this procedure is called during initialization of TimeIO *)

END TimeIO.
