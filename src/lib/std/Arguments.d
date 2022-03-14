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
   $Id: Arguments.d,v 0.2 1997/02/28 15:49:48 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Arguments.d,v $
   Revision 0.2  1997/02/28  15:49:48  borchert
   header fixed

   Revision 0.1  1997/02/21  19:17:56  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Arguments;	(* mh 5/85 *)
				(* rev mh 6/88 *)
(*
 *	This module reads options and other arguments from the command
 *	line. An argument "-" or "--" stops option reading. "-", how-
 *	ever, will be delivered as argument then, whereas "--" will not.
 *)

(*
 *	Example:
 *
 *	xflag := FALSE;
 *	string := defaultstring;
 *	number := 1;
 *	InitArgs("[-x] [-s string] [-nnn] [file]...");
 *	WHILE GetFlag(flag) DO
 *	   CASE flag OF
 *	      "x":  xflag := TRUE;
 *	   |  "s":  FetchString(string);
 *	   |  "0".."9":
 *	            UngetOpt;
 *	            FetchCard(number);
 *	      ELSE  Usage
 *	   END;
 *	END; (*WHILE GetFlag*)
 *	WHILE GetArg(filename) DO
 *	   IF StrCmp(filename,"-") = 0 THEN
 *	      (* process stdin *)
 *	   ELSE
 *	      (* process filename *)
 *	   END;
 *	END; (*WHILE GetArg*)
 *)

   PROCEDURE InitArgs(infostring: ARRAY OF CHAR);
	(* specifies infostring and (re)starts the reading cyclus *)

   PROCEDURE Usage;
	(* prints 'Usage: command infostring' on stderr and aborts
	 * program execution. FetchString, FetchCard and FetchInt call
	 * this procedure automatically in case of errors.
	 *)

   PROCEDURE GetFlag(VAR flag: CHAR): BOOLEAN;
	(* tries to read one flag, i.e. a character within a string containing
	 * a leading '-',from the argument list and returns TRUE if successful.
	 *)

   PROCEDURE GetOpt( VAR flag: CHAR; VAR plus: BOOLEAN): BOOLEAN;
	(* reads one character within a string starting in '+' or '-'.
	 *)

   PROCEDURE FetchString(VAR string: ARRAY OF CHAR);
	(* The procedures FetchXXX try to read data of type XXX from
	 * the argument list.
	 *)

   PROCEDURE FetchCard(  VAR number: CARDINAL);
	(* syntax of cardinal arguments:  [+]{digit}  *)

   PROCEDURE FetchInt(   VAR number: INTEGER);
	(* syntax of integer arguments:  [+|-]{digit}  *)

   PROCEDURE FetchOct(   VAR number: CARDINAL);
	(* syntax of octal arguments:  [+]{octdigit}  *)

   PROCEDURE FetchHex(   VAR number: CARDINAL);
	(* syntax of hexadecimal arguments:  [+]{hexdigit}  *)

   PROCEDURE GetArg(VAR argument: ARRAY OF CHAR): BOOLEAN;
	(* reads one argument or returns FALSE if all are read. *)

   PROCEDURE UngetArg;
	(* pushes the argument that has been read just before
	 * back to the argument list.
	 *)

   PROCEDURE UngetOpt;
	(* pushes the flag or option that has been read just before
	 * back to the argument list.
	 *)

   PROCEDURE AllArgs;
	(* calls 'Usage' if any arguments are not yet read. *)

END Arguments. 
