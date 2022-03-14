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
   $Id: Passwd.d,v 0.2 1997/02/28 15:50:15 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Passwd.d,v $
   Revision 0.2  1997/02/28  15:50:15  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:06  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Passwd;
(*
 *	scanning and searching the passord file
 *
 *	Martin Hasch, University of Ulm, Nov-29-1988
 *)

   TYPE
      Pwent =
	 RECORD
	    logname:  ARRAY [0..7] OF CHAR;
	    password: ARRAY [0..15] OF CHAR;
	    uid:      CARDINAL;
	    gid:      CARDINAL;
	    fullname: ARRAY [0..31] OF CHAR;
	    dir:      ARRAY [0..31] OF CHAR;
	    shell:    ARRAY [0..31] OF CHAR;
	 END;

   PROCEDURE OpenPw(filename: ARRAY OF CHAR): BOOLEAN;
   (* returns TRUE on success *)

   PROCEDURE GetPwent(VAR pwent: Pwent): BOOLEAN;

   PROCEDURE GetPwuid(uid: CARDINAL; VAR pwent: Pwent): BOOLEAN;

   PROCEDURE GetPwnam(logn: ARRAY OF CHAR; VAR pwent: Pwent): BOOLEAN;

   PROCEDURE ReopenPw(): BOOLEAN;
   (* returns TRUE if passwd file is open and seekable *)

   PROCEDURE ClosePw(): BOOLEAN;
   (* returns TRUE if passwd file was open *)


   PROCEDURE FetchPwuid(uid: CARDINAL; VAR pwent: Pwent): BOOLEAN;
   (* implies OpenPw("/etc/passwd"), and ClosePw() *)

   PROCEDURE FetchPwnam(logn: ARRAY OF CHAR; VAR pwent: Pwent): BOOLEAN;
   (* implies OpenPw("/etc/passwd"), and ClosePw() *)

END Passwd.
