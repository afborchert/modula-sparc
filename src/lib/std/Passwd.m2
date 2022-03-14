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
   $Id: Passwd.m2,v 0.2 1997/02/28 15:50:16 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Passwd.m2,v $
   Revision 0.2  1997/02/28  15:50:16  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:28  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Passwd;

   FROM StdIO	IMPORT FILE, MODE, Fopen, Fclose;
   FROM Strings	IMPORT StrCmp;
   FROM ScanPwfile IMPORT fieldsep, linesep, ReRead, GetText, GetNumber;

(* TYPE Pwent = RECORD ... END; *)

   CONST
      pwfilename  = "/etc/passwd";

   VAR
      pwfile: FILE;
      opened: BOOLEAN;

   PROCEDURE GetPwent(VAR pwent: Pwent): BOOLEAN;		(* EXPORTED *)
   BEGIN
      IF ~opened THEN RETURN FALSE END;
      WITH pwent DO
	 RETURN
	    GetText(pwfile, logname,fieldsep) &
	    GetText(pwfile, password,fieldsep) &
	    GetNumber(pwfile, uid,fieldsep) &
	    GetNumber(pwfile, gid,fieldsep) &
	    GetText(pwfile, fullname,fieldsep) &
	    GetText(pwfile, dir,fieldsep) &
	    GetText(pwfile, shell,linesep)
      END;
   END GetPwent;

   PROCEDURE OpenPw(filename: ARRAY OF CHAR): BOOLEAN;		(* EXPORTED *)
   BEGIN
      IF opened THEN
	 IF ~Fclose(pwfile) THEN END;
      END;
      opened := Fopen(pwfile, filename, read, (*buff'd*) TRUE);
      RETURN opened
   END OpenPw;

   PROCEDURE ClosePw(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      IF ~opened THEN
	 RETURN FALSE
      END;
      IF ~Fclose(pwfile) THEN END;
      opened := FALSE;
      RETURN TRUE
   END ClosePw;

   PROCEDURE ReopenPw(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      RETURN
	 opened & ReRead(pwfile)
   END ReopenPw;

   PROCEDURE GetPwuid(uid: CARDINAL; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetPwent(pwent) DO
	 IF pwent.uid = uid THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetPwuid;

   PROCEDURE GetPwnam(logn: ARRAY OF CHAR; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetPwent(pwent) DO
	 IF StrCmp(pwent.logname,logn) = 0 THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetPwnam;

   PROCEDURE FetchPwuid(uid: CARDINAL; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenPw(pwfilename) &
	 GetPwuid(uid, pwent) &
	 ClosePw()
   END FetchPwuid;

   PROCEDURE FetchPwnam(logn: ARRAY OF CHAR; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenPw(pwfilename) &
	 GetPwnam(logn, pwent) &
	 ClosePw()
   END FetchPwnam;

END Passwd.
