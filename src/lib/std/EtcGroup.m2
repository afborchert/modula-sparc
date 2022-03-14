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
   $Id: EtcGroup.m2,v 0.2 1997/02/28 15:50:02 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: EtcGroup.m2,v $
   Revision 0.2  1997/02/28  15:50:02  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:24  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE EtcGroup;

   FROM StdIO	IMPORT FILE, MODE, Fopen, Fclose;
   FROM Strings	IMPORT StrCmp;
   FROM ScanPwfile IMPORT fieldsep, linesep, GetText, GetNumber, ReRead;

   CONST
      grfilename  = "/etc/group";

   VAR
      grfile: FILE;
      opened: BOOLEAN;

   PROCEDURE GetGrent(VAR grent: Grent): BOOLEAN;		(* EXPORTED *)
      VAR
	 buf: ARRAY [0..1023] OF CHAR;
   BEGIN
      IF ~opened THEN RETURN FALSE END;
      WITH grent DO
	 members := NIL;		(* members not yet implemented *)
	 RETURN
	    GetText(grfile, grname,fieldsep) &
	    GetText(grfile, password,fieldsep) &
	    GetNumber(grfile, gid,fieldsep) &
	    GetText(grfile, buf,linesep)
      END;
   END GetGrent;

   PROCEDURE OpenGr(filename: ARRAY OF CHAR): BOOLEAN;		(* EXPORTED *)
   BEGIN
      IF opened THEN
	 IF ~Fclose(grfile) THEN END;
      END;
      opened := Fopen(grfile, filename, read, (*buff'd*) TRUE);
      RETURN opened
   END OpenGr;

   PROCEDURE CloseGr(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      IF ~opened THEN
	 RETURN FALSE
      END;
      IF ~Fclose(grfile) THEN END;
      opened := FALSE;
      RETURN TRUE
   END CloseGr;

   PROCEDURE ReopenGr(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      RETURN opened & ReRead(grfile)
   END ReopenGr;

   PROCEDURE GetGrgid(gid: CARDINAL; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetGrent(grent) DO
	 IF grent.gid = gid THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetGrgid;

   PROCEDURE GetGrnam(grn: ARRAY OF CHAR; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetGrent(grent) DO
	 IF StrCmp(grent.grname,grn) = 0 THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetGrnam;

   PROCEDURE FetchGrgid(gid: CARDINAL; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenGr(grfilename) &
	 GetGrgid(gid, grent) &
	 CloseGr()
   END FetchGrgid;

   PROCEDURE FetchGrnam(grn: ARRAY OF CHAR; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenGr(grfilename) &
	 GetGrnam(grn, grent) &
	 CloseGr()
   END FetchGrnam;

END EtcGroup.
