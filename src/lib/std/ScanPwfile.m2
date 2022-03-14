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
   $Id: ScanPwfile.m2,v 0.2 1997/02/28 15:50:31 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: ScanPwfile.m2,v $
   Revision 0.2  1997/02/28  15:50:31  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:34  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE ScanPwfile;

   FROM StdIO	IMPORT FILE, Fseek, Fgetc;

   PROCEDURE GetText(pwfile: FILE;
		     VAR text: ARRAY OF CHAR; sepchar: CHAR): BOOLEAN;
   (* EXPORTED *)
      VAR
	 ch: CHAR;
	 pos: CARDINAL;
   BEGIN
      pos := 0;
      LOOP
	 IF ~Fgetc(ch,pwfile) THEN
	    RETURN FALSE
	 END;
	 IF ch = sepchar THEN EXIT END;
	 IF pos <= HIGH(text) THEN
	    text[pos] := ch;
	    INC(pos);
	 END;
      END; (*LOOP*)
      IF pos <= HIGH(text) THEN
	 text[pos] := 0C;
      END;
      RETURN TRUE;
   END GetText;

   PROCEDURE GetNumber(pwfile: FILE;
			VAR number: CARDINAL; sepchar: CHAR): BOOLEAN;
   (* EXPORTED *)

      VAR
	 ch: CHAR;
	 numerical: BOOLEAN;
   BEGIN
      number := 0;
      numerical := TRUE;
      LOOP
	 IF ~Fgetc(ch,pwfile) THEN
	    RETURN FALSE
	 END;
	 IF ch = sepchar THEN
	    RETURN TRUE
	 END;
	 IF numerical THEN
	    IF (ch >= "0") & (ch <= "9") THEN
	       number := number * 10 + (ORD(ch) - ORD("0"));
	    ELSE
	       numerical := FALSE;
	    END;
	 END;
      END; (*LOOP*)
   END GetNumber;

   PROCEDURE ReRead(pwfile: FILE): BOOLEAN;		(* EXPORTED *)
   BEGIN
      RETURN Fseek(pwfile, (*offset*)0, (*whence=beginning*)0 )
   END ReRead;

END ScanPwfile.
