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
   $Id: StdIO.d,v 0.3 1997/02/28 15:50:34 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: StdIO.d,v $
   Revision 0.3  1997/02/28  15:50:34  borchert
   header fixed

   Revision 0.2  1997/02/21  19:29:39  borchert
   removal of old copyright

   Revision 0.1  1997/02/21  19:18:13  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE StdIO; (* AFB 1/84 *)

   FROM SYSTEM IMPORT ADDRESS;
   FROM SystemTypes IMPORT OFF;

   TYPE
      FILE; (* hidden *)
      MODE = (read, write, append);

   VAR
      stdin, stdout, stderr: FILE;

   (* all functions return FALSE in error case *)

   PROCEDURE Fopen(VAR f: FILE; name: ARRAY OF CHAR; mode: MODE;
		   buffered: BOOLEAN) : BOOLEAN;

   PROCEDURE Fclose(f: FILE) : BOOLEAN;

   PROCEDURE Fread(ptr: ADDRESS; size: CARDINAL; VAR nitems: CARDINAL;
		   f: FILE) : BOOLEAN;

   PROCEDURE Fwrite(ptr: ADDRESS; size: CARDINAL; VAR nitems: CARDINAL;
		    f: FILE) : BOOLEAN;

   PROCEDURE Fseek(f: FILE; offset: OFF; whence: CARDINAL) : BOOLEAN;

   PROCEDURE Ftell(f: FILE; VAR pos: OFF) : BOOLEAN;

   PROCEDURE Feof(f: FILE) : BOOLEAN;

   PROCEDURE Ferror(f: FILE) : BOOLEAN;

   PROCEDURE Fgetc(VAR ch: CHAR; f: FILE) : BOOLEAN;

   PROCEDURE Fputc(ch: CHAR; f: FILE) : BOOLEAN;

   PROCEDURE Fungetc(ch: CHAR; f: FILE) : BOOLEAN;

   PROCEDURE CloseAll() : BOOLEAN;

   PROCEDURE Fflush(f: FILE) : BOOLEAN;

   PROCEDURE Fdopen(VAR f: FILE; fd: CARDINAL; mode: MODE;
                    buffered: BOOLEAN) : BOOLEAN;

   PROCEDURE FileNo(f: FILE) : CARDINAL;

END StdIO.
