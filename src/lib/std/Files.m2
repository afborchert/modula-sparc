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
   $Id: Files.m2,v 0.2 1997/02/28 15:50:03 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Files.m2,v $
   Revision 0.2  1997/02/28  15:50:03  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:24  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Files;

   (* high level module for file handling *)

   FROM StdIO IMPORT read, write, Fopen, Fclose, Fputc, Fgetc,
      Ftell, Fseek;
   FROM SysLink IMPORT Link;
   FROM SysUnlink IMPORT Unlink;
   FROM SystemTypes IMPORT OFF;

   (* (* from definition module *)

   IMPORT StdIO;

   TYPE FILE = StdIO.FILE;

   VAR Done: BOOLEAN;

   *)

   PROCEDURE OpenRead(VAR f: FILE; filename: ARRAY OF CHAR);
   BEGIN
      Done := Fopen(f, filename, read, (* buffered = *) TRUE);
   END OpenRead;

   PROCEDURE OpenWrite(VAR f: FILE; filename: ARRAY OF CHAR);
   BEGIN
      Done := Fopen(f, filename, write, (* buffered = *) TRUE);
   END OpenWrite;

   PROCEDURE Close(f: FILE);
   BEGIN
      Done := Fclose(f);
   END Close;

   PROCEDURE SetPos(f: FILE; pos: OFF);
   BEGIN
      Done := Fseek(f, pos, 0);
   END SetPos;

   PROCEDURE GetPos(f: FILE; VAR pos: OFF);
   BEGIN
      Done := Ftell(f, pos);
   END GetPos;

   PROCEDURE Reset(f: FILE);
   BEGIN
      Done := Fseek(f, 0, 0);
   END Reset;

   PROCEDURE Delete(filename: ARRAY OF CHAR);
   BEGIN
      Done := Unlink(filename);
   END Delete;

   PROCEDURE Rename(oldname, newname: ARRAY OF CHAR);
   BEGIN
      IF Unlink(newname) THEN (* ignore result *) END;
      (* short circuit evaluation !!! *)
      Done := Link(oldname, newname) AND Unlink(oldname);
   END Rename;

END Files.
