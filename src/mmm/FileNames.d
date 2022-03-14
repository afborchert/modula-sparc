(* Ulm's Modula-2 System: Makefile Generator
   Copyright (C) 1987-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Makefile Generator for Modula-2 is free software; you can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   Ulm's Makefile Generator for Modula-2 is distributed in the hope
   that it will be useful, but WITHOUT ANY WARRANTY; without even the
   implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: FileNames.d,v 0.1 1997/02/24 08:36:04 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: FileNames.d,v $
   Revision 0.1  1997/02/24  08:36:04  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE FileNames; (* AFB 3/87 *)

   FROM StdIO IMPORT FILE;

   CONST
      filenmlen = 512;		(* maximal file name length *)
      dirsiz    = 14;		(* maximal file name length in directory *)
      maxsuffix = 3;		(* maximal suffix length *)

   TYPE
      FileName = 
	 RECORD
	    dirname: ARRAY [0..filenmlen-1] OF CHAR;
	    archive: ARRAY [0..dirsiz-1] OF CHAR;
	    basename: ARRAY [0..dirsiz-1] OF CHAR;
	    suffix: ARRAY [0..maxsuffix-1] OF CHAR;
	 END;

   PROCEDURE ConvertFileName(filename: ARRAY OF CHAR;
			     VAR newfn: FileName);

   PROCEDURE EnterFileName(filename: ARRAY OF CHAR);

   PROCEDURE EnterExtern(newfn: FileName; member: ARRAY OF CHAR;
			 modR: BOOLEAN;
			 modulename: ARRAY OF CHAR);

   PROCEDURE GetFileName(VAR filename: FileName; VAR extern: BOOLEAN;
			 VAR modR: BOOLEAN;
			 VAR modulename: ARRAY OF CHAR) : BOOLEAN;

   PROCEDURE WriteFileName(fp: FILE; filename: FileName);

   PROCEDURE ConstructFileName(VAR fn: ARRAY OF CHAR; filename: FileName);

   PROCEDURE ConstructArchiveName(VAR fn: ARRAY OF CHAR;
				  archive: ARRAY OF CHAR;
				  filename: FileName);

END FileNames.
