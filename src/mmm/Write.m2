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
   $Id: Write.m2,v 0.1 1997/02/24 08:36:23 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Write.m2,v $
   Revision 0.1  1997/02/24  08:36:23  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Write; (* AFB 3/87 *)

   FROM SymTab IMPORT ModuleSet, mtab, ModuleRange, modules, srclist,
      SourceRef;
   FROM TopSort IMPORT defindex, defmodules;
   FROM FileNames IMPORT filenmlen, FileName, ConstructFileName,
      ConstructArchiveName;
   FROM Strings IMPORT StrCpy;
   FROM Out IMPORT Write, WriteLn, WriteString;

   PROCEDURE WriteNameAndSuffix(fn: FileName; suffix: ARRAY OF CHAR);
      VAR filename: ARRAY[0..filenmlen-1] OF CHAR;
   BEGIN
      StrCpy(fn.suffix, suffix);
      ConstructFileName(filename, fn);
      WriteString(filename);
   END WriteNameAndSuffix;

   PROCEDURE WriteArchiveAndSuffix(fn: FileName;
                                   archive, suffix: ARRAY OF CHAR);
      VAR filename: ARRAY[0..filenmlen-1] OF CHAR;
   BEGIN
      StrCpy(fn.suffix, suffix);
      ConstructArchiveName(filename, archive, fn);
      WriteString(filename);
   END WriteArchiveAndSuffix;

   PROCEDURE WriteModuleSet(mset: ModuleSet; suffix: ARRAY OF CHAR);
      VAR module: ModuleRange;
   BEGIN
      IF modules > 0 THEN
	 FOR module := MIN(ModuleRange) TO modules-1 DO
	    IF module IN mset THEN
	       Write(" ");
	       WriteNameAndSuffix(mtab[module]^.filename, suffix);
	    END;
	 END;
      END;
   END WriteModuleSet;

   PROCEDURE WriteModuleSetInArchive(mset: ModuleSet;
                                     archive, suffix: ARRAY OF CHAR);
      VAR module: ModuleRange;
   BEGIN
      IF modules > 0 THEN
	 FOR module := MIN(ModuleRange) TO modules-1 DO
	    IF module IN mset THEN
	       Write(" ");
	       IF mtab[module]^.extern THEN
		  WriteNameAndSuffix(mtab[module]^.filename, suffix);
	       ELSE
		  WriteArchiveAndSuffix(mtab[module]^.filename,
		                        archive, suffix);
	       END;
	    END;
	 END;
      END;
   END WriteModuleSetInArchive;

   PROCEDURE WriteSortedModuleSetInArchive(mset: ModuleSet;
                                           archive, sfx: ARRAY OF CHAR);
      VAR module: ModuleRange;
   BEGIN
      IF defmodules > 0 THEN
	 FOR module := MIN(ModuleRange) TO defmodules-1 DO
	    IF defindex[module] IN mset THEN
	       Write(" ");
	       WITH mtab[defindex[module]]^ DO
		  IF extern THEN
		     WriteNameAndSuffix(filename, sfx);
		  ELSE
		     WriteArchiveAndSuffix(filename, archive, sfx);
		  END;
	       END;
	    END;
	 END;
      END;
   END WriteSortedModuleSetInArchive;

END Write.
