(* Ulm's Modula-2 Compiler    Solaris 2.x/SPARCv8
   Copyright (C) 1983-1996 Universitaet Ulm, SAI, 89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Modula-2 has been designed and developed by Niklaus Wirth
   at the Institut fuer Informatik, ETH Zuerich, Switzerland
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Compiler is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   Ulm's Modula-2 Compiler is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   $Id: MCArgs.m2,v 0.4 1999/02/10 16:57:52 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCArgs.m2,v $
   Revision 0.4  1999/02/10  16:57:52  borchert
   missing -K added

   Revision 0.3  1997/02/27  17:57:19  borchert
   -k (nokey) parameter added

   Revision 0.2  1997/02/26  17:48:30  borchert
   exit on invalid source file names

   Revision 0.1  1997/02/21  18:40:20  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCArgs; (* AFB 3/96 *)

   (* workup command line and set global variables in
      MCBase and MCP[1-4]Public accordingly
   *)

   FROM Arguments IMPORT InitArgs, Usage, GetFlag, GetArg,
      FetchString, FetchCard;
   FROM FtdIO IMPORT FwriteString, FwriteLn;
   FROM Memory IMPORT ALLOCATE;
   FROM StdIO IMPORT stderr;
   FROM Strings IMPORT StrCpy;
   FROM SysGetpid IMPORT Getpid;
   FROM SysExit IMPORT EnterCleanup;
   FROM SystemTypes IMPORT ProcessId;
   FROM SysUnlink IMPORT Unlink;
   FROM MCBase IMPORT Spellix;
   FROM MCFatal IMPORT Fatal;
   FROM MCSuffixes IMPORT TestSuffix, MakeName, moduleSX, reffileSX, asfileSX,
      symfileSX, definitionSX;
   IMPORT MCBase, MCSymFile, MCP1Public, MCP2Public, MCP3Public, MCP4Public, S;

   TYPE
      TmpFile = POINTER TO TmpFileRec;
      TmpFileRec =
	 RECORD
	    file: ARRAY [0..511] OF CHAR;
	    next: TmpFile;
	 END;
   VAR
      pid: ProcessId;
      tmpfiles: TmpFile;

   PROCEDURE RemoveTempFiles;
      VAR
	 tmpfile: TmpFile;
   BEGIN
      tmpfile := tmpfiles;
      WHILE tmpfile # NIL DO
	 IF ~Unlink(tmpfile^.file) THEN END;
	 tmpfile := tmpfile^.next;
      END;
   END RemoveTempFiles;

   PROCEDURE CreateTmpName(tmpdir: ARRAY OF CHAR;
			   prefix: ARRAY OF CHAR;
                           VAR tmpfile: ARRAY OF CHAR);
      VAR
	 entry: TmpFile;
   BEGIN
      S.printf3(tmpfile, "%s/%s.%d", tmpdir, prefix, pid);
      NEW(entry);
      WITH entry^ DO
	 StrCpy(file, tmpfile);
	 next := tmpfiles;
      END;
      IF tmpfiles = NIL THEN
	 EnterCleanup(RemoveTempFiles);
      END;
      tmpfiles := entry;
   END CreateTmpName;

   PROCEDURE ScanArgs;
      VAR
	 flag: CHAR;
	 tmpdir, il1, il2, asc, err, symfile: ARRAY [0..511] OF CHAR;
	 revision: CARDINAL;
	 head, tail, newsym: MCP1Public.SymNamePtr;
   BEGIN
      (* default parameters for 1st pass *)
      MCP1Public.ascName := ""; (* identifier table file *)
      MCP1Public.srcName := ""; (* compiler input file *)
      MCP1Public.il1Name := ""; (* interpass file from pass 1 to pass 2 *)
      MCP1Public.symNames := NIL;
      MCP1Public.listing := FALSE;

      (* default parameters for 2nd pass *)
      MCP2Public.ascName := ""; (* identifier table file *)
      MCP2Public.refName := ""; (* reference file *)
      MCP2Public.il1Name := ""; (* interpass file from pass 1 to pass 2 *)
      MCP2Public.il2Name := ""; (* interpass file from pass 2 to pass 3 *)

      (* default parameters for 3rd pass *)
      MCP3Public.ascName := ""; (* identifier table file *)
      MCP3Public.il1Name := ""; (* interpass file from pass 3 to pass 4 *)
      MCP3Public.il2Name := ""; (* interpass file from pass 2 to pass 3 *)
      MCP3Public.MDDFlag := FALSE; (* postprocessing by mdd? *)

      (* default parameters for 4th pass *)
      MCP4Public.il1Name := ""; (* interpass file from pass 3 to pass 4 *)
      MCP4Public.assName := ""; (* assembler output file *)
      MCP4Public.errName := ""; (* file with error messages *)
      MCP4Public.sourcefilename := ""; (* compiler input file *)
      MCP4Public.profile := FALSE; (* if on: count proc calls *)
      MCP4Public.Sflag := FALSE; (* produce nice output? *)
      MCP4Public.lflag := TRUE; (* produce line number labels? *)
      MCP4Public.errFile := TRUE; (* if on: messages on errName *)
      MCP4Public.Rflag := FALSE; (* if on: no range checks *)
      MCP4Public.Kflag := FALSE; (* if on: support huge stack offsets *)

      (* default parameters for symbol file generation *)
      MCSymFile.ascName := ""; (* identifier table file *)
      MCSymFile.symName := ""; (* symbol file *)

      (* other parameters *)
      tmpdir := "/tmp";
      MCBase.modrev := TRUE;
      MCBase.modrev2 := TRUE;
      MCBase.ismain := FALSE;
      MCBase.isdef := FALSE;
      MCBase.verbose := FALSE;
      MCBase.nokey := FALSE;

      InitArgs("[-ekKlLpRSv] [-r(0|1|2)] [-f reffile] [-o outfile] [-t tmpdir|-T il1 il2 asc err] source [symfiles ...]");

      WHILE GetFlag(flag) DO
	 CASE flag OF
	 | 'e':   MCP4Public.errFile := FALSE;
	 | 'f':   FetchString(MCP2Public.refName);
	 | 'k':   MCBase.nokey := TRUE;
	 | 'K':   MCP4Public.Kflag := TRUE;
	 | 'l':   MCP4Public.lflag := FALSE;
	 | 'L':   MCP1Public.listing := TRUE;
	 | 'o':   FetchString(MCP4Public.assName);
		  StrCpy(MCSymFile.symName, MCP4Public.assName);
	 | 'p':   MCP4Public.profile := TRUE;
	 | 'r':   FetchCard(revision);
		  CASE revision OF
		  | 0: MCBase.modrev := FALSE; MCBase.modrev2 := FALSE;
		  | 1: MCBase.modrev := TRUE;  MCBase.modrev2 := FALSE;
		  | 2: MCBase.modrev := TRUE;  MCBase.modrev2 := TRUE;
		  ELSE
		     Usage;
		  END;
	 | 'R':   MCP4Public.Rflag := TRUE;
	 | 'S':   MCP4Public.Sflag := TRUE;
	 | 't':   FetchString(tmpdir);
	 | 'T':   FetchString(il1); FetchString(il2); FetchString(asc);
		  FetchString(err);
	 | 'v':   MCBase.verbose := TRUE;
	 ELSE
	    Usage;
	 END;
      END;

      FetchString(MCP1Public.srcName);
      MCBase.isdef := TestSuffix(MCP1Public.srcName, definitionSX);
      IF ~MCBase.isdef & ~TestSuffix(MCP1Public.srcName, moduleSX) THEN
	 FwriteString(stderr, 'fatal error: invalid source file suffix of "');
	 FwriteString(stderr, MCP1Public.srcName);
	 FwriteString(stderr, '" -- either "');
	 FwriteString(stderr, moduleSX);
	 FwriteString(stderr, '" or "');
	 FwriteString(stderr, definitionSX);
	 FwriteString(stderr, '" expected.');
	 FwriteLn(stderr);
	 Fatal("invalid source file suffix");
      END;
      StrCpy(MCP4Public.sourcefilename, MCP1Public.srcName);

      head := NIL; tail := NIL;
      WHILE GetArg(symfile) DO
	 NEW(newsym);
         WITH newsym^ DO
	    StrCpy(symName, symfile);
            moduleName := Spellix(0);
            link := NIL;
         END;
         IF head = NIL THEN
            head := newsym;
         ELSE
            tail^.link := newsym;
         END;
         tail := newsym;
      END;
      MCP1Public.symNames := head;

      IF (il1[0] = 0C) OR (il2[0] = 0C) OR (asc[0] = 0C) OR (err[0] = 0C) THEN
	 IF tmpdir[0] = 0C THEN
	    Usage;
	 END;
	 CreateTmpName(tmpdir, "modula-il1", il1);
	 CreateTmpName(tmpdir, "modula-il2", il2);
	 CreateTmpName(tmpdir, "modula-asc", asc);
	 CreateTmpName(tmpdir, "modula-err", err);
      END;
      StrCpy(MCP1Public.il1Name, il1);
      StrCpy(MCP2Public.il1Name, il1);
      StrCpy(MCP2Public.il2Name, il2);
      StrCpy(MCP3Public.il1Name, il1);
      StrCpy(MCP3Public.il2Name, il2);
      StrCpy(MCP4Public.il1Name, il1);
      StrCpy(MCP1Public.ascName, asc);
      StrCpy(MCP2Public.ascName, asc);
      StrCpy(MCP3Public.ascName, asc);
      StrCpy(MCP4Public.errName, err);
      StrCpy(MCSymFile.ascName, asc);

      IF MCBase.isdef THEN
	 IF MCSymFile.symName[0] = 0C THEN
	    MakeName(MCSymFile.symName, MCP1Public.srcName, 
		  definitionSX, symfileSX);
	 END;
      ELSE
	 IF MCP2Public.refName[0] = 0C THEN
	    MakeName(MCP2Public.refName, MCP1Public.srcName, 
		  moduleSX, reffileSX);
	 END;
	 IF MCP4Public.assName[0] = 0C THEN
	    MakeName(MCP4Public.assName, MCP1Public.srcName,
		  moduleSX, asfileSX);
	 END;
      END;
   END ScanArgs;

BEGIN
   pid := Getpid();
END MCArgs.
