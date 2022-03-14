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
   $Id: E.m2,v 0.2 1997/02/28 15:59:45 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: E.m2,v $
   Revision 0.2  1997/02/28  15:59:45  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:40  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE E; (* WS 6/88 *)

   FROM StdIO IMPORT stderr, CloseAll;
   FROM SYSTEM IMPORT BYTE, ADDRESS;
   FROM LongStrings IMPORT Long, Alloc, AddString, ClearLong, AddChar,
      StrAdr, StrSize, AddBytes, Lwrite;
   FROM Printf IMPORT FmtExitCode, Eabort, Default, ErrorReaction,
      PanicAbort, Printf;
   FROM Errno IMPORT errno;
   FROM Strings IMPORT StrLen, StrCpy;
   FROM Chars IMPORT lf;
   FROM SystemTypes IMPORT Sig;
   (*
   FROM SysSignal IMPORT Signal, default;
   FROM SysKill IMPORT Kill;
   *)
   FROM SysGetpid IMPORT Getpid;
   FROM SysExit IMPORT Exit;

   VAR 
      ErrorMode : BITSET;
      Done      : FmtExitCode;
      OutputText: Long;
      AppendText: Long;
      x         : CHAR;
      initp     : BOOLEAN;
      exitp     : BOOLEAN;
      InitP     : PROC;
      ExitP     : PROC;
      called    : CARDINAL;

   CONST 
      fatalmess = "[fatal] ";
      errormess = "[error] ";
      panicmess = "[panic] ";
      blanks    = "        ";

   PROCEDURE EnterInitProc(proc : PROC);

   BEGIN 
      initp := TRUE;
      InitP := proc;
   END EnterInitProc;

   PROCEDURE EnterExitProc(proc : PROC);

   BEGIN 
      exitp := TRUE;
      ExitP := proc;
   END EnterExitProc;

   PROCEDURE ClearInitProc();

   BEGIN 
      initp := FALSE;
   END ClearInitProc;

   PROCEDURE ClearExitProc();

   BEGIN 
      exitp := FALSE;
   END ClearExitProc;

   PROCEDURE Signals(set : SigSet; proc : PROC);

      VAR 
         sig : Sig;
   BEGIN 
      (*
      set := set - SigSet{SIGSTOP, SIGKILL};
      FOR sig := SIGHUP TO SIGUSR2 DO 
         IF (sig IN set) AND NOT Signal(sig,proc) THEN 
            rror2(Eabort,"Cannot set signal %u (errno = %u).\n", sig, errno);
         END;
      END;
      *)
   END Signals;

   PROCEDURE setmode (mode : BITSET);

   BEGIN 
      ErrorMode := mode;
   END setmode;

   PROCEDURE Panic();

   BEGIN 
      IF CloseAll() THEN
      END;
      Exit(PanicAbort);
      (*
      IF Signal(SIGIOT,default) THEN
      END;
      IF ~Kill(Getpid(),SIGIOT) THEN 
	 (* if reached *)
         Exit(PanicAbort);
      END 
      *)
   END Panic;

   PROCEDURE getmode (VAR mode : BITSET);

   BEGIN 
      mode := ErrorMode;
   END getmode;

   PROCEDURE success() : FmtExitCode;

   BEGIN 
      RETURN Done;
   END success;

   PROCEDURE done() : BOOLEAN;

   BEGIN 
      RETURN Done = Success;
   END done;

   PROCEDURE Start(exit : ExitCode);

   BEGIN 
      INC(called);
      IF initp AND (called = 1) THEN 
         InitP();
      END;
      ClearLong(OutputText);
      IF exit = 0 THEN 
         AddString(OutputText,errormess);
      ELSIF exit = -1 THEN 
         AddString(OutputText,panicmess);
      ELSE 
         AddString(OutputText,fatalmess);
      END;
   END Start;

   PROCEDURE Finish(no : CARDINAL; exit : ExitCode; VAR fmt : ARRAY OF CHAR);

      PROCEDURE GenOutput () : BOOLEAN;

         VAR 
            name : ARRAY[0..255] OF CHAR;
            fmt  : ARRAY[0..43] OF CHAR;
            asz  : CARDINAL;
            no   : CARDINAL;

      BEGIN 
         IF exit # 0 THEN 
            asz := StrSize(AppendText);
            IF asz > 0 THEN 
               AddBytes(OutputText,StrAdr(AppendText),asz);
            END;
            ARGV(name,0);
            IF exit = -1 THEN 
               fmt := "[%-5s] Panic exit, core dumped. Stop.\n\n\n";
               no := 1;
            ELSE 
               fmt := "[%-5s] Exit code %u. Stop.\n";
               no := 2;
            END;
            RETURN Printf(OutputText,no,fmt,name,exit,x,x,x,x,x, x) = Success;
         ELSE 
            RETURN TRUE;
         END;
      END GenOutput;

      VAR 
         fmt3   : ARRAY[0..255] OF CHAR;
         factor : INTEGER;

   BEGIN 
      IF StrLen(fmt) > 0 THEN 
         IF Done = Success THEN 
            IF NOT GenOutput() THEN 
               Done := FmtPanic;
            ELSIF NOT Lwrite(OutputText,stderr) THEN 
               Done := CannotWriteStderr;
            END;
         END;
         IF Done # Success THEN 
            IF called > 3 THEN
	       Panic();
	    ELSE
	       factor := Eabort;
               ErrorReaction(Done,ErrorMode,no,factor,fmt3);
               rror3(factor,fmt3,no,exit,fmt);
            END;
         END;
      END;
      IF exit = -1 THEN 
         Panic();
      ELSIF exit # 0 THEN 
         Exit(exit);
      END;
      IF exitp AND (called = 1) THEN 
         ExitP();
      END;
      DEC(called);
   END Finish;

   PROCEDURE AddFatalLine(mess : ARRAY OF CHAR);

   BEGIN 
      IF StrLen(mess) > 0 THEN 
         AddString(AppendText,blanks);
         AddString(AppendText,mess);
         AddChar(AppendText,lf);
      ELSE 
         ClearLong(AppendText);
      END;
   END AddFatalLine;

   PROCEDURE rror0(exit : ExitCode; fmt : ARRAY OF CHAR);

   BEGIN 
      Start(exit);
      Done := Printf(OutputText,0,fmt,x,x,x,x,x,x,x,x);
      Finish(0,exit,fmt);
   END rror0;

   PROCEDURE rror1(exit : ExitCode; fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   BEGIN 
      Start(exit);
      Done := Printf(OutputText,1,fmt,i1,x,x,x,x,x,x,x);
      Finish(1,exit,fmt);
   END rror1;

   PROCEDURE rror2(exit : ExitCode; fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF 
      BYTE );

   BEGIN 
      Start(exit);
      Done := Printf(OutputText,2,fmt,i1,i2,x,x,x,x,x,x);
      Finish(2,exit,fmt);
   END rror2;

   PROCEDURE rror3(exit : ExitCode; fmt : ARRAY OF CHAR; i1,i2,i3 : ARRAY OF 
      BYTE);

   BEGIN 
      Start(exit);
      Done := Printf(OutputText,3,fmt,i1,i2,i3,x,x,x,x,x);
      Finish(3,exit,fmt);
   END rror3;

   PROCEDURE rror4(exit : ExitCode; fmt : ARRAY OF CHAR; i1,i2,i3,i4 : 
      ARRAY OF BYTE );

   BEGIN 
      Start(exit);
      Done := Printf(OutputText,4,fmt,i1,i2,i3,i4,x,x,x,x);
      Finish(4,exit,fmt);
   END rror4;

   PROCEDURE rror5(exit : ExitCode; fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5 : 
      ARRAY OF BYTE);

   BEGIN 
      Start(exit);
      Done := Printf(OutputText,5,fmt,i1,i2,i3,i4,i5,x,x,x);
      Finish(5,exit,fmt);
   END rror5;

   PROCEDURE rror6(exit : ExitCode; fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6 : 
      ARRAY OF BYTE);

   BEGIN 
      Start(exit);
      Done := Printf(OutputText,6,fmt,i1,i2,i3,i4,i5,i6,x,x);
      Finish(6,exit,fmt);
   END rror6;

   PROCEDURE rror7(exit : ExitCode; fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6,i7 
      : ARRAY OF BYTE);

   BEGIN 
      Start(exit);
      Done := Printf(OutputText,7,fmt,i1,i2,i3,i4,i5,i6,i7,x);
      Finish(7,exit,fmt);
   END rror7;

   PROCEDURE rror8(exit : ExitCode; fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6,i7,
      i8 : ARRAY OF BYTE);

   BEGIN 
      Start(exit);
      Done := Printf(OutputText,8,fmt,i1,i2,i3,i4,i5,i6,i7,i8);
      Finish(8,exit,fmt);
   END rror8;

BEGIN 
   Done := Undefined;
   exitp := FALSE;
   initp := FALSE;
   called := 0;
   ErrorMode := Default;
   Alloc(OutputText);
   Alloc(AppendText);
END E. 
