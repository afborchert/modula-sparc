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
   $Id: CallShell.m2,v 0.2 1997/02/28 15:49:52 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: CallShell.m2,v $
   Revision 0.2  1997/02/28  15:49:52  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:20  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE CallShell;

   FROM SysSignal IMPORT Signal, ignore, old;
   FROM SystemTypes IMPORT Sig;
   FROM SysExec IMPORT Exec;
   FROM SysFork IMPORT Fork;
   FROM SysWait IMPORT Wait;
   FROM SysExit IMPORT Exit;
   FROM UnixString IMPORT Buffer, Copy;
   FROM SYSTEM IMPORT ADR, ADDRESS;

   PROCEDURE Shell(cmd: ARRAY OF CHAR; VAR status: CARDINAL) : BOOLEAN;
      CONST shell = "/bin/sh";
         ExecFailed = 255;
      VAR child, pid: CARDINAL;
          args: ARRAY[0..3] OF ADDRESS;
          arg1, arg2: ARRAY[0..15] OF CHAR;
          intproc, quitproc: PROC;
          ign: BOOLEAN;
          CmdBuffer: Buffer; (* cmd with null byte *)
   BEGIN
      IF NOT Fork(child) THEN RETURN FALSE END;
      IF child = 0 THEN (* son *)
         arg1 := shell; arg2 := "-c";
         args[0] := ADR(arg1);
         args[1] := ADR(arg2);
         Copy(CmdBuffer, cmd);
         args[2] := ADR(CmdBuffer);
         args[3] := 0;
         Exec(shell, ADR(args));
         Exit(ExecFailed);
      ELSE (* father *)
	 ign := Signal(SIGINT, ignore); intproc := old;
	 ign := Signal(SIGQUIT, ignore); quitproc := old;
         WHILE Wait(pid, status) AND (pid <> child) DO END;
         ign := Signal(SIGINT, intproc);
         ign := Signal(SIGQUIT, quitproc);
         RETURN status DIV 100H <> ExecFailed;
      END;
   END Shell;

END CallShell.
