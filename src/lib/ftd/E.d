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
   $Id: E.d,v 0.2 1997/02/28 15:59:44 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: E.d,v $
   Revision 0.2  1997/02/28  15:59:44  borchert
   header fixed

   Revision 0.1  1997/02/21  19:48:31  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

(* 
 *    E - formatted error messages on stdout  (ws 6/88)
 *      - common error handling for fomatting modules
 *    =================================================
 *)

DEFINITION MODULE E;

   FROM SYSTEM IMPORT BYTE;
   FROM Printf IMPORT FmtExitCode;
   FROM SystemTypes IMPORT Sig;

   (* ---- program exit codes ----  *)

   TYPE 
      ExitCode = [-1..255];

      (*      -1 : issue a message, abort with core dump *)
      (*       0 : issue a message, do   n o t   terminate program *)
      (*      >0 : issue a message, terminate program with exitcode *)
      (*    >200 : used by F,S, ... *)

   (* --- diagnostic --- *)

   PROCEDURE done () : BOOLEAN;

   PROCEDURE success() : FmtExitCode;

   (* --- set mode for errorhandling (default is "FmtError.Default") --- *)

   PROCEDURE setmode (mode : BITSET);

   PROCEDURE getmode (VAR mode : BITSET);

   (* --- output procedures --- *)
   (*     if fmt is empty, no message is issued *)

   PROCEDURE rror0 (exit : ExitCode; fmt : ARRAY OF CHAR);

   PROCEDURE rror1 (exit : ExitCode; fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   PROCEDURE rror2 (exit : ExitCode; fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF 
      BYTE);

   PROCEDURE rror3 (exit : ExitCode; fmt : ARRAY OF CHAR; i1,i2, i3 : 
      ARRAY OF BYTE );

   PROCEDURE rror4 (exit : ExitCode; fmt : ARRAY OF CHAR; i1, i2, i3, i4 : 
      ARRAY OF BYTE);

   PROCEDURE rror5 (exit : ExitCode; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5 
      : ARRAY OF BYTE);

   PROCEDURE rror6 (exit : ExitCode; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5,
      i6 : ARRAY OF BYTE);

   PROCEDURE rror7 (exit : ExitCode; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5,
      i6, i7 : ARRAY OF BYTE);

   PROCEDURE rror8 (exit : ExitCode; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5,
      i6, i7 , i8 : ARRAY OF BYTE);

   PROCEDURE EnterInitProc(proc : PROC);

   PROCEDURE EnterExitProc(proc : PROC);

   PROCEDURE ClearInitProc();

   PROCEDURE ClearExitProc();

   (* ---- define a text which is appended to fatal error messages ---- *)
   (*      empty strings clears, others are appended                    *)

   PROCEDURE AddFatalLine(text : ARRAY OF CHAR);

   (* ---- Abort program with core dump. If impossible exit with exicode 255. *)

   (* ---- Support standard signal handling *)

   TYPE
      SigSet = SET OF Sig;

   CONST
      UserSignal = SigSet {SIGHUP, SIGINT, SIGQUIT,SIGTERM};
      FatalSignal = SigSet {SIGILL..SIGUSR2} - SigSet{SIGSTOP, SIGKILL};
      
   PROCEDURE Signals(set : SigSet; proc : PROC);

END E. 
