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
   $Id: SystemTypes.d,v 0.2 1997/02/28 15:48:10 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SystemTypes.d,v $
   Revision 0.2  1997/02/28  15:48:10  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:25  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

(* Modula-2 Library    -  UNIX System V  -     AFB 9/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE SystemTypes; (* and constants *)

   (* see...
	/usr/include/fcntl.h
	/usr/include/signal.h
	/usr/include/sys/dir.h
	/usr/include/sys/param.h
	/usr/include/sys/types.h
   *)

   CONST
      DirSize = 255;
      MaxOpenFiles = 128;
      (* file control options; arguments of fcntl(2) and open(2) *)
      rdonly = {};
      wronly = { 31 };
      rdwr = { 30 };
      ndelay = { 29 };
      append = { 28 };
      async = { 25 };
      creat = { 22 };
      trunc = { 21 };
      excl = { 20 };
      nbio = { 19 };
      sync = { 18 };

      atFDCWD = -3041965; (* see /usr/include/sys/fcntl.h *)
   TYPE
      Sig = (SIG0,                                                     (*  0 *)
	     SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGTRAP, SIGIOT, SIGEMT, (*  7 *)
	     SIGFPE, SIGKILL, SIGBUS, SIGSEGV, SIGSYS, SIGPIPE,        (* 13 *)
	     SIGALRM, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGPWR,      (* 19 *)
	     SIGWINCH, SIGURG, SIGPOLL, SIGSTOP, SIGTSTP, SIGCONT,     (* 25 *)
	     SIGTTIN, SIGTTOU, SIGVTALRM, SIGPROF, SIGXCPU, SIGXFSZ,   (* 31 *)
	     SIGWAITING, SIGLWP, SIGFREEZE, SIGTHAW,                   (* 35 *)
	     SIGRT36, SIGRT37, SIGRT38, SIGRT39, SIGRT40, SIGRT41,     (* 41 *)
	     SIGRT42, SIGRT43);                                        (* 43 *)

      (* see /usr/include/sys/procset.h, typedef of idtype_t *)
      IdType = (PID, PPID, PGID, SID, CID, UID, GID, ALL); (* incomplete *)

   CONST
      (* aliases *)
      SIGABRT = SIGIOT;
      SIGCLD = SIGCHLD;
      SIGIO = SIGPOLL;
      SIGRTMIN = SIGRT36;
      SIGRTMAX = SIGRT43;

   TYPE
      SigSet = SET OF Sig;

      ProcessId = INTEGER;	(* ProcessId may be -1 for kill *)
      TIME = LONGINT;
      OFF = LONGINT;		(* offset/size of files *)

END SystemTypes.
