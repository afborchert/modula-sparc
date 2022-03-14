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
   $Id: Processes.d,v 0.2 1997/02/28 15:50:20 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Processes.d,v $
   Revision 0.2  1997/02/28  15:50:20  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:07  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Processes;

   TYPE SIGNAL; (* hidden *)

   PROCEDURE StartProcess(P: PROC; n: CARDINAL);
   (* start a concurrent process with program P and workspace of size n. *)
   (* PROC is a standard type defined as PROC = PROCEDURE                *)

   PROCEDURE SEND(VAR s: SIGNAL);
   (* one process waitung for s is resumed *)

   PROCEDURE WAIT(VAR s: SIGNAL);
   (* wait for some other process to send s *)

   PROCEDURE Awaited(s: SIGNAL) : BOOLEAN;
   (* Awaited(s) = "at least one process is waiting for s" *)

   PROCEDURE Init(VAR s: SIGNAL);
   (* compulsory initialization *)

END Processes.
