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
   $Id: CoExpressio.m2,v 0.2 1997/02/28 15:49:55 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: CoExpressio.m2,v $
   Revision 0.2  1997/02/28  15:49:55  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:21  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE CoExpressions;

   FROM SYSTEM IMPORT ADDRESS, NEWPROCESS, TRANSFER, WORD;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;

   (* (* from definition module *)
   TYPE
      CoRoutine;
   VAR
      main, source: CoRoutine;
   *)

   CONST
      StackSize = 8192;

   TYPE
      ResultType = WORD;
      CoRoutine = POINTER TO Process;

      Process =
         RECORD
            pc: ADDRESS; (* coroutine *)
            stack: ADDRESS; (* pointer to stack of pc *)
            caller: CoRoutine; (* return to caller on failure *)
         END;
   VAR
      cp: CoRoutine;
      failed: BOOLEAN;
      SuspendValue: ResultType;

   PROCEDURE Send(cr: CoRoutine; value: ResultType);
   BEGIN
      IF cr = NIL THEN RETURN END;
      source := cp;
      SuspendValue := value;
      failed := FALSE;
      cp := cr;
      TRANSFER(source^.pc, cp^.pc);
   END Send;

   PROCEDURE SendChar(cr: CoRoutine; ch: CHAR);
   BEGIN
      Send(cr, ORD(ch));
   END SendChar;

   PROCEDURE Fail;
      VAR src: CoRoutine;
   BEGIN
      source := cp;
      cp := cp^.caller;
      failed := TRUE;
      TRANSFER(source^.pc, cp^.pc);
   END Fail;

   PROCEDURE Create(VAR cr: CoRoutine; proc: PROC);
   BEGIN
      NEW(cr);
      WITH cr^ DO
         ALLOCATE(stack, StackSize);
         NEWPROCESS(proc, stack, StackSize, pc);
      END;
   END Create;

   PROCEDURE Receive(cr: CoRoutine; VAR value: ResultType) : BOOLEAN;
   BEGIN
      WITH cr^ DO
         cr^.caller := cp;
         source := cp;
         cp := cr;
         TRANSFER(source^.pc, cp^.pc);
         IF failed THEN
            DEALLOCATE(stack, StackSize);
            DISPOSE(cr);
            source := NIL;
            RETURN FALSE;
         ELSE
            value := SuspendValue;
            RETURN TRUE;
         END;
      END;
   END Receive;

   PROCEDURE ReceiveChar(cr: CoRoutine; VAR ch: CHAR) : BOOLEAN;
      VAR value: ResultType; returnValue: BOOLEAN;
   BEGIN
      returnValue := Receive(cr, value);
      IF returnValue THEN
         ch := CHR(CARDINAL(value));
      END;
      RETURN returnValue;
   END ReceiveChar;

   PROCEDURE Call(VAR cr: CoRoutine);
      VAR dummy: ResultType;
   BEGIN
      IF NOT Receive(cr, dummy) THEN
         cr := NIL;
      END;
   END Call;

BEGIN
   source := NIL;
   NEW(cp);
   main := cp;
END CoExpressions.
