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
   $Id: MCP4Stack.d,v 0.1 1997/02/21 18:40:15 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Stack.d,v $
   Revision 0.1  1997/02/21  18:40:15  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP4Stack; (* AFB 4/89 *)

   (* allocation and deallocation of temporary stack space *)

   FROM MCBase IMPORT Size, Offset;

   PROCEDURE StackAlloc(VAR offset: Offset; size: Size);
      (* `offset' is aligned *)

   PROCEDURE StackFree(offset: Offset; size: Size);

   PROCEDURE StackOffset(offset: Offset);
      (* called at block entry; `offset' is the offset for temporaries *)

   PROCEDURE StackUse(VAR size: Size);
      (* called at block exit; *)
      (* returns the (aligned) number of bytes used for temporaries *)

END MCP4Stack.
