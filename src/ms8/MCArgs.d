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
   $Id: MCArgs.d,v 0.1 1997/02/21 18:40:03 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCArgs.d,v $
   Revision 0.1  1997/02/21  18:40:03  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCArgs;

   (* workup command line and set global variables in
      MCBase and MCP[1-4]Public accordingly
   *)

   PROCEDURE ScanArgs;

END MCArgs.
