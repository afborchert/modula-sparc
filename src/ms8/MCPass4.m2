(* Ulm's Modula-2 Compiler    Solaris 2.x/SPARCv8
   -- partially derived from ETH Zurichs Modula-2 Compiler for Lilith --
   Copyright (C) 1983-1996 Universitaet Ulm, SAI, 89069 Ulm, Germany
             (C) 1979-1981 Institut fuer Informatik, ETH Zuerich, Switzerland
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
   $Id: MCPass4.m2,v 0.1 1997/02/21 18:40:36 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCPass4.m2,v $
   Revision 0.1  1997/02/21  18:40:36  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCPass4;          (* AFB 8/83 *)

   FROM MCP4Blocks IMPORT GenBegin, GenEnd, CompilationUnit;
   FROM MCP4Global IMPORT InitErrorHandling;
   FROM MCP4In IMPORT OpenInput, CloseInput;

   PROCEDURE Pass4;
   BEGIN 
      InitErrorHandling;
      OpenInput;
      GenBegin; CompilationUnit; GenEnd;
      CloseInput;
   END Pass4;

END MCPass4. 
