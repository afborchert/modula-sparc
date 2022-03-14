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
   $Id: MCP4Stateme.d,v 0.1 1997/02/21 18:40:16 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP4Stateme.d,v $
   Revision 0.1  1997/02/21  18:40:16  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP4Statements;

   (* code generation for statements *)

   FROM MCBase IMPORT Symbol, Type;
   FROM MCP4Attributes IMPORT Attribute;

   PROCEDURE StatSequ1(s: Symbol);
   PROCEDURE StatSequ3(s1, s2, s3: Symbol);

   PROCEDURE UseWith(i: INTEGER; VAR at: Attribute);
   PROCEDURE WithType(i: INTEGER; VAR type: Type);

END MCP4Statements.
