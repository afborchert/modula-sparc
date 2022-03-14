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
   $Id: MCP3Ident.d,v 0.1 1997/02/21 18:40:10 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP3Ident.d,v $
   Revision 0.1  1997/02/21  18:40:10  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP3Ident;            (* LG *)

   FROM MCBase IMPORT Idptr, Stptr, Stset, Listptr;
   FROM MCP3IO IMPORT spix;

   PROCEDURE Locate(list: Idptr; VAR ip: Idptr);
   (* Search an identifier in list.       *)
   (* The name spix is taken from MCP3IO. *)
   (* The result (possibly NIL) is        *)
   (* assigned to ip.                     *)

   PROCEDURE FAmong(sp: Stptr; forms: Stset): BOOLEAN;

   (* form of referenced structure is among forms *)

   PROCEDURE NewImpList(imp: Listptr);
   (* Initialisation of a new import list. *)
   (* An existing import list belonging to *)
   (* the same module is disposed.         *)

   PROCEDURE TermImpList(VAR imp: Listptr);
   (* Termination of the new import list. *)
   (* The new list is assigned to impp.   *)

   PROCEDURE EnterImpList(ip: Idptr);

   (* The identifier ip is entered into the import list. *)

   PROCEDURE DisposeImpList(VAR imp: Listptr);

   (* Dispose the whole import list of a module. *)

   PROCEDURE MarkModScope(ip: Idptr);

   (* Mark the scope of a module. *)

   PROCEDURE ReleaseModScope;

   (* Release the scope of a module. *)

   PROCEDURE MarkProcScope(ip: Idptr);

   (* Mark the scope of a procedure. *)

   PROCEDURE ReleaseProcScope;

   (* Release the scope of a procedure. *)

   PROCEDURE MarkWithScope(ip: Idptr);

   (* Mark the scope of a with statement *)

   PROCEDURE ReleaseWithScope;

   (* Release the scope of a with statement *)

   PROCEDURE FieldIndex(): CARDINAL;

   (* Return the nesting level of a in a with statement *)

   PROCEDURE BodyMark;

   (* mark scope level of compiled body *)

   PROCEDURE BodyScopes(): CARDINAL;

   (* return number of scopes nested in compiled body *)

   PROCEDURE SearchId(VAR ip: Idptr);
   (* Search an identifier in all scopes. *)
   (* The name spix is taken from MCP3IO. *)

   PROCEDURE ExportSearch(explist: Idptr; VAR ip: Idptr);
   (* Search an identifier in the         *)
   (* export list of a module.            *)
   (* The name spix is taken from MCP3IO. *)

END MCP3Ident. 
