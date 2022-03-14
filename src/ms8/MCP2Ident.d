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
   $Id: MCP2Ident.d,v 0.1 1997/02/21 18:40:08 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCP2Ident.d,v $
   Revision 0.1  1997/02/21  18:40:08  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE MCP2Ident;     (* LG *)

  FROM MCBase IMPORT Idptr, Stptr, Listptr, Spellix;
  FROM MCP2IO IMPORT spix;
   
  PROCEDURE Locate(list: Idptr; VAR ip: Idptr); 
    (* Search an identifier in list.       *)  
    (* The name spix is taken from MCP2IO. *)
    (* The result (possibly NIL) is        *)
    (* assigned to ip.                     *) 

  PROCEDURE NewImpList(imp: Listptr);
    (* Initialisation of a new import list. *) 
    (* An existing import list belonging to *)
    (* the same module is disposed.         *)

  PROCEDURE TermImpList(VAR imp: Listptr);
    (* Termination of the new import list. *)
    (* The new list is assigned to impp.   *)

  PROCEDURE EnterImpList(ip: Idptr);
    (* The identifier ip is entered into the import list. *)

  PROCEDURE MarkScope(ip: Idptr);  
    (* Mark the scope of a module or a procedure. *) 

  PROCEDURE ReleaseScope(ip: Idptr); 
    (* Release the scope of a module or a procedure. *)  

  PROCEDURE MsEntry(list: Idptr);
    (* Enter a list into the mslist of the current scope *)
 
  PROCEDURE EnterList(VAR list: Idptr; ip: Idptr); 
    (* Enter identifier ip into list *) 

  PROCEDURE EnterExportId(ip: Idptr);
     (* Enter identifier ip into list of current scope *)
     (* and into the export list of the global module  *)

  PROCEDURE EnterId(ip: Idptr); 
    (* Enter identifier ip into list of current scope. *) 

  PROCEDURE EnterForward(ptrtype: Stptr);
    (* Enter a forward reference of a pointer type *)
    (* into the forward list of the current scope. *)
    (* The name spix is taken from MCP2IO.         *) 

  PROCEDURE SearchInBlock(VAR ip: Idptr);  
    (* Search an identifier in the current scope. *)
    (* The name spix is taken from MCP2IO.        *) 
 
  PROCEDURE SearchId(VAR ip: Idptr);  
    (* Search an identifier in all scopes. *) 
    (* The name spix is taken from MCP2IO. *) 

  PROCEDURE ExportSearch(explist: Idptr; VAR ip: Idptr); 
    (* Search an identifier in the         *)
    (* export list of a module.            *)
    (* The name spix is taken from MCP2IO. *)  

  PROCEDURE SymModSearch(VAR ip: Idptr); 
    (* Search an identifier in the scope   *)
    (* of a symbolic module.               *) 
    (* The name spix is taken from MCP2IO. *) 

  PROCEDURE GlobalKnown(spix: Spellix): BOOLEAN; 
    (* Check whether the name spix is known on the *) 
    (* global level of an implementation module.   *)

END MCP2Ident.
