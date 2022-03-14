(* Ulm's Modula-2 System: Makefile Generator
   Copyright (C) 1987-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Makefile Generator for Modula-2 is free software; you can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   Ulm's Makefile Generator for Modula-2 is distributed in the hope
   that it will be useful, but WITHOUT ANY WARRANTY; without even the
   implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: SymTab.d,v 0.1 1997/02/24 08:36:10 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: SymTab.d,v $
   Revision 0.1  1997/02/24  08:36:10  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE SymTab; (* 3/87 *)

   FROM FileNames IMPORT FileName;

   CONST
      maxmodule = 512;		(* maximum number of modules *)
      modnmlen  = 512;		(* maximal module name length *)

   TYPE
      ModuleName = ARRAY [0..modnmlen-1] OF CHAR;
      ModuleRange = [0..maxmodule-1];
      ModuleSet = SET OF ModuleRange;
      SourceKind = (m2impl, m2def, m2main, mrimpl, mrdef, mrmain);
      SourceKindSet = SET OF SourceKind;
      SourceRef = POINTER TO Source;
      Source =
	 RECORD
	    kind: SourceKind;
	    filename: FileName;
	    modulename: ModuleName;
	    extern: BOOLEAN;                (* if on: no source given *)
	    depset: ModuleSet;
	    link: SourceRef;
	    CASE associated: BOOLEAN OF
	    | TRUE: otherpart: ModuleRange; (* impmod of defmod or vice versa *)
	    END;
	 END;

   CONST
      defs = SourceKindSet{m2def, mrdef};

   VAR
      mtab: ARRAY ModuleRange OF SourceRef;
      modules: CARDINAL; (* count of modules *)
      defset: ModuleSet; (* set of definition modules *)
      extset: ModuleSet; (* set of modules in MODPATH and lib *)
      (* sets of non-external Modula-2 and Modula/R modules *)
      m2set, mrset: ModuleSet;
      srclist: SourceRef;

   PROCEDURE InitSymTab;

   PROCEDURE InitModules;

   PROCEDURE LookForDef(modname: ModuleName; VAR module: ModuleRange) : BOOLEAN;
      (* only Modula-2 modules will be returned *)

   PROCEDURE LookForDefR(modname: ModuleName;
                         VAR module: ModuleRange) : BOOLEAN;
      (* Modula/R or Modula-2 module possible *)

   PROCEDURE LookForMod(modname: ModuleName; VAR module: ModuleRange) : BOOLEAN;

   PROCEDURE LookForModR(modname: ModuleName;
                         VAR module: ModuleRange) : BOOLEAN;

   PROCEDURE CheckFileNames;

   PROCEDURE Closure(deps: ModuleSet; VAR closure: ModuleSet);

END SymTab.
