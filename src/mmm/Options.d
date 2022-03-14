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
   $Id: Options.d,v 0.1 1997/02/24 08:36:07 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: Options.d,v $
   Revision 0.1  1997/02/24  08:36:07  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

DEFINITION MODULE Options; (* 3/87 *)

   TYPE
      VersionsManagement = (none, sccs, rcs);
   VAR
      SYMarchive: BOOLEAN; (* support SYM archive *)
      aronce: BOOLEAN;     (* update SYM archive with one ar-call only *)
      library: BOOLEAN;    (* hold objects in library *)
      profile: BOOLEAN;    (* support profiled library *)
      scanlibs: BOOLEAN;   (* scan libraries in MODPATH and MODRPATH *)
      lookforenv: BOOLEAN; (* look for macro in environment (on -c only) *)
      versmanag: VersionsManagement;

END Options.
