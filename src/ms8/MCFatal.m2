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
   $Id: MCFatal.m2,v 0.1 1997/02/21 18:40:22 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: MCFatal.m2,v $
   Revision 0.1  1997/02/21  18:40:22  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE MCFatal;

   FROM Errno IMPORT errno;
   FROM MCBase IMPORT passes;
   FROM FtdIO IMPORT FwriteString, FwriteLn;
   FROM StdIO IMPORT stderr;
   FROM SysExit IMPORT Exit;
   FROM SysPerror IMPORT Perror;
   IMPORT MCP1Public, MCP2Public, MCP3Public, MCP4Public;

   PROCEDURE Fatal(msg: ARRAY OF CHAR);
      (* print msg to stderr and exit *)
   BEGIN
      FwriteString(stderr, "Fatal error of Modula-2 Compiler: ");
      FwriteString(stderr, msg);
      FwriteLn(stderr);
      IF (passes > 0) & (MCP1Public.ErrorsFound OR MCP2Public.ErrorsFound OR
            MCP3Public.ErrorsFound OR MCP4Public.ErrorsFound) THEN
	 Exit(passes);
      ELSE
	 Exit(5);
      END;
   END Fatal;

   PROCEDURE IOFault(filename: ARRAY OF CHAR);
   BEGIN
      FwriteString(stderr, "Modula-2 Compiler: ");
      IF errno # 0 THEN
	 Perror(filename);
      ELSE
	 FwriteString(stderr, "unknown error: ");
	 FwriteString(stderr, filename);
	 FwriteLn(stderr);
      END;
      Fatal("I/O error (see above)");
   END IOFault;

END MCFatal.
