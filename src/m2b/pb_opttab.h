/* Ulm's Modula-2 System: Modula-2 Beautifier
   Copyright (C) 1983-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Beautifier is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   Ulm's Modula-2 Beautifier is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: pb_opttab.h,v 0.1 1997/02/24 15:51:45 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: pb_opttab.h,v $
   Revision 0.1  1997/02/24  15:51:45  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Optionstabelle fuer "pb_options.c"
 */

OPTSTR	opttab[] = {
	{ ""   , 's' , 0 , FALSE } ,
	{ "-c" , 'i' , 0 , TRUE  } ,
	{ "-h" , 'f' , 0 , TRUE  } ,
	{ "-l" , 'i' , 0 , TRUE  } ,
	{ "-o" , 'f' , 0 , TRUE  } ,
	{ "-p" , 'f' , 0 , TRUE  } ,
	{ "-s" , 'i' , 0 , TRUE  } ,
	{ "-v" , 'f' , 0 , TRUE  } ,
	{ ""   , 's' , 0 , FALSE } ,
	{ ""   , 's' , 0 , FALSE } 
	} ;
