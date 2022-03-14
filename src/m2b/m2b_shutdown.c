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
   $Id: m2b_shutdown.c,v 0.1 1997/02/24 15:51:41 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_shutdown.c,v $
   Revision 0.1  1997/02/24  15:51:41  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Geregelte Terminierung von pb im Fehlerfall
 */

#include <unistd.h>

extern	int	fatalerror;

extern void pass2_end();
extern void p2_fast_end();
extern void temp_close();
extern void in_end();
extern int temp_topen();

void shut_down ()
{
	if ( fatalerror )
		alarm ( (unsigned) 20 ); /* garantierter Abbruch */
	in_end();
	if ( !fatalerror )
		pass2_end();
	else
		p2_fast_end();
	if ( temp_topen() )
		temp_close();
}
