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
   $Id: m2b_debug.c,v 0.1 1997/02/24 15:51:32 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_debug.c,v $
   Revision 0.1  1997/02/24  15:51:32  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * fuer's entwanzen...
 */

#ifdef DEBUG

#include	<stdio.h>
#include	"pb_mac.h"
#define	STACKSIZE	50

static	char	*stack[STACKSIZE];
static	int	top = 0;

nm_push(string)
char *string;
{
	if ( top >= STACKSIZE ) {
		++top;
		return;
		}
	stack[top++] = string;
}

nm_pop()
{
	if ( top <= 0 )
		fatal("Illegal call of `nm_pop'\n");
	--top;
}

nm_dump()
{
	fprintf(stderr,"Stack :\n-------\n");
	if ( top >= STACKSIZE ) {
		fprintf(stderr,"Stackoverflow !\n");
		top = STACKSIZE-1;
		}
	for ( --top ; top >= 0 ; --top )
		fprintf(stderr,"\t`%s'\n",stack[top]);
}

dump_all()
{
	fprintf(stderr,"Dump of pb :\n------------\n");
	nm_dump();
	in_dump();
	format_dump();
	pass2_dump();
}

#endif DEBUG
