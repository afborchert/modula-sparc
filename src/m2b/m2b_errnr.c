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
   $Id: m2b_errnr.c,v 0.1 1997/02/24 15:51:33 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_errnr.c,v $
   Revision 0.1  1997/02/24  15:51:33  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/*
 *	m2e -- fetch error messages
 */

#include	<stdio.h>

#ifdef SYSV
#define	rindex(s,c)	strrchr(s,c)
extern char * strrchr();
#else
extern char * rindex();
#endif

extern void error();

#ifndef ERROR
#define ERROR "/usr/local/lib/modula/m2_error"
#endif

#ifndef MAXERRS
#define	MAXERRS	500
#endif

#define	TRUE	1
#define	FALSE	0

static	long	messages[MAXERRS];
static	FILE	*fp;
static	int	only_numbers;

void errnr_init ()
{
	char	buf[BUFSIZ];
	long	pos;
	int	index;


	if ( (fp = fopen(ERROR, "r")) == NULL )
	{	only_numbers = TRUE;
		return;
	}
	only_numbers = FALSE;
	pos = 0;
	while ( fgets(buf,BUFSIZ,fp) )
	{	
		switch ( sscanf(buf,"%d",&index) ) {
#ifdef DEBUG
		case 0 :
			fatal("in `error'-file : error number missing");

#endif DEBUG
		default :
#ifdef	DEBUG
			if ( messages[index] )
				fatal("in `error'-file : error number %d twice declared",index);
#endif	DEBUG
			if (index >= 0 && index < MAXERRS)
				messages[index] = pos;
#ifdef DEBUG
			else
				fatal("in `error'-file: error number %d exceeds maximum (%d)\n", index, MAXERRS-1);
#endif DEBUG

		}
		pos = ftell(fp);
	}
}

void errnr(nr)
	int	nr;
{	char	buf[BUFSIZ];
	char * ptr;

	if (nr == 0 || messages[nr])
	{	fseek ( fp , messages[nr] , 0 );
		fscanf( fp , "%d" , &nr );
		fgets(buf, BUFSIZ, fp);
		if ((ptr = rindex(buf, '\n')) != NULL)
			*ptr = '\0';
		if ( buf[0] == '\t' )
			error("%s", buf+1);
		else
			error("%s", buf);
	}
	else
		error("error code : %d\n",nr);
}
