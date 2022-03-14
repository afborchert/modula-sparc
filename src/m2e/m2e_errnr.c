/* Ulm's Modula-2 Compiler: Print Error Messages of the Compiler
   Copyright (C) 1983-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
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
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: m2e_errnr.c,v 0.1 1997/02/24 16:33:28 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2e_errnr.c,v $
   Revision 0.1  1997/02/24  16:33:28  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/*
 *	m2e -- fetch error messages
 */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#ifdef SYSV
#define rindex(s,c)	strrchr(s,c)
extern char * strrchr();
#else
extern char * rindex();
#endif

extern void error();

extern char * errfile;	/* imported from m2e.c */

#define	MAXERRS	500

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
	char * modlib = NULL;
	char * error_file = NULL;

	if (errfile)
		error_file = errfile;
	else if ((modlib = getenv("MODLIB")) != NULL)
	{	error_file = malloc(strlen(modlib)+10);
		strcpy(error_file, modlib);
		strcat(error_file, "/m2_error");
	}
	if (!error_file || (fp = fopen(error_file, "r")) == NULL)
	{	only_numbers = TRUE;
		if (modlib && error_file)
			free(error_file);
		return;
	}
	if (modlib)
		free(error_file);
	only_numbers = FALSE;
	pos = 0;
	while ( fgets(buf,BUFSIZ,fp) )
	{	switch ( sscanf(buf,"%d",&index) ) {
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
		if (buf[0] == '\t')
			error("%s", buf+1);
		else
			error("%s", buf);
	}
	else
		error("error code : %d\n",nr);
}
