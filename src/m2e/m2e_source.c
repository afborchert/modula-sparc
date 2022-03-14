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
   $Id: m2e_source.c,v 0.1 1997/02/24 16:33:28 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2e_source.c,v $
   Revision 0.1  1997/02/24  16:33:28  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/*
 *	m2e -- fetch source lines
 */

#include	<stdarg.h>
#include	<stdio.h>

#define	FORMAT	"%4d    %s"
#define	WHITE	"****    "
#define	WHITER	"        "

extern void errnr();

static FILE *src;
static int eof;
static int last_line;
static char buf[BUFSIZ];
static int pos = 0;
extern int listing;


void source_init(fp)
	FILE	*fp;
{
	src = fp;
	last_line = 0;
	eof = 0;
}

char * source(line)
	int	line;
{	int	index;

	for ( index = 0 ; index < line-last_line ; ++index )
		if ( fgets(buf, BUFSIZ, src) == NULL )
		{	++eof;
			return "*** end of file ***\n" ;
		}
	last_line = line;
	return buf;
}

/*
 *	calculate exact position
 */

void setpos()
{	char * ptr;
	int epos;

	for (ptr = buf, epos = pos, pos = 0; ptr-buf < epos && *ptr; ++ptr)
		if (*ptr == '\t')
			pos += 8-(pos % 8);
		else
			++pos;
}

void print_error(line, epos, code)
	int	line, epos, code;
{	int lline;

	if (listing)
		while(!eof && last_line < line)
		{	lline = last_line+1;
			pos = 0;
			printf(FORMAT, lline, source(lline));
		}
	pos = epos;
	if (line != last_line)
	{	printf(FORMAT, line, source(line));
		last_line = line;
	}
	if (listing)
		printf(WHITE);
	else
		printf(WHITER);
	setpos();
	errnr(code);
}

void print_rest()
{	int line;
	char * ptr;

	line = last_line;
	while(ptr = source(++line), ! eof)
	{	pos = 0;
		printf(FORMAT, line, ptr);
	}
}

void error(char * fmt, ...)
{	
	va_list ap;
	int i;

	for ( i = 0 ; i < pos ; ++i )
		putchar(' ');
	printf("^ ");
	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
	putchar('\n');
}
