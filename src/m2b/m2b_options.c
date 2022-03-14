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
   $Id: m2b_options.c,v 0.1 1997/02/24 15:51:39 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_options.c,v $
   Revision 0.1  1997/02/24  15:51:39  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Bearbeitung der Optionen
 */

#include	<stdio.h>
#include	<ctype.h>
#include	"pb_mac.h"
#include	"pb_optstr.h"

extern void usage();
extern void quit();
extern void fatal();

static int getint();

static	int	intarg;
static	char	*getname();
static	OPTSTR	*find();

void options ( argc , argv , tab , tablen )
int	argc;
char	**argv;
OPTSTR	*tab;
int	tablen;
{
	OPTSTR	*ptr;
	char	*opt_name;
	OPTSTR	*opt_ptr;
	
	for ( ptr = tab ; ptr-tab < tablen ; ++ptr )
		ptr->opt_used = 0;
	while ( argc > 0 && argv ) {
		opt_name = getname ( *argv );
		opt_ptr = find ( opt_name , tab , tablen );
		if ( opt_ptr == NULL )
			usage();
		switch ( opt_ptr->opt_type ) {
	
		case 'f' :			/* simple flag */
			*(opt_ptr->opt_spez.f.addr) =
				~(opt_ptr->opt_spez.f.dflt);
			break;
	
		case 'i' :			/* with integer argument */
			if ( getint ( *argv ) ) {
				if ( getint ( *++argv ) )
					quit("Argument of `%s' missing",
						opt_name);
				--argc;
				}
			if ( intarg < opt_ptr->opt_spez.i.arg_low )
				quit("Argument of `%s' must be greater than %d",
					opt_name,opt_ptr->opt_spez.i.arg_low);
			if ( intarg > opt_ptr->opt_spez.i.arg_high )
				quit("Argument of `%s' must be less than %d",
					opt_name,opt_ptr->opt_spez.i.arg_high);
			*(opt_ptr->opt_spez.i.arg_addr) = intarg;
			break;
	
		case 's':			/* with string argument */
			if ( strcmp ( opt_ptr->opt_name , "" ) ) {
				*(opt_ptr->opt_spez.s.str_addr) = *++argv;
					/* take next */
				--argc;
				}
			else if ( strcmp ( *argv , "-" ) == 0 )
				*(opt_ptr->opt_spez.s.str_addr) = "";
			else
				*(opt_ptr->opt_spez.s.str_addr) = *argv;
			break;
	
		default :
			if ( isvisible(opt_ptr->opt_type) )
				fatal(
				"Illegal optiontype found in optiontable : `%c'",
				opt_ptr->opt_type);
			else
				fatal("Illegal optiontype found in optiontable : %o ( octal )", (int) opt_ptr->opt_type);
		}
		++opt_ptr->opt_used;
		++argv;
		--argc;
		} /* of while */
	
	/*
	 * Defaults einsetzen fuer unbenutzte Optionen
	 */
	
	for ( ptr = tab ; ptr-tab < tablen ; ++ptr )
		if ( ptr->opt_used == 0 )
			switch ( ptr->opt_type ) {
	
			case 'f' :
				*(ptr->opt_spez.f.addr) = ptr->opt_spez.f.dflt;
				break;
	
			case 'i' :
				*(ptr->opt_spez.i.arg_addr) = ptr->opt_spez.i.arg_dflt;
				break;
	
			case 's' :
				*(ptr->opt_spez.s.str_addr) = ptr->opt_spez.s.str_dflt;
				break;
	
			default :
				if ( isvisible(ptr->opt_type) )
					fatal("Illegal optiontype found in optiontable : `%c'",ptr->opt_type);
				else
					fatal("Illegal optiontype found in optiontable : %o (octal)",(int) ptr->opt_type);
			}
}


static	char	*
getname ( string )
char	*string;
{
	static	char	result[16];
	char	*ptr;

	for ( ptr = result ; !isdigit(*string) && ptr-result < 16 ; ++ptr ) {
		*ptr = *string;
		++string;
		}
	*ptr = '\0';
	return ( result );
}

static	OPTSTR	*
find ( name , tab , tablen )
char	*name;
OPTSTR	*tab;
int	tablen;
{
	OPTSTR	*ptr;
	
	if ( strcmp(name,"-") == 0 )
		name = "";
	for ( ptr = tab ; ptr-tab < tablen ; ++ptr )
		if ( strlen(ptr->opt_name) == 0 && name[0] != '-' ||
		     strcmp(name,ptr->opt_name) == 0 )
			if ( ptr->opt_used == 0 || ptr->opt_repl )
				return ( ptr );
	return ( NULL );
}

static	int
getint	( string )
char	*string;
{
	char	*ptr;
	for ( ptr = string ; ; ++ptr )
		if ( *ptr == '\0' )
			return ( 1 );	/* nicht erfolgreich */
		else if ( isdigit ( *ptr ) )
			break;
	sscanf ( ptr , "%d" , &intarg );
	return ( 0 );			/* erfolgreich */
}
