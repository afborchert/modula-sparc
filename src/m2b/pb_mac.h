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
   $Id: pb_mac.h,v 0.1 1997/02/24 15:51:44 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: pb_mac.h,v $
   Revision 0.1  1997/02/24  15:51:44  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * diverse Macros fuer pb...
 */

#define	MAX(a,b)	((a)>(b) ? a : b)
#define	MIN(a,b)	((a)<(b) ? a : b)
#define isvisible(ch)	(' '<=(char)(ch) && (char)(ch)<='~')
#define	TRUE		(-1)
#define	FALSE		(0)
#define	EOF_CH		('\0')

#ifdef lint
#	define	ignore(a)	Ignore((char *) (a))
#	define	ignorf(a)	Ignorf((int (*)())(a))
#else
#	define	ignore(a)	a
#	define	ignorf(a)	a
#endif

#ifdef DEBUG
#	define	BEGIN(string)	nm_push(string);
#	define	END		{ nm_pop(); }
#	define	RETURN(expr)	{ nm_pop(); return(expr);}
#else
#	define	BEGIN(string)	/***/
#	define	END		/***/
#	define	RETURN(expr)	return(expr);
#endif
