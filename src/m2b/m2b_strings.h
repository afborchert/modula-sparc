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
   $Id: m2b_strings.h,v 0.1 1997/02/24 15:51:42 borchert Exp $
   ----------------------------------------------------------------------------
   $Log: m2b_strings.h,v $
   Revision 0.1  1997/02/24  15:51:42  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Strings fuer Fehlermeldungen
 */

char	syntax[] = { "Syntax Error." };
char	eoferr[] = { "Unexpected EOF in PASCAL-source." };
char	buferr[] = { "Buffer overflow." };
char	enderr[] = { "Structure Error : inappropriate end." };
char	elserr[] = { "inappropiate else." };
char	opterr[] = { "Illegal option specification." };
char	optign[] = { "Warning : Unknown option." };
char	comerr[] = { "Comment too long." };
char	endexp[] = { "'end' was expected." };
char	missrp[] = { "Missing keyword 'repeat'" };
char	misswh[] = { "Missing keyword 'while' or 'for...to'." };
char	missif[] = { "Missing keyword 'if'" };
char	numerr[] = { "Number too large in local option." };
char	illch [] = { "Illegal character." };
char	pusherr[]= { "Modularize your program !." };
char	misscm[] = { "Missing ','" };
char	missbg[] = { "Missing keyword 'begin'." };
char	blkerr[] = { "Malformed declaration part." };
char	misscs[] = { "Missing keyword 'case'." };
char	stmerr[] = { "Malformed statement." };
char	explp [] = { "'(' expected." };
char	idexp [] = { "Identifier expected." };
char	missfr[] = { "Missing keyword 'for'." };
char	experr[] = { "Malformed expression." };
char	exprp [] = { "')' expected." };
char	assexp[] = { "':=' expected." };
char	equexp[] = { "'=' expected." };
char	numexp[] = { "Number expected." };
char	semexp[] = { "';' expected." };
char	lspexp[] = { "'[' expected." };
char	csterr[] = { "Error in const-declaration part." };
char	typerr[] = { "Error in type-declaration part." };
char	varerr[] = { "Error in var-declaration part." };
char	strerr[] = { "Nonterminated string." };
char	lblerr[] = { "Error in label-declaration part." };
char	comexp[] = { "',' expected." };
char	rpexp [] = { "')' expected." };
char	parerr[] = { "Error in parameter list." };
