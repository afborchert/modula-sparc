/*
 * This file has been taken from the GNU C Library, release 2.0.1,
 * subdirectory sysdeps/sparc without modifications
 * afb 2/97
 */

#define	FUNC(name)	\
	.global name;	\
	.align 4;	\
	name:
