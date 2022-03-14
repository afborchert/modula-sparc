!------------------------------------------------------------------------------
! Ulm's Modula-2 System       Solaris 2.x / SPARCv8 platform
! (C) Copyright 1996, University of Ulm, Germany
! AFB 4/96
!------------------------------------------------------------------------------
! $Id: m2rt0.s,v 0.1 1997/02/24 07:55:42 borchert Exp $
!------------------------------------------------------------------------------
! $Log: m2rt0.s,v $
! Revision 0.1  1997/02/24  07:55:42  borchert
! Initial revision
!
!------------------------------------------------------------------------------
	.file	"m2rt0.s"
	.section ".text"
	.align	4
	.globl	_start
_start:
! get argc, argv & environ
	or	%g0,%g0,%fp
	ld	[%sp+64],%l0		! argc
	add	%sp,68,%l1		! argv
	sll	%l0,2,%l2
	add	%l2,4,%l2
	add	%l1,%l2,%l2		! environ
	sethi	%hi(M..argc),%i0
	st	%l0,[%i0+%lo(M..argc)]
	sethi	%hi(M..argv),%i0
	st	%l1,[%i0+%lo(M..argv)]

! initialize variables of SysLocations
	.globl	M.SysLocations.V
	.globl	_etext
	.globl	_edata
	.globl	_end

! SysLocations.ProgramEnd
	sethi	%hi(_end),%g1
	or	%g1,%lo(_end),%i1
	sethi	%hi(M.SysLocations.V+0),%i0
	st	%i1,[%i0+%lo(M.SysLocations.V+0)]

! SysLocations.Etext
	sethi	%hi(_etext),%g1
	or	%g1,%lo(_etext),%i1
	sethi	%hi(M.SysLocations.V+4),%i0
	st	%i1,[%i0+%lo(M.SysLocations.V+4)]

! SysLocations.Edata
	sethi	%hi(_edata),%g1
	or	%g1,%lo(_edata),%i1
	sethi	%hi(M.SysLocations.V+8),%i0
	st	%i1,[%i0+%lo(M.SysLocations.V+8)]

! SysLocations.Break
!    we take _end and align it to the next 4096-byte boundary
	sethi	%hi(_end),%g1
	or	%g1,%lo(_end),%i1
	add	%i1,4095,%i1
	andn	%i1,4095,%i1
	sethi	%hi(M.SysLocations.V+12),%i0
	st	%i1,[%i0+%lo(M.SysLocations.V+12)]

! SysLocations.Environment -- to be done
	sethi	%hi(M.SysLocations.V+16),%i0
	st	%l2,[%i0+%lo(M.SysLocations.V+16)]

! initialize RTErrors
	.globl	M.RTErrors
	call	M.RTErrors,0
	nop

! initialize main module
	.globl	M.I.main
	call	M.I.main,0
	nop
	.globl	M..exit
M..exit:
! calling SysExit.Exit(0)
	or	%g0,%g0,%o0
	call	M.SysExit_1,1
	nop
! calling exit(0)
	or	%g0,%g0,%o0
	or	%g0,1,%g1
	ta	8
! live forever if we are able to survive SysExit.Exit & exit(0)   :-)
.loop:	ba	.loop
	nop
	.globl	M.RTE.CaseErr
M.RTE.CaseErr:
! calling exit(255)
	or	%g0,255,%o0
	or	%g0,1,%g1
	ta	8
	ba	.loop
	nop
!
	.globl		M..argc
	.globl		M..argv
	.reserve	M..argc,4,".bss",4
	.reserve	M..argv,4,".bss",4
! following data section is necessary to avoid a kernel bug
! during loading
	.section ".data"
	.long	4711
