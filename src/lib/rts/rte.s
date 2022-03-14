!------------------------------------------------------------------------------
! Ulm's Modula-2 System       Solaris 2.x / SPARCv8 platform
! (C) Copyright 1996, University of Ulm, Germany
! AFB 4/96
!------------------------------------------------------------------------------
! $Id: rte.s,v 0.1 1997/02/24 07:57:01 borchert Exp $
!------------------------------------------------------------------------------
! $Log: rte.s,v $
! Revision 0.1  1997/02/24  07:57:01  borchert
! Initial revision
!
!------------------------------------------------------------------------------
! run time errors
!------------------------------------------------------------------------------
	.file	"rte.s"
	savewin = 64		! number of bytes for saving our reg win

!------------------------------------------------------------------------------
! variants of run time errors
!------------------------------------------------------------------------------
	halt = 1		! calling HALT is treated as run time error
	case = 2		! no case label (and no ELSE)
	stack = 3		! stack overflow (currently not checked)
	crend = 4		! return of a coroutine
	prio = 5		! priority error (currently not checked)
	fret = 6		! function does not return any value
	range = 7		! range errors

!------------------------------------------------------------------------------
! variants of range checks
!------------------------------------------------------------------------------
	unsigned = 1		! CARDINAL
	signed = 2		! INTEGER
	sign = 3		! INTEGER/CARDINAL conversion
	dyn = 4			! check against dyn array boundaries

!------------------------------------------------------------------------------
! global record which contains all informations about a run time failure
!------------------------------------------------------------------------------
	rte_type = 0		! halt..range, 0 means: no error yet
	rte_module = 4		! pointer to name of module
	rte_line = 8		! line number in source
	rte_pc = 12		! program counter
! following components are valid only if rte_type equals range
	rte_rtype = 16		! unsigned..dyn
	rte_value = 20		! the value which is out of range
	rte_min = 24		! lower bound
	rte_max = 28		! upper bound
	rte_size = 32

	.globl	rte
	.reserve	rte,32,".bss",4

	.reserve	.nested,4,".bss",4

!------------------------------------------------------------------------------
! entry points for the run time errors
!------------------------------------------------------------------------------
! input parameters:
!   %i0:  pointer to name of module
!   %i1:  line number in source
!   %i2:  lower bound
!   %i3:  upper bound
!   %i4:  value which is out of [lower bound .. upper bound]
!   %i7:  program counter
!------------------------------------------------------------------------------
! local variables:
!   %l0:  type of run time error (halt..range)
!   %l1:  type of range error (if any)
!   %l2:  pointer to rte record
!   %l3:  pointer to nested
!------------------------------------------------------------------------------
	.globl	M..halt
M..halt:
	save	%sp,-savewin,%sp
	ba	.L.notify
	or	%g0,halt,%l0

	.globl	M..case
M..case:
	save	%sp,-savewin,%sp
	ba	.L.notify
	or	%g0,case,%l0

	.globl	M..stack
M..stack:
	save	%sp,-savewin,%sp
	ba	.L.notify
	or	%g0,stack,%l0

	.globl	M..crend
M..crend:
	save	%sp,-savewin,%sp
	or	%g0,%g0,%i0
	or	%g0,%g0,%i1
	ba	.L.notify
	or	%g0,crend,%l0

	.globl	M..prio
M..prio:
	save	%sp,-savewin,%sp
	ba	.L.notify
	or	%g0,prio,%l0

	.globl	M..fret
M..fret:
	save	%sp,-savewin,%sp
	ba	.L.notify
	or	%g0,fret,%l0

	.globl	M..unsigned
M..unsigned:
	save	%sp,-savewin,%sp
	or	%g0,range,%l0
	ba	.L.notify
	or	%g0,unsigned,%l1

	.globl	M..unsigned0
M..unsigned0:
	save	%sp,-savewin,%sp
	or	%g0,range,%l0
	or	%g0,unsigned,%l1
	ba	.L.notify
	or	%g0,%g0,%i2

	.globl	M..signed
M..signed:
	save	%sp,-savewin,%sp
	or	%g0,range,%l0
	ba	.L.notify
	or	%g0,signed,%l1

	.globl	M..signed0
M..signed0:
	save	%sp,-savewin,%sp
	or	%g0,range,%l0
	or	%g0,signed,%l1
	ba	.L.notify
	or	%g0,%g0,%i2

	.globl	M..conv
M..conv:
	save	%sp,-savewin,%sp
	or	%g0,range,%l0
	ba	.L.notify
	or	%g0,sign,%l1

	.globl	M..dyn
M..dyn:
	save	%sp,-savewin,%sp
	or	%g0,range,%l0
	or	%g0,dyn,%l1
	ba	.L.notify
	or	%g0,%g0,%i2

.L.notify:
	sethi	%hi(rte),%l2
	or	%l2,%lo(rte),%l2
	st	%l0,[%l2+rte_type]
	st	%i0,[%l2+rte_module]
	st	%i1,[%l2+rte_line]
	subcc	%l0,range,%g0
	bne	.L.norange
	st	%i7,[%l2+rte_pc]
	st	%l1,[%l2+rte_rtype]
	st	%i4,[%l2+rte_value]
	st	%i2,[%l2+rte_min]
	st	%i3,[%l2+rte_max]
.L.norange:
	sethi	%hi(.nested),%g1
	ldstub	[%g1+%lo(.nested)],%g1
	subcc	%g1,%g0,%g0
	bne	.L.abort
	nop
	call	M.RTErrors_1
	or	%l2,%g0,%o0
! try to kill the own process:
.L.abort:
	or	%g0,20,%g1		! getpid
	ta	8			! our pid is now in %o0
	or	%g0,37,%g1		! kill
	or	%g0,6,%o1		! SIGABRT alias SIGIOT
	ta	8
! if we do survive this, try a plain exit(255):
	or	%g0,1,%g1		! exit
	or	%g0,255,%o0
	ta	8
! if we survive even this, go into loop
.L.loop:
	ba	.L.loop
	nop
