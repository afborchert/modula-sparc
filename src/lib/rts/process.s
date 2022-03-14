!------------------------------------------------------------------------------
! Ulm's Modula-2 System       Solaris 2.x / SPARCv8 platform
! (C) Copyright 1996, University of Ulm, Germany
! AFB 4/96
!------------------------------------------------------------------------------
! $Id: process.s,v 0.1 1997/02/24 07:56:47 borchert Exp $
!------------------------------------------------------------------------------
! $Log: process.s,v $
! Revision 0.1  1997/02/24  07:56:47  borchert
! Initial revision
!
!------------------------------------------------------------------------------
! NEWPROCESS & TRANSFER
!------------------------------------------------------------------------------
	.file	"process.s"

	context = 100		! system call number of set/getcontext
	savewin = 64		! number of bytes for saving our reg win
	ucontext_size = 448	! max known size of ucontext_t

!------------------------------------------------------------------------------
! internal process structure (which is pointed to by the ADDRESS params)
!------------------------------------------------------------------------------
	p_top = 0		! points to the current stack top
	p_started = 4		! flag, whether this cr has been started
	p_context = 8		! current context, valid if started
	p_proc = 12		! cr body
	p_size = 16		! size of this structure (a multiple of 8!)

	.section ".text"
	.align	4
!------------------------------------------------------------------------------
! parameters of NEWPROCESS:
!    %i0:  P: PROC;		! procedure parameter which becomes the
!				! body of the new coroutine
!    %i1:  A: ADDRESS;		! points to the begin of the new stack
!    %i2:  n: CARDINAL;		! size of the new stack
!------------------------------------------------------------------------------
! return value:
!    %i0:  VAR p1: ADDRESS;     ! pointer to process structure
!------------------------------------------------------------------------------
! local variables:
!    %l0:  new stack top
!------------------------------------------------------------------------------
	.globl	M..newprocess
M..newprocess:
	save	%sp,-savewin,%sp
	add	%i1,%i2,%i1		! let A point to the end of the stack
	andn	%i1,7,%i1		! and align it to a 8-byte-boundary
	sub	%i1,p_size,%i1		! allocate process structure on it
	sub	%i1,savewin,%l0
	st	%l0,[%i1+p_top]		! set current stack top
	st	%g0,[%i1+p_started]	! it has not started yet
	st	%g0,[%i1+p_context]	! no context yet
	st	%i0,[%i1+p_proc]	! set cr body
	or	%i1,%g0,%i0		! set return val
	ret
	restore

!------------------------------------------------------------------------------
! parameters of TRANSFER:
!    %i0:  VAR p1: ADDRESS	! where the old context is to be saved to
!    %i1:  VAR p2: ADDRESS	! where to take the new context from
!------------------------------------------------------------------------------
! local variables:
!    %l0:  p1: ADDRESS (dereferenced)
!    %l1:  p2: ADDRESS (dereferenced)
!    %l2:  points to area of local stack where the context may be stored to
!    %l3:  counter which gets incremented on each return from getcontext()
!------------------------------------------------------------------------------
	.globl	M..transfer
M..transfer:
	save	%sp,-savewin-ucontext_size-p_size-8,%sp
	add	%sp,savewin,%l2
! dereference VAR parameters
	add	%fp,-p_size-8,%l0
	ld	[%i1],%l1
	st	%l0,[%i0]
! setup counter which allows us to differentiate between different
! returns from getcontext()
	st	%g0,[%fp-8]	! zero counter
! save current context
	or	%g0,context,%g1	! set/getcontext
	or	%g0,%g0,%o0	! 1st param: 0: getcontext
	or	%l2,%g0,%o1	! 2nd param: where to store context to
	ta	8
! we may return to this point from two situations:
!   (1) the system call returns (will be the first return)
!   (2) the calling cr will be suspended again (by another TRANSFER)
	ld	[%fp-8],%l3
	add	%l3,1,%l3	! increment counter
	st	%l3,[%fp-8]	! store incremented counter back
	subcc	%l3,1,%g0	! just return if counter # 1
	bne	.L.return
	nop
! OK we are in case (1), lets save the old context now
	st	%sp,[%l0+p_top]
	or	%g0,1,%g1
	st	%g1,[%l0+p_started]
	st	%l2,[%l0+p_context]
! we have to do a context switch now:
!    (1) manually if it is the first time, i.e. started is FALSE
!    (2) by setcontext if the cr was already running
	ld	[%l1+p_started],%g1
	subcc	%g1,%g0,%g0
	be	.L.first
	nop
! restore the context by invoking setcontext() which will not return,
! hopefully :-)
	or	%g0,context,%g1		! set/getcontext
	or	%g0,1,%o0		! 1st param: 1: setcontext
	ld	[%l1+p_context],%o1	! 2nd param: from where to take context
	ta	8
! just return if setcontext does not work
	ba	.L.return
	nop
! OK, it is the first time when this coroutine gets started...
.L.first:
	ta	4			! clean register windows
	or	%g0,%g0,%fp		! zero frame pointer
	ld	[%l1+p_top],%sp		! set new stack pointer
	ld	[%l1+p_proc],%g1	! get address of cr body
	jmpl	%g1,%o7			! and jump to it
	nop
	call	M..crend		! abort if a cr returns
	nop
.L.return:
	ret
	restore
