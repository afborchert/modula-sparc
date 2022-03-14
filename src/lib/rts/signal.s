!------------------------------------------------------------------------------
! Ulm's Modula-2 System       Solaris 2.x / SPARCv8 platform
! (C) Copyright 1996, University of Ulm, Germany
! AFB 4/96
!------------------------------------------------------------------------------
! $Id: signal.s,v 0.1 1997/02/24 07:56:52 borchert Exp $
!------------------------------------------------------------------------------
! $Log: signal.s,v $
! Revision 0.1  1997/02/24  07:56:52  borchert
! Initial revision
!
!------------------------------------------------------------------------------
! signal handling stuff
!------------------------------------------------------------------------------
	.file	"signal.s"

	NSIG = 44		! valid signal numbers range from 1 to NSIG-1
	EINVAL = 22		! error code for invalid signal numbers
	sigaction = 98		! system call number of sigaction()
	context = 100		! system call number of set/getcontext
	savewin = 64		! number of bytes for saving our reg win

!------------------------------------------------------------------------------
! struct sigaction:
!    sa_flags      possible flags, always 0 here
!    sa_handler    signal handler
!    sa_mask	   which other signals are to be blocked, always 0 here
!    sa_resv       reserved area
!------------------------------------------------------------------------------
	size_sigaction = 32	!
	sa_flags = 0
	sa_handler = 4
	sa_mask = 8
	sa_resv = 24

!------------------------------------------------------------------------------
! array of Modula-2 signal handlers, index by signal number
!------------------------------------------------------------------------------
! regrettably, the assembler is unable to accept constant expressions here :-(
!	.reserve	.handlers,NSIG*4,".bss",4
	.reserve	.handlers,176,".bss",4

!------------------------------------------------------------------------------
! %i0:  signal number
! %i1:  new handler
! %i2:  where to store old handler
! %i3:  where to store errno
!------------------------------------------------------------------------------
! local variables on stack
	act = - size_sigaction		! new sigaction structure
	oact = act - size_sigaction	! old sigaction structure
	localvars = - oact		! size of local variables
!------------------------------------------------------------------------------
! local register variables
!       %l0:  -> .handlers[signo]
!	%l1:  -> act
!	%l2:  -> oact
!	%l3:  handler which is passed to system (0, 1, or .handler)
!	%l4:  returned handler (previous value)
!       %l5:  previous value of .handlers[signo]
!------------------------------------------------------------------------------
	.section ".text"
	.align	4
	.globl	M..signal
M..signal:
	save	%sp,-savewin-localvars,%sp
	subcc	%i0,%g0,%g0		! signo <= 0?
	bleu	.invalid_signo
	subcc	%i0,NSIG,%g0		! signo >= NSIG?
	bcc	.invalid_signo
! which handler is to be passed to the system?
	orcc	%g0,%i1,%g0		! handler = 0?  (0 = SIG_DFL)
	be	.fill_act
	or	%i1,%g0,%l3
	subcc	%i1,1,%g0		! handler = 1?  (1 = SIG_IGN)
	be	.fill_act
	nop
	sethi	%hi(.handler),%l3
	add	%l3,%lo(.handler),%l3	! take .handler as handler
! fill act structure
.fill_act:
	add	%fp,act,%l1
	st	%g0,[%l1+sa_flags]
	st	%l3,[%l1+sa_handler]
	st	%g0,[%l1+sa_mask]
	st	%g0,[%l1+sa_mask+4]
	st	%g0,[%l1+sa_mask+8]
	st	%g0,[%l1+sa_mask+12]
	st	%g0,[%l1+sa_resv]
	st	%g0,[%l1+sa_resv+4]
! set up system call
	or	%g0,sigaction,%g1	! number of system call
	or	%i0,%g0,%o0		! 1st param: signal number
	or	%l1,%g0,%o1		! 2nd param: ptr to new sigaction str
	add	%fp,oact,%o2		! 3rd param: ptr to old sigaction str
	ta	8
	bcs,a	.failed_call
	st	%o0,[%i3]		! pass errno on failure
! let %l0 point to .handlers[signo]
	sethi	%hi(.handlers),%l0
	add	%l0,%lo(.handlers),%l0
	sll	%i0,2,%g1
	add	%l0,%g1,%l0
! check returned handler value for 0 or 1
	add	%fp,oact,%l2
	ld	[%l2+sa_handler],%l2
	orcc	%l2,%g0,%g0		! old handler == 0?
	be,a	.returnOK
	st	%l2,[%i2]
	subcc	%l2,1,%g0		! old handler == 1?
	be,a	.returnOK
	st	%l2,[%i2]
	ld	[%l0],%l5
	st	%l5,[%i2]
.returnOK:
	st	%g0,[%i3]		! set errno to 0
	st	%i1,[%l0]
	orcc	%g0,1,%i0		! return TRUE
	ret
	restore

.invalid_signo:
	or	%g0,EINVAL,%l0
	st	%l0,[%i3]		! set errno to EINVAL
.failed_call:
	orcc	%g0,%g0,%i0		! RETURN FALSE
	ret
	restore

!------------------------------------------------------------------------------
! signal handler which has been passed to the system
! we have to call the Modula-2 handler and to return from interrupt
!------------------------------------------------------------------------------
! input parameters:
!    %i0:  signal number
!    %i1:  ptr to more informations about the signal  (ignored)
!    %i2:  ptr to old context  (used to return)
!------------------------------------------------------------------------------
! local registers:
!    %l0:  Modula-2 handler
!------------------------------------------------------------------------------
.handler:
	save	%sp,-savewin,%sp
	sll	%i0,2,%i0
	sethi	%hi(.handlers),%l0
	add	%l0,%lo(.handlers),%l0
	ld	[%l0+%i0],%l0
	jmpl	%l0,%o7
	nop
	or	%g0,100,%g1	! set/getcontext
	or	%g0,1,%o0	! 1st param: 1: setcontext
	or	%i2,%g0,%o1	! 2nd param: context
	ta	8
! well, we don't expect a return here
	ret
	restore
