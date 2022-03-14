# {updated by mmm: Thu Feb 27 14:51:49 1997 GMT}
# {flags: -a+ -e- -l+ -L- -m- -1- -v-}
# {rules}
.SUFFIXES:
.SUFFIXES:	.o .sy .m2 .d
MFLAGS =	
M2C =		m2c
.m2.o:
		$(M2C) -c $(MFLAGS) $*.m2
.d.a:
		$(M2C) $(MFLAGS) $*.d
# {macros}
SRC =		Errno.d Errno.m2 Sys.d Sys.m2 SysAccess.d SysAccess.m2 \
		SysAlarm.d SysAlarm.m2 SysBreak.d SysBreak.m2 SysClose.d \
		SysClose.m2 SysCreat.d SysCreat.m2 SysDup.d SysDup.m2 \
		SysExec.d SysExec.m2 SysExit.d SysExit.m2 SysFcntl.d \
		SysFcntl.m2 SysFork.d SysFork.m2 SysGetpid.d SysGetpid.m2 \
		SysGetuid.d SysGetuid.m2 SysIoctl.d SysIoctl.m2 SysKill.d \
		SysKill.m2 SysLink.d SysLink.m2 SysLocation.d SysLocation.m2 \
		SysLseek.d SysLseek.m2 SysOpen.d SysOpen.m2 SysPanic.d \
		SysPanic.m2 SysPause.d SysPause.m2 SysPipe.d SysPipe.m2 \
		SysRead.d SysRead.m2 SysSetuid.d SysSetuid.m2 SysSignal.d \
		SysSignal.m2 SysStat.d SysStat.m2 SysTermIO.d SysTermIO.m2 \
		SysTime.d SysTime.m2 SysUnlink.d SysUnlink.m2 SysWait.d \
		SysWait.m2 SysWrite.d SysWrite.m2 SystemTypes.d \
		SystemTypes.m2 UnixString.d UnixString.m2
TAGSRC =	Errno.m2 Sys.m2 SysAccess.m2 SysAlarm.m2 SysBreak.m2 \
		SysClose.m2 SysCreat.m2 SysDup.m2 SysExec.m2 SysExit.m2 \
		SysFcntl.m2 SysFork.m2 SysGetpid.m2 SysGetuid.m2 SysIoctl.m2 \
		SysKill.m2 SysLink.m2 SysLocation.m2 SysLseek.m2 SysOpen.m2 \
		SysPanic.m2 SysPause.m2 SysPipe.m2 SysRead.m2 SysSetuid.m2 \
		SysSignal.m2 SysStat.m2 SysTermIO.m2 SysTime.m2 SysUnlink.m2 \
		SysWait.m2 SysWrite.m2 SystemTypes.m2 UnixString.m2
OBJS =		
SYMS =		
REFS =		Errno.r Sys.r SysAccess.r SysAlarm.r SysBreak.r SysClose.r \
		SysCreat.r SysDup.r SysExec.r SysExit.r SysFcntl.r SysFork.r \
		SysGetpid.r SysGetuid.r SysIoctl.r SysKill.r SysLink.r \
		SysLocation.r SysLseek.r SysOpen.r SysPanic.r SysPause.r \
		SysPipe.r SysRead.r SysSetuid.r SysSignal.r SysStat.r \
		SysTermIO.r SysTime.r SysUnlink.r SysWait.r SysWrite.r \
		SystemTypes.r UnixString.r
# {precious}
LIB =		libm2.a
.PRECIOUS:	$(LIB) SYM
# {all}
all:		$(LIB)
# {linkage}
LIBS =		
LDFLAGS =	
# {dependencies}
$(LIB):		$(LIB)(Errno.o) $(LIB)(Sys.o) $(LIB)(SysAccess.o) \
		$(LIB)(SysAlarm.o) $(LIB)(SysBreak.o) $(LIB)(SysClose.o) \
		$(LIB)(SysCreat.o) $(LIB)(SysDup.o) $(LIB)(SysExec.o) \
		$(LIB)(SysExit.o) $(LIB)(SysFcntl.o) $(LIB)(SysFork.o) \
		$(LIB)(SysGetpid.o) $(LIB)(SysGetuid.o) $(LIB)(SysIoctl.o) \
		$(LIB)(SysKill.o) $(LIB)(SysLink.o) $(LIB)(SysLocation.o) \
		$(LIB)(SysLseek.o) $(LIB)(SysOpen.o) $(LIB)(SysPanic.o) \
		$(LIB)(SysPause.o) $(LIB)(SysPipe.o) $(LIB)(SysRead.o) \
		$(LIB)(SysSetuid.o) $(LIB)(SysSignal.o) $(LIB)(SysStat.o) \
		$(LIB)(SysTermIO.o) $(LIB)(SysTime.o) $(LIB)(SysUnlink.o) \
		$(LIB)(SysWait.o) $(LIB)(SysWrite.o) $(LIB)(SystemTypes.o) \
		$(LIB)(UnixString.o)
SYM(Errno.sy):	Errno.d
		$(M2C) $(MFLAGS) Errno.d
$(LIB)(Errno.o): SYM(Errno.sy) Errno.m2
		$(M2C) $(MFLAGS) -c Errno.m2
		ar rv $(LIB) Errno.o && rm -f Errno.o
SYM(Sys.sy):	Sys.d
		$(M2C) $(MFLAGS) Sys.d
$(LIB)(Sys.o):	SYM(Sys.sy) Sys.m2
		$(M2C) $(MFLAGS) -c Sys.m2
		ar rv $(LIB) Sys.o && rm -f Sys.o
SYM(SysAccess.sy): SysAccess.d
		$(M2C) $(MFLAGS) SysAccess.d
$(LIB)(SysAccess.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysAccess.sy) \
		SYM(UnixString.sy) SysAccess.m2
		$(M2C) $(MFLAGS) -c SysAccess.m2
		ar rv $(LIB) SysAccess.o && rm -f SysAccess.o
SYM(SysAlarm.sy): SysAlarm.d
		$(M2C) $(MFLAGS) SysAlarm.d
$(LIB)(SysAlarm.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysAlarm.sy) SysAlarm.m2
		$(M2C) $(MFLAGS) -c SysAlarm.m2
		ar rv $(LIB) SysAlarm.o && rm -f SysAlarm.o
SYM(SysBreak.sy): SysBreak.d
		$(M2C) $(MFLAGS) SysBreak.d
$(LIB)(SysBreak.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysBreak.sy) \
		SYM(SysLocation.sy) SysBreak.m2
		$(M2C) $(MFLAGS) -c SysBreak.m2
		ar rv $(LIB) SysBreak.o && rm -f SysBreak.o
SYM(SysClose.sy): SysClose.d
		$(M2C) $(MFLAGS) SysClose.d
$(LIB)(SysClose.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysClose.sy) SysClose.m2
		$(M2C) $(MFLAGS) -c SysClose.m2
		ar rv $(LIB) SysClose.o && rm -f SysClose.o
SYM(SysCreat.sy): SysCreat.d
		$(M2C) $(MFLAGS) SysCreat.d
$(LIB)(SysCreat.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysCreat.sy) \
		SYM(UnixString.sy) SysCreat.m2
		$(M2C) $(MFLAGS) -c SysCreat.m2
		ar rv $(LIB) SysCreat.o && rm -f SysCreat.o
SYM(SysDup.sy):	SysDup.d
		$(M2C) $(MFLAGS) SysDup.d
$(LIB)(SysDup.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysClose.sy) SYM(SysDup.sy) \
		SYM(SysFcntl.sy) SysDup.m2
		$(M2C) $(MFLAGS) -c SysDup.m2
		ar rv $(LIB) SysDup.o && rm -f SysDup.o
SYM(SysExec.sy): SysExec.d
		$(M2C) $(MFLAGS) SysExec.d
$(LIB)(SysExec.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysExec.sy) \
		SYM(SysLocation.sy) SYM(UnixString.sy) SysExec.m2
		$(M2C) $(MFLAGS) -c SysExec.m2
		ar rv $(LIB) SysExec.o && rm -f SysExec.o
SYM(SysExit.sy): SysExit.d
		$(M2C) $(MFLAGS) SysExit.d
$(LIB)(SysExit.o): SYM(Sys.sy) SYM(SysExit.sy) SysExit.m2
		$(M2C) $(MFLAGS) -c SysExit.m2
		ar rv $(LIB) SysExit.o && rm -f SysExit.o
SYM(SysFcntl.sy): SysFcntl.d
		$(M2C) $(MFLAGS) SysFcntl.d
$(LIB)(SysFcntl.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysFcntl.sy) SysFcntl.m2
		$(M2C) $(MFLAGS) -c SysFcntl.m2
		ar rv $(LIB) SysFcntl.o && rm -f SysFcntl.o
SYM(SysFork.sy): SysFork.d
		$(M2C) $(MFLAGS) SysFork.d
$(LIB)(SysFork.o): SYM(Errno.sy) SYM(SysFork.sy) SysFork.m2
		$(M2C) $(MFLAGS) -c SysFork.m2
		ar rv $(LIB) SysFork.o && rm -f SysFork.o
SYM(SysGetpid.sy): SysGetpid.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysGetpid.d
$(LIB)(SysGetpid.o): SYM(Sys.sy) SYM(SystemTypes.sy) SYM(SysGetpid.sy) \
		SysGetpid.m2
		$(M2C) $(MFLAGS) -c SysGetpid.m2
		ar rv $(LIB) SysGetpid.o && rm -f SysGetpid.o
SYM(SysGetuid.sy): SysGetuid.d
		$(M2C) $(MFLAGS) SysGetuid.d
$(LIB)(SysGetuid.o): SYM(Sys.sy) SYM(SysGetuid.sy) SysGetuid.m2
		$(M2C) $(MFLAGS) -c SysGetuid.m2
		ar rv $(LIB) SysGetuid.o && rm -f SysGetuid.o
SYM(SysIoctl.sy): SysIoctl.d
		$(M2C) $(MFLAGS) SysIoctl.d
$(LIB)(SysIoctl.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysIoctl.sy) SysIoctl.m2
		$(M2C) $(MFLAGS) -c SysIoctl.m2
		ar rv $(LIB) SysIoctl.o && rm -f SysIoctl.o
SYM(SysKill.sy): SysKill.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysKill.d
$(LIB)(SysKill.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(SysKill.sy) SysKill.m2
		$(M2C) $(MFLAGS) -c SysKill.m2
		ar rv $(LIB) SysKill.o && rm -f SysKill.o
SYM(SysLink.sy): SysLink.d
		$(M2C) $(MFLAGS) SysLink.d
$(LIB)(SysLink.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysLink.sy) \
		SYM(UnixString.sy) SysLink.m2
		$(M2C) $(MFLAGS) -c SysLink.m2
		ar rv $(LIB) SysLink.o && rm -f SysLink.o
SYM(SysLocation.sy): SysLocation.d
		$(M2C) $(MFLAGS) SysLocation.d
$(LIB)(SysLocation.o): SYM(SysLocation.sy) SysLocation.m2
		$(M2C) $(MFLAGS) -c SysLocation.m2
		ar rv $(LIB) SysLocation.o && rm -f SysLocation.o
SYM(SysLseek.sy): SysLseek.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysLseek.d
$(LIB)(SysLseek.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(SysLseek.sy) SysLseek.m2
		$(M2C) $(MFLAGS) -c SysLseek.m2
		ar rv $(LIB) SysLseek.o && rm -f SysLseek.o
SYM(SysOpen.sy): SysOpen.d
		$(M2C) $(MFLAGS) SysOpen.d
$(LIB)(SysOpen.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysOpen.sy) \
		SYM(UnixString.sy) SysOpen.m2
		$(M2C) $(MFLAGS) -c SysOpen.m2
		ar rv $(LIB) SysOpen.o && rm -f SysOpen.o
SYM(SysPanic.sy): SysPanic.d
		$(M2C) $(MFLAGS) SysPanic.d
$(LIB)(SysPanic.o): SYM(SysExit.sy) SYM(SysPanic.sy) SYM(SysWrite.sy) \
		SysPanic.m2
		$(M2C) $(MFLAGS) -c SysPanic.m2
		ar rv $(LIB) SysPanic.o && rm -f SysPanic.o
SYM(SysPause.sy): SysPause.d
		$(M2C) $(MFLAGS) SysPause.d
$(LIB)(SysPause.o): SYM(Sys.sy) SYM(SysPause.sy) SysPause.m2
		$(M2C) $(MFLAGS) -c SysPause.m2
		ar rv $(LIB) SysPause.o && rm -f SysPause.o
SYM(SysPipe.sy): SysPipe.d
		$(M2C) $(MFLAGS) SysPipe.d
$(LIB)(SysPipe.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysPipe.sy) SysPipe.m2
		$(M2C) $(MFLAGS) -c SysPipe.m2
		ar rv $(LIB) SysPipe.o && rm -f SysPipe.o
SYM(SysRead.sy): SysRead.d
		$(M2C) $(MFLAGS) SysRead.d
$(LIB)(SysRead.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysRead.sy) SysRead.m2
		$(M2C) $(MFLAGS) -c SysRead.m2
		ar rv $(LIB) SysRead.o && rm -f SysRead.o
SYM(SysSetuid.sy): SysSetuid.d
		$(M2C) $(MFLAGS) SysSetuid.d
$(LIB)(SysSetuid.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysSetuid.sy) \
		SysSetuid.m2
		$(M2C) $(MFLAGS) -c SysSetuid.m2
		ar rv $(LIB) SysSetuid.o && rm -f SysSetuid.o
SYM(SysSignal.sy): SysSignal.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysSignal.d
$(LIB)(SysSignal.o): SYM(Errno.sy) SYM(SystemTypes.sy) SYM(SysSignal.sy) \
		SysSignal.m2
		$(M2C) $(MFLAGS) -c SysSignal.m2
		ar rv $(LIB) SysSignal.o && rm -f SysSignal.o
SYM(SysStat.sy): SysStat.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysStat.d
$(LIB)(SysStat.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(UnixString.sy) SYM(SysStat.sy) SysStat.m2
		$(M2C) $(MFLAGS) -c SysStat.m2
		ar rv $(LIB) SysStat.o && rm -f SysStat.o
SYM(SysTermIO.sy): SysTermIO.d
		$(M2C) $(MFLAGS) SysTermIO.d
$(LIB)(SysTermIO.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysTermIO.sy) \
		SysTermIO.m2
		$(M2C) $(MFLAGS) -c SysTermIO.m2
		ar rv $(LIB) SysTermIO.o && rm -f SysTermIO.o
SYM(SysTime.sy): SysTime.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysTime.d
$(LIB)(SysTime.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(SysTime.sy) SysTime.m2
		$(M2C) $(MFLAGS) -c SysTime.m2
		ar rv $(LIB) SysTime.o && rm -f SysTime.o
SYM(SysUnlink.sy): SysUnlink.d
		$(M2C) $(MFLAGS) SysUnlink.d
$(LIB)(SysUnlink.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysUnlink.sy) \
		SYM(UnixString.sy) SysUnlink.m2
		$(M2C) $(MFLAGS) -c SysUnlink.m2
		ar rv $(LIB) SysUnlink.o && rm -f SysUnlink.o
SYM(SysWait.sy): SysWait.d
		$(M2C) $(MFLAGS) SysWait.d
$(LIB)(SysWait.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysWait.sy) SysWait.m2
		$(M2C) $(MFLAGS) -c SysWait.m2
		ar rv $(LIB) SysWait.o && rm -f SysWait.o
SYM(SysWrite.sy): SysWrite.d
		$(M2C) $(MFLAGS) SysWrite.d
$(LIB)(SysWrite.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysWrite.sy) SysWrite.m2
		$(M2C) $(MFLAGS) -c SysWrite.m2
		ar rv $(LIB) SysWrite.o && rm -f SysWrite.o
SYM(SystemTypes.sy): SystemTypes.d
		$(M2C) $(MFLAGS) SystemTypes.d
$(LIB)(SystemTypes.o): SYM(SystemTypes.sy) SystemTypes.m2
		$(M2C) $(MFLAGS) -c SystemTypes.m2
		ar rv $(LIB) SystemTypes.o && rm -f SystemTypes.o
SYM(UnixString.sy): UnixString.d
		$(M2C) $(MFLAGS) UnixString.d
$(LIB)(UnixString.o): SYM(UnixString.sy) UnixString.m2
		$(M2C) $(MFLAGS) -c UnixString.m2
		ar rv $(LIB) UnixString.o && rm -f UnixString.o
#
LibDir :=	/usr/local/lib/modula
LibFiles :=	SYM REF $(LIB)
LibDirFiles :=	$(patsubst %,$(LibDir)/%,$(LibFiles))
Defs :=		$(filter, %.d,$(SRC))
Syms :=		$(patsubst %.d,SYM(%.sy),$(Defs))
.PHONY:		install
install:	$(LibDirFiles)
$(LibDirFiles):	$(LibDir)/%: %
		cp $< $@
SYM:		$(Syms)
REF:		$(LIB)
