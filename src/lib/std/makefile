# {updated by mmm: Mon Jan 11 10:05:06 1999 GMT}
# {flags: -a+ -e- -l+ -L+ -m- -1- -v-}
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
SRC =		ASCII.d ASCII.m2 Archive.d Archive.m2 Arguments.d \
		Arguments.m2 Calendar.d Calendar.m2 CallShell.d CallShell.m2 \
		Clock.d Clock.m2 CoExpressio.d CoExpressio.m2 Conversions.d \
		Conversions.m2 Directories.d Directories.m2 Environment.d \
		Environment.m2 EtcGroup.m2 EtcGroup.d Files.d Files.m2 \
		FtdIO.d FtdIO.m2 Functions.d Functions.m2 GetPass.d \
		GetPass.m2 InOut.d InOut.m2 MathLib.d MathLib.m2 Passwd.d \
		Passwd.m2 PipeIO.m2 PipeIO.d Plot.d Plot.m2 Processes.d \
		Processes.m2 RTErrors.d RTErrors.m2 RandomGener.d \
		RandomGener.m2 ReadIntCard.d ReadIntCard.m2 RealConv.d \
		RealConv.m2 RealInOut.d RealInOut.m2 ScanPwfile.d \
		ScanPwfile.m2 StdFuncs.d StdFuncs.m2 StdIO.d StdIO.m2 \
		Storage.d Storage.m2 StrSpec.d StrSpec.m2 StrToNum.d \
		StrToNum.m2 StrToReal.d StrToReal.m2 Strings.d Strings.m2 \
		SysConf.d SysConf.m2 SysPerror.d SysPerror.m2 Terminal.d \
		Terminal.m2 TimeIO.d TimeIO.m2
TAGSRC =	ASCII.m2 Archive.m2 Arguments.m2 Calendar.m2 CallShell.m2 \
		Clock.m2 CoExpressio.m2 Conversions.m2 Directories.m2 \
		Environment.m2 EtcGroup.m2 Files.m2 FtdIO.m2 Functions.m2 \
		GetPass.m2 InOut.m2 MathLib.m2 Passwd.m2 PipeIO.m2 Plot.m2 \
		Processes.m2 RTErrors.m2 RandomGener.m2 ReadIntCard.m2 \
		RealConv.m2 RealInOut.m2 ScanPwfile.m2 StdFuncs.m2 StdIO.m2 \
		Storage.m2 StrSpec.m2 StrToNum.m2 StrToReal.m2 Strings.m2 \
		SysConf.m2 SysPerror.m2 Terminal.m2 TimeIO.m2
OBJS =		
SYMS =		
REFS =		ASCII.r Archive.r Arguments.r Calendar.r CallShell.r Clock.r \
		CoExpressio.r Conversions.r Directories.r Environment.r \
		EtcGroup.r Files.r FtdIO.r Functions.r GetPass.r InOut.r \
		MathLib.r Passwd.r PipeIO.r Plot.r Processes.r RTErrors.r \
		RandomGener.r ReadIntCard.r RealConv.r RealInOut.r \
		ScanPwfile.r StdFuncs.r StdIO.r Storage.r StrSpec.r \
		StrToNum.r StrToReal.r Strings.r SysConf.r SysPerror.r \
		Terminal.r TimeIO.r
# {precious}
LIB =		libm2.a
.PRECIOUS:	$(LIB) SYM
# {all}
all:		$(LIB)
# {linkage}
LIBS =		
LDFLAGS =	
# {dependencies}
$(LIB):		$(LIB)(ASCII.o) $(LIB)(Archive.o) $(LIB)(Arguments.o) \
		$(LIB)(Calendar.o) $(LIB)(CallShell.o) $(LIB)(Clock.o) \
		$(LIB)(CoExpressio.o) $(LIB)(Conversions.o) \
		$(LIB)(Directories.o) $(LIB)(Environment.o) \
		$(LIB)(EtcGroup.o) $(LIB)(Files.o) $(LIB)(FtdIO.o) \
		$(LIB)(Functions.o) $(LIB)(GetPass.o) $(LIB)(InOut.o) \
		$(LIB)(MathLib.o) $(LIB)(Passwd.o) $(LIB)(PipeIO.o) \
		$(LIB)(Plot.o) $(LIB)(Processes.o) $(LIB)(RTErrors.o) \
		$(LIB)(RandomGener.o) $(LIB)(ReadIntCard.o) \
		$(LIB)(RealConv.o) $(LIB)(RealInOut.o) $(LIB)(ScanPwfile.o) \
		$(LIB)(StdFuncs.o) $(LIB)(StdIO.o) $(LIB)(Storage.o) \
		$(LIB)(StrSpec.o) $(LIB)(StrToNum.o) $(LIB)(StrToReal.o) \
		$(LIB)(Strings.o) $(LIB)(SysConf.o) $(LIB)(SysPerror.o) \
		$(LIB)(Terminal.o) $(LIB)(TimeIO.o)
SYM(ASCII.sy):	ASCII.d
		$(M2C) $(MFLAGS) ASCII.d
$(LIB)(ASCII.o): SYM(ASCII.sy) ASCII.m2
		$(M2C) $(MFLAGS) -c ASCII.m2
		ar rv $(LIB) ASCII.o && rm -f ASCII.o
SYM(Archive.sy): Archive.d /usr/local/lib/modula/SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) Archive.d
$(LIB)(Archive.o): SYM(ASCII.sy) SYM(Storage.sy) SYM(StrToNum.sy) \
		SYM(Strings.sy) /usr/local/lib/modula/SYM(SysPanic.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(Archive.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) Archive.m2
		$(M2C) $(MFLAGS) -c Archive.m2
		ar rv $(LIB) Archive.o && rm -f Archive.o
SYM(Arguments.sy): Arguments.d
		$(M2C) $(MFLAGS) Arguments.d
$(LIB)(Arguments.o): SYM(ASCII.sy) SYM(Arguments.sy) SYM(StrSpec.sy) \
		SYM(StrToNum.sy) SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) SYM(StdIO.sy) \
		Arguments.m2
		$(M2C) $(MFLAGS) -c Arguments.m2
		ar rv $(LIB) Arguments.o && rm -f Arguments.o
SYM(Calendar.sy): Calendar.d /usr/local/lib/modula/SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) Calendar.d
$(LIB)(Calendar.o): SYM(Environment.sy) SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysTime.sy) SYM(Calendar.sy) \
		Calendar.m2
		$(M2C) $(MFLAGS) -c Calendar.m2
		ar rv $(LIB) Calendar.o && rm -f Calendar.o
SYM(CallShell.sy): CallShell.d
		$(M2C) $(MFLAGS) CallShell.d
$(LIB)(CallShell.o): SYM(CallShell.sy) /usr/local/lib/modula/SYM(SysExec.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) \
		/usr/local/lib/modula/SYM(SysFork.sy) \
		/usr/local/lib/modula/SYM(SysSignal.sy) \
		/usr/local/lib/modula/SYM(SysWait.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) \
		/usr/local/lib/modula/SYM(UnixString.sy) CallShell.m2
		$(M2C) $(MFLAGS) -c CallShell.m2
		ar rv $(LIB) CallShell.o && rm -f CallShell.o
SYM(Clock.sy):	Clock.d /usr/local/lib/modula/SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) Clock.d
$(LIB)(Clock.o): /usr/local/lib/modula/SYM(Sys.sy) \
		/usr/local/lib/modula/SYM(SysTime.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(Clock.sy) \
		Clock.m2
		$(M2C) $(MFLAGS) -c Clock.m2
		ar rv $(LIB) Clock.o && rm -f Clock.o
SYM(CoExpressio.sy): CoExpressio.d
		$(M2C) $(MFLAGS) CoExpressio.d
$(LIB)(CoExpressio.o): SYM(CoExpressio.sy) SYM(Storage.sy) CoExpressio.m2
		$(M2C) $(MFLAGS) -c CoExpressio.m2
		ar rv $(LIB) CoExpressio.o && rm -f CoExpressio.o
SYM(Conversions.sy): Conversions.d
		$(M2C) $(MFLAGS) Conversions.d
$(LIB)(Conversions.o): SYM(Conversions.sy) Conversions.m2
		$(M2C) $(MFLAGS) -c Conversions.m2
		ar rv $(LIB) Conversions.o && rm -f Conversions.o
SYM(Directories.sy): Directories.d /usr/local/lib/modula/SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) Directories.d
$(LIB)(Directories.o): SYM(Storage.sy) SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(Sys.sy) \
		/usr/local/lib/modula/SYM(SysClose.sy) \
		/usr/local/lib/modula/SYM(SysLseek.sy) \
		/usr/local/lib/modula/SYM(SysOpen.sy) \
		/usr/local/lib/modula/SYM(SysStat.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) \
		SYM(Directories.sy) Directories.m2
		$(M2C) $(MFLAGS) -c Directories.m2
		ar rv $(LIB) Directories.o && rm -f Directories.o
SYM(Environment.sy): Environment.d
		$(M2C) $(MFLAGS) Environment.d
$(LIB)(Environment.o): SYM(Environment.sy) \
		/usr/local/lib/modula/SYM(SysLocation.sy) Environment.m2
		$(M2C) $(MFLAGS) -c Environment.m2
		ar rv $(LIB) Environment.o && rm -f Environment.o
$(LIB)(EtcGroup.o): SYM(EtcGroup.sy) SYM(Strings.sy) SYM(StdIO.sy) \
		SYM(ScanPwfile.sy) EtcGroup.m2
		$(M2C) $(MFLAGS) -c EtcGroup.m2
		ar rv $(LIB) EtcGroup.o && rm -f EtcGroup.o
SYM(EtcGroup.sy): EtcGroup.d
		$(M2C) $(MFLAGS) EtcGroup.d
SYM(Files.sy):	Files.d /usr/local/lib/modula/SYM(SystemTypes.sy) \
		SYM(StdIO.sy)
		$(M2C) $(MFLAGS) Files.d
$(LIB)(Files.o): /usr/local/lib/modula/SYM(SysLink.sy) \
		/usr/local/lib/modula/SYM(SysUnlink.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(StdIO.sy) \
		SYM(Files.sy) Files.m2
		$(M2C) $(MFLAGS) -c Files.m2
		ar rv $(LIB) Files.o && rm -f Files.o
SYM(FtdIO.sy):	FtdIO.d SYM(StdIO.sy)
		$(M2C) $(MFLAGS) FtdIO.d
$(LIB)(FtdIO.o): SYM(Conversions.sy) SYM(ReadIntCard.sy) SYM(StdIO.sy) \
		SYM(FtdIO.sy) FtdIO.m2
		$(M2C) $(MFLAGS) -c FtdIO.m2
		ar rv $(LIB) FtdIO.o && rm -f FtdIO.o
SYM(Functions.sy): Functions.d
		$(M2C) $(MFLAGS) Functions.d
$(LIB)(Functions.o): SYM(ASCII.sy) SYM(Functions.sy) SYM(Storage.sy) \
		SYM(Strings.sy) Functions.m2
		$(M2C) $(MFLAGS) -c Functions.m2
		ar rv $(LIB) Functions.o && rm -f Functions.o
SYM(GetPass.sy): GetPass.d
		$(M2C) $(MFLAGS) GetPass.d
$(LIB)(GetPass.o): SYM(ASCII.sy) SYM(GetPass.sy) SYM(RandomGener.sy) \
		/usr/local/lib/modula/SYM(SysOpen.sy) \
		/usr/local/lib/modula/SYM(SysTermIO.sy) SYM(StdIO.sy) \
		SYM(FtdIO.sy) GetPass.m2
		$(M2C) $(MFLAGS) -c GetPass.m2
		ar rv $(LIB) GetPass.o && rm -f GetPass.o
SYM(InOut.sy):	InOut.d
		$(M2C) $(MFLAGS) InOut.d
$(LIB)(InOut.o): SYM(Conversions.sy) SYM(InOut.sy) SYM(ReadIntCard.sy) \
		SYM(Terminal.sy) InOut.m2
		$(M2C) $(MFLAGS) -c InOut.m2
		ar rv $(LIB) InOut.o && rm -f InOut.o
SYM(MathLib.sy): MathLib.d
		$(M2C) $(MFLAGS) MathLib.d
$(LIB)(MathLib.o): SYM(MathLib.sy) MathLib.m2
		$(M2C) $(MFLAGS) -c MathLib.m2
		ar rv $(LIB) MathLib.o && rm -f MathLib.o
SYM(Passwd.sy):	Passwd.d
		$(M2C) $(MFLAGS) Passwd.d
$(LIB)(Passwd.o): SYM(Passwd.sy) SYM(Strings.sy) SYM(StdIO.sy) \
		SYM(ScanPwfile.sy) Passwd.m2
		$(M2C) $(MFLAGS) -c Passwd.m2
		ar rv $(LIB) Passwd.o && rm -f Passwd.o
$(LIB)(PipeIO.o): /usr/local/lib/modula/SYM(SysClose.sy) \
		/usr/local/lib/modula/SYM(SysDup.sy) \
		/usr/local/lib/modula/SYM(SysExec.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) \
		/usr/local/lib/modula/SYM(SysFork.sy) \
		/usr/local/lib/modula/SYM(SysPipe.sy) \
		/usr/local/lib/modula/SYM(SysWait.sy) \
		/usr/local/lib/modula/SYM(UnixString.sy) SYM(StdIO.sy) \
		SYM(PipeIO.sy) PipeIO.m2
		$(M2C) $(MFLAGS) -c PipeIO.m2
		ar rv $(LIB) PipeIO.o && rm -f PipeIO.o
SYM(PipeIO.sy):	PipeIO.d SYM(StdIO.sy)
		$(M2C) $(MFLAGS) PipeIO.d
SYM(Plot.sy):	Plot.d SYM(StdIO.sy)
		$(M2C) $(MFLAGS) Plot.d
$(LIB)(Plot.o):	SYM(SysPerror.sy) SYM(StdIO.sy) SYM(FtdIO.sy) SYM(Plot.sy) \
		Plot.m2
		$(M2C) $(MFLAGS) -c Plot.m2
		ar rv $(LIB) Plot.o && rm -f Plot.o
SYM(Processes.sy): Processes.d
		$(M2C) $(MFLAGS) Processes.d
$(LIB)(Processes.o): SYM(Processes.sy) SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(SysPanic.sy) Processes.m2
		$(M2C) $(MFLAGS) -c Processes.m2
		ar rv $(LIB) Processes.o && rm -f Processes.o
SYM(RTErrors.sy): RTErrors.d
		$(M2C) $(MFLAGS) RTErrors.d
$(LIB)(RTErrors.o): SYM(RTErrors.sy) SYM(StdIO.sy) SYM(FtdIO.sy) RTErrors.m2
		$(M2C) $(MFLAGS) -c RTErrors.m2
		ar rv $(LIB) RTErrors.o && rm -f RTErrors.o
SYM(RandomGener.sy): RandomGener.d
		$(M2C) $(MFLAGS) RandomGener.d
$(LIB)(RandomGener.o): SYM(RandomGener.sy) /usr/local/lib/modula/SYM(Sys.sy) \
		/usr/local/lib/modula/SYM(SysTime.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) RandomGener.m2
		$(M2C) $(MFLAGS) -c RandomGener.m2
		ar rv $(LIB) RandomGener.o && rm -f RandomGener.o
SYM(ReadIntCard.sy): ReadIntCard.d
		$(M2C) $(MFLAGS) ReadIntCard.d
$(LIB)(ReadIntCard.o): SYM(ReadIntCard.sy) ReadIntCard.m2
		$(M2C) $(MFLAGS) -c ReadIntCard.m2
		ar rv $(LIB) ReadIntCard.o && rm -f ReadIntCard.o
SYM(RealConv.sy): RealConv.d
		$(M2C) $(MFLAGS) RealConv.d
$(LIB)(RealConv.o): SYM(RealConv.sy) RealConv.m2
		$(M2C) $(MFLAGS) -c RealConv.m2
		ar rv $(LIB) RealConv.o && rm -f RealConv.o
SYM(RealInOut.sy): RealInOut.d SYM(StdIO.sy)
		$(M2C) $(MFLAGS) RealInOut.d
$(LIB)(RealInOut.o): SYM(RealConv.sy) SYM(Strings.sy) SYM(Terminal.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) SYM(RealInOut.sy) RealInOut.m2
		$(M2C) $(MFLAGS) -c RealInOut.m2
		ar rv $(LIB) RealInOut.o && rm -f RealInOut.o
SYM(ScanPwfile.sy): ScanPwfile.d SYM(ASCII.sy) SYM(StdIO.sy)
		$(M2C) $(MFLAGS) ScanPwfile.d
$(LIB)(ScanPwfile.o): SYM(StdIO.sy) SYM(ScanPwfile.sy) ScanPwfile.m2
		$(M2C) $(MFLAGS) -c ScanPwfile.m2
		ar rv $(LIB) ScanPwfile.o && rm -f ScanPwfile.o
SYM(StdFuncs.sy): StdFuncs.d
		$(M2C) $(MFLAGS) StdFuncs.d
$(LIB)(StdFuncs.o): SYM(Functions.sy) SYM(MathLib.sy) SYM(StdFuncs.sy) \
		StdFuncs.m2
		$(M2C) $(MFLAGS) -c StdFuncs.m2
		ar rv $(LIB) StdFuncs.o && rm -f StdFuncs.o
SYM(StdIO.sy):	StdIO.d /usr/local/lib/modula/SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) StdIO.d
$(LIB)(StdIO.o): /usr/local/lib/modula/SYM(Errno.sy) SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(SysClose.sy) \
		/usr/local/lib/modula/SYM(SysCreat.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) \
		/usr/local/lib/modula/SYM(SysLseek.sy) \
		/usr/local/lib/modula/SYM(SysOpen.sy) \
		/usr/local/lib/modula/SYM(SysRead.sy) \
		/usr/local/lib/modula/SYM(SysTermIO.sy) \
		/usr/local/lib/modula/SYM(SysWrite.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(StdIO.sy) \
		StdIO.m2
		$(M2C) $(MFLAGS) -c StdIO.m2
		ar rv $(LIB) StdIO.o && rm -f StdIO.o
SYM(Storage.sy): Storage.d
		$(M2C) $(MFLAGS) Storage.d
$(LIB)(Storage.o): SYM(Storage.sy) /usr/local/lib/modula/SYM(SysBreak.sy) \
		/usr/local/lib/modula/SYM(SysPanic.sy) Storage.m2
		$(M2C) $(MFLAGS) -c Storage.m2
		ar rv $(LIB) Storage.o && rm -f Storage.o
SYM(StrSpec.sy): StrSpec.d
		$(M2C) $(MFLAGS) StrSpec.d
$(LIB)(StrSpec.o): SYM(ASCII.sy) SYM(Storage.sy) SYM(StrSpec.sy) \
		SYM(Strings.sy) StrSpec.m2
		$(M2C) $(MFLAGS) -c StrSpec.m2
		ar rv $(LIB) StrSpec.o && rm -f StrSpec.o
SYM(StrToNum.sy): StrToNum.d
		$(M2C) $(MFLAGS) StrToNum.d
$(LIB)(StrToNum.o): SYM(ASCII.sy) SYM(StrToNum.sy) StrToNum.m2
		$(M2C) $(MFLAGS) -c StrToNum.m2
		ar rv $(LIB) StrToNum.o && rm -f StrToNum.o
SYM(StrToReal.sy): StrToReal.d
		$(M2C) $(MFLAGS) StrToReal.d
$(LIB)(StrToReal.o): SYM(RealConv.sy) SYM(StrToReal.sy) SYM(Strings.sy) \
		StrToReal.m2
		$(M2C) $(MFLAGS) -c StrToReal.m2
		ar rv $(LIB) StrToReal.o && rm -f StrToReal.o
SYM(Strings.sy): Strings.d
		$(M2C) $(MFLAGS) Strings.d
$(LIB)(Strings.o): SYM(Strings.sy) Strings.m2
		$(M2C) $(MFLAGS) -c Strings.m2
		ar rv $(LIB) Strings.o && rm -f Strings.o
SYM(SysConf.sy): SysConf.d
		$(M2C) $(MFLAGS) SysConf.d
$(LIB)(SysConf.o): SYM(Strings.sy) SYM(SysConf.sy) SysConf.m2
		$(M2C) $(MFLAGS) -c SysConf.m2
		ar rv $(LIB) SysConf.o && rm -f SysConf.o
SYM(SysPerror.sy): SysPerror.d
		$(M2C) $(MFLAGS) SysPerror.d
$(LIB)(SysPerror.o): /usr/local/lib/modula/SYM(Errno.sy) SYM(Strings.sy) \
		SYM(SysPerror.sy) SYM(StdIO.sy) SYM(FtdIO.sy) SysPerror.m2
		$(M2C) $(MFLAGS) -c SysPerror.m2
		ar rv $(LIB) SysPerror.o && rm -f SysPerror.o
SYM(Terminal.sy): Terminal.d
		$(M2C) $(MFLAGS) Terminal.d
$(LIB)(Terminal.o): SYM(Terminal.sy) SYM(StdIO.sy) Terminal.m2
		$(M2C) $(MFLAGS) -c Terminal.m2
		ar rv $(LIB) Terminal.o && rm -f Terminal.o
SYM(TimeIO.sy):	TimeIO.d SYM(Calendar.sy) SYM(StdIO.sy)
		$(M2C) $(MFLAGS) TimeIO.d
$(LIB)(TimeIO.o): SYM(ASCII.sy) SYM(Conversions.sy) SYM(Environment.sy) \
		SYM(Storage.sy) SYM(Strings.sy) SYM(Calendar.sy) \
		SYM(StdIO.sy) SYM(TimeIO.sy) SYM(FtdIO.sy) TimeIO.m2
		$(M2C) $(MFLAGS) -c TimeIO.m2
		ar rv $(LIB) TimeIO.o && rm -f TimeIO.o
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
