# {updated by mmm: Thu Feb 27 14:51:51 1997 GMT}
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
SRC =		Attributes.d Attributes.m2 Delay.d Delay.m2 FtdWin.d \
		FtdWin.m2 FunctionKey.d FunctionKey.m2 MainWin.d MainWin.m2 \
		Menus.d Menus.m2 Screen.d Screen.m2 TermInfo.d TermInfo.m2 \
		Windows.d Windows.m2
TAGSRC =	Attributes.m2 Delay.m2 FtdWin.m2 FunctionKey.m2 MainWin.m2 \
		Menus.m2 Screen.m2 TermInfo.m2 Windows.m2
OBJS =		
SYMS =		
REFS =		Attributes.r Delay.r FtdWin.r FunctionKey.r MainWin.r \
		Menus.r Screen.r TermInfo.r Windows.r
# {all}
LIB =		libm2.a
all:		$(LIB)
# {linkage}
LIBS =		
LDFLAGS =	
#
TermInfo.d:	TermInfo.t /usr/include/term.h term.sh
		term.sh
# {dependencies}
$(LIB):		$(LIB)(Attributes.o) $(LIB)(Delay.o) $(LIB)(FtdWin.o) \
		$(LIB)(FunctionKey.o) $(LIB)(MainWin.o) $(LIB)(Menus.o) \
		$(LIB)(Screen.o) $(LIB)(TermInfo.o) $(LIB)(Windows.o)
SYM(Attributes.sy): Attributes.d ./SYM(StdIO.sy) SYM(TermInfo.sy)
		$(M2C) $(MFLAGS) Attributes.d
$(LIB)(Attributes.o): ./SYM(StdIO.sy) ./SYM(Storage.sy) SYM(TermInfo.sy) \
		SYM(Attributes.sy) SYM(Delay.sy) Attributes.m2
		$(M2C) $(MFLAGS) -c Attributes.m2
		ar rv $(LIB) Attributes.o && rm -f Attributes.o
SYM(Delay.sy):	Delay.d ./SYM(StdIO.sy)
		$(M2C) $(MFLAGS) Delay.d
$(LIB)(Delay.o): ./SYM(StdIO.sy) SYM(Delay.sy) Delay.m2
		$(M2C) $(MFLAGS) -c Delay.m2
		ar rv $(LIB) Delay.o && rm -f Delay.o
SYM(FtdWin.sy):	FtdWin.d SYM(Windows.sy)
		$(M2C) $(MFLAGS) FtdWin.d
$(LIB)(FtdWin.o): ./SYM(ASCII.sy) ./SYM(Conversions.sy) SYM(Windows.sy) \
		SYM(FtdWin.sy) FtdWin.m2
		$(M2C) $(MFLAGS) -c FtdWin.m2
		ar rv $(LIB) FtdWin.o && rm -f FtdWin.o
SYM(FunctionKey.sy): FunctionKey.d ./SYM(StdIO.sy) SYM(TermInfo.sy)
		$(M2C) $(MFLAGS) FunctionKey.d
$(LIB)(FunctionKey.o): ./SYM(StdIO.sy) ./SYM(Storage.sy) ./SYM(Strings.sy) \
		./SYM(SysAlarm.sy) ./SYM(SysSignal.sy) ./SYM(SystemTypes.sy) \
		SYM(TermInfo.sy) SYM(Delay.sy) SYM(FunctionKey.sy) \
		FunctionKey.m2
		$(M2C) $(MFLAGS) -c FunctionKey.m2
		ar rv $(LIB) FunctionKey.o && rm -f FunctionKey.o
SYM(MainWin.sy): MainWin.d SYM(Windows.sy)
		$(M2C) $(MFLAGS) MainWin.d
$(LIB)(MainWin.o): ./SYM(ASCII.sy) ./SYM(FtdIO.sy) ./SYM(StdIO.sy) \
		./SYM(SysExit.sy) SYM(Windows.sy) SYM(FtdWin.sy) \
		SYM(MainWin.sy) MainWin.m2
		$(M2C) $(MFLAGS) -c MainWin.m2
		ar rv $(LIB) MainWin.o && rm -f MainWin.o
SYM(Menus.sy):	Menus.d SYM(Windows.sy)
		$(M2C) $(MFLAGS) Menus.d
$(LIB)(Menus.o): ./SYM(ASCII.sy) ./SYM(Storage.sy) ./SYM(Strings.sy) \
		SYM(Attributes.sy) SYM(FunctionKey.sy) SYM(Windows.sy) \
		SYM(FtdWin.sy) SYM(Menus.sy) Menus.m2
		$(M2C) $(MFLAGS) -c Menus.m2
		ar rv $(LIB) Menus.o && rm -f Menus.o
SYM(Screen.sy):	Screen.d ./SYM(StdIO.sy) SYM(TermInfo.sy)
		$(M2C) $(MFLAGS) Screen.d
$(LIB)(Screen.o): ./SYM(CallShell.sy) ./SYM(Environment.sy) ./SYM(StdIO.sy) \
		./SYM(Storage.sy) ./SYM(SysTermIO.sy) SYM(TermInfo.sy) \
		SYM(Delay.sy) SYM(Screen.sy) Screen.m2
		$(M2C) $(MFLAGS) -c Screen.m2
		ar rv $(LIB) Screen.o && rm -f Screen.o
SYM(TermInfo.sy): TermInfo.d
		$(M2C) $(MFLAGS) TermInfo.d
$(LIB)(TermInfo.o): ./SYM(Clock.sy) ./SYM(Conversions.sy) \
		./SYM(Environment.sy) ./SYM(StdIO.sy) ./SYM(Strings.sy) \
		./SYM(SysPanic.sy) SYM(TermInfo.sy) TermInfo.m2
		$(M2C) $(MFLAGS) -c TermInfo.m2
		ar rv $(LIB) TermInfo.o && rm -f TermInfo.o
SYM(Windows.sy): Windows.d ./SYM(StdIO.sy) SYM(TermInfo.sy) \
		SYM(Attributes.sy) SYM(FunctionKey.sy) SYM(Screen.sy)
		$(M2C) $(MFLAGS) Windows.d
$(LIB)(Windows.o): ./SYM(ASCII.sy) ./SYM(Environment.sy) ./SYM(FtdIO.sy) \
		./SYM(StdIO.sy) ./SYM(Storage.sy) ./SYM(StrToNum.sy) \
		./SYM(SysFcntl.sy) ./SYM(SysTermIO.sy) ./SYM(SystemTypes.sy) \
		SYM(TermInfo.sy) SYM(Attributes.sy) SYM(FunctionKey.sy) \
		SYM(Screen.sy) SYM(Windows.sy) Windows.m2
		$(M2C) $(MFLAGS) -c Windows.m2
		ar rv $(LIB) Windows.o && rm -f Windows.o
#
tags:		$(TAGSRC)
		mtags $(TAGSRC)
#
.PRECIOUS:	Pr.date
SPOOL =		lp
pr:		Pr.date
Pr.date:	$(SRC)
		pr $? | $(SPOOL)
		-touch Pr.date
#
clean:		; rm -f $(OBJS) $(SYMS) $(REFS)
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
#
Mods :=		$(filter %.m2,$(SRC))
Objs :=		$(patsubst %.m2,%.o,$(Mods))
.PHONY:		cleanlib
cleanlib:	; ar d $(LIB) $(Objs)
