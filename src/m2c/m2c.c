/* Ulm's Modula-2 Compiler: Command Line Interface
   Copyright (C) 1983-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Compiler is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   Ulm's Modula-2 Compiler is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: m2c.c,v 0.3 1998/06/17 09:09:39 borchert Exp borchert $
   ----------------------------------------------------------------------------
   $Log: m2c.c,v $
   Revision 0.3  1998/06/17  09:09:39  borchert
   - ``m2c -v'' is now permitted and prints a version-dependent banner
   - various code cleanups due to gcc -Wall

   Revision 0.2  1997/02/27  17:58:55  borchert
   major cleanup:
   - old code for UNIX Edition VII, XELOS, m68k etc. removed
   - various K&R-style code fixed
   - -LP parameter removed

   Revision 0.1  1997/02/27  15:10:54  borchert
   Initial revision

   ----------------------------------------------------------------------------
*/

/*
 *	afb 3/84
 *	rev afb 5/84 : output generally on stderr
 *	rev afb 6/84 : listings
 *	rev afb 11/84: MODPATH & MODLIB
 *	rev afb 4/86 : xelos
 *	rev afb 2/88 : Targon/31
 *	rev afb 2/89 : SUN
 *	rev afb 3/90 : protect against shared libraries of SunOS 4.0
 *	rev afb 7/90 : call m2e with error file
 *	rev afb 2/93 : FP
 *	rev afb 4/96 : one-binary version of compiler
 */

/*
 *	define ...
 *
 *	LIBDIR		default location of Modula-2 library
 *	SYSV		if we have Unix System V
 *	VERSION		fixed length string (e.g. "1.0")
 */

/* names of environment parameters */
#define	MODLIB	"MODLIB"	/* library directory */
#define	MODPATH	"MODPATH"	/* search path of directories */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <time.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/wait.h>

#ifdef DIRSIZ
#undef	DIRSIZ
#endif
#define	DIRSIZ	MAXNAMLEN

static char * alloc();
static char * callm();
static char * get_libdir();
static char getsuf();
static char * gettmp();
static char * scandir();
static char * search_lib();
static char * setsuf();
static char * strsave();
static char * test_lib();
static int test_filename();
static int test_sym();
static void call_m2e();
static int callsys();
static int compile();
static void copy();
static void die();
static void header();
static void isr();
static void lprintf(char * fmt, ...);
static void remove_file();
static void rmall();
static void symbolfile_list();
static void print_version();
static int e_access();


#define	MAXARG	4096

/*
 *	used commands
 */

char	*as  = { "/usr/ccs/bin/as" };
char	*ld  = { "/usr/ccs/bin/ld" };
char	*ar  = { "/usr/ccs/bin/ar" };

/*
 *	passes of the compiler (in libdir)
 *
 *	exit codes of the passes:
 *
 *	0 : okay
 *	1 : errors occured, nevertheless start next pass
 *	2 : stop the compilation and run lister
 *	3 : stop the compilation -- error messages have been printed
 */

char	modula[] = { "modula" };
char	*passes[] = { modula, NULL };
char	m2e[]	= { "m2e" };

#ifdef LIBDIR	/* may be overridden using the MODLIB environment parameter */
	char	*libdir = { LIBDIR };
#else
	char	*libdir = { "/usr/local/lib/modula/" };
#endif

char	*m2lib, *fplib;
char	*rts_prefix = { "" };
char	*rts[]  = { "m2rt0.o", NULL };
char	*mrts[] = { "mm2rt0.o", NULL };
char	*symlib = "SYM";
char	*reflib = "REF";
char	*prefix = NULL;
char	*ld_args[MAXARG];	/* pick up args for ld */
char	**ptr = ld_args;	/* points to current arg of ld */
char	*symfiles[MAXARG];	/* all symbol files in arg list */
char	**sym_ptr = symfiles;

/*
 *	default temp directory
 *	may be changed with the -T option
 */

char	*tmpdir	= { "/tmp" };

/*
 *	level of temp files
 */

#define	ALL_TMP	3
#define	LNK_TMP	2
#define	ASS_TMP	1
#define	CMP_TMP	0

#define	USAGE	fprintf(stderr, usage, name), die(100)


/*
 *	usage not complete but typical
 */

char usage[] = { "Usage: %s [-o outfile] [-S] [-c] [-L | -LP] [-Ttmpdir] [option...] file...\n" };
char	*name;

/*
 *	flags
 */

int	main_flag = 0;	/* if == 1: take following arg as main module */
int	Bflag = 0;	/* if on: use backup version */
int	aflag = 0;	/* if on: don't archive anything */
int	sflag = 0;	/* run compiler only */
int	Kflag = 0;	/* 1: enforce dummy keys for steady state tests */
int	cflag = 0;	/* no linkage */
int	pflag = 0;	/* if on: profile */
int	Mflag = 1;	/* if on: no stack checks */
			/* for UNIX systems with limited stack it's */
			/* useful to check for stack limit on default */
int	Cflag = 0;	/* if on: no range checks */
int	Nflag = 0;	/* if on: generate labels with line numbers */
int	vflag = 0;	/* verbose flag */
int	Vflag = 0;	/* no execution, verbose output only */
int	Rflag = 0;	/* 1: don't remove_file anything */
int	revision = 2;	/* Modula-2 version to be taken */
int	Lflag = 0;	/* if on: produce listing */
int	Hflag = 0;	/* support huge stack allocations */

char	* outfile = "a.out";	/* for -o option */
FILE	*listp = NULL;	/* pipeline to `pr' */
char * listing = NULL;	/* listing file */
int	SYM_found = 0;	/* if on: aflag isn't on and SYM exists */
int	ignore = 0;	/* if on: continue compiling further files;
                           even if previous compilation fails */

int main(argc, argv)
	int	argc;
	char	**argv;
{	char * cp;

	(name = strrchr(* argv, '/')) ? ++name : (name = * argv);

	if (signal(SIGINT, SIG_IGN) == SIG_DFL)
	{	signal(SIGINT, isr);
		signal(SIGQUIT, isr);
		signal(SIGHUP, isr);
	}
	else
		signal(SIGHUP, SIG_IGN);
	signal(SIGTERM, isr);
	signal(SIGPIPE, isr);

	setbuf(stderr, NULL);
	if (argc == 1)
		USAGE;
	*ptr++ = ld;
	*ptr++ = "-dn";

	/*
	 * work up arguments
	 *	use continue to proceed next arg instead of break
	 *	use break to stop flag processing
	 *	unknown flags are passed to ld
	 */
	while ( --argc && **++argv == '-' ) { switch ( *++*argv ) {
	default :
		/* pass argument to ld */
		*ptr++ = --*argv;
		continue;
	case 'o' :
		*ptr++ = "-o";
		if (--argc)
		{	outfile = *++argv;
			*ptr++ = outfile;
		}
		else
			USAGE;
		continue;
	case 'l' :
	case 'm' :
		--*argv;
		break;
	case 'S' :
		++sflag;
		++cflag;
		continue;
	case 'c' :
		++cflag;
		continue;
	case 'p' :
		++pflag;
		continue;
	case 'M' :
		Mflag = 0;
		continue;
	case 'C' :
		++Cflag;
		continue;
	case 'N' :
		++Nflag;
		continue;
	case 'V' :
		++Vflag;
	case 'v' :
		++vflag;
		continue;
	case 'R' :
		++Rflag;
		continue;
	case 'T' :
		if (*++* argv)
			tmpdir = *argv;
		else
			USAGE;
		continue;
	case 'B' :	/* backup version */
		++Bflag;
		if (*++* argv)
			prefix = *argv;
		else
			prefix = "old";
		continue;
	case '0' :	/* prefix of m2rt0.o */
		if (*++* argv)
			rts_prefix = *argv;
		else
			rts_prefix = "";
	case 'a' :
		++aflag;
		continue;
	case 'r' :
		if (*++* argv)
		{	revision = atoi(* argv);
			if (revision < 0 || revision > 2)
				USAGE;
		}
		else
			revision = 0;
		continue;
	case 'i' :
		++ignore;
		continue;
	case 'K' :
		++Kflag;
		continue;
	case 'H' :
		++Hflag;
		continue;
	case 'L' :
		++Lflag;
		continue;
	} break; }
	if (! argc)
	{	if (vflag)
		{	print_version(); exit(0);
		}
		else
			USAGE;
	}

	/*
	 *	look for environment parameters
	 */

#ifdef MODLIB
	if (cp = getenv(MODLIB))
	{	if (e_access(cp, /* execute = */ 1))
		{	fprintf(stderr, "Warning: MODLIB parameter ignored: ");
			perror(cp);
		}
		else
		{	libdir = alloc(strlen(cp) + /* "/" */ 1);
			strcpy(libdir, cp);
			strcat(libdir, "/");
		}
	}
#endif MODLIB

        m2lib = alloc(strlen(get_libdir()) + /* plibm2.a */ 9);
	strcpy(m2lib, get_libdir());
	if (pflag)
		strcat(m2lib, "plibm2.a");
	else
		strcat(m2lib, "libm2.a");

	die(compile(argc, argv));
	/*NOTREACHED*/
}	/* main */

/*
 *	scheduler
 *	return 0 if ok
 */

static int compile(argc, argv)
	int	argc;			/* rest of arguments */
	char	**argv;			/* without flags */
{	char	*args[MAXARG];
	int	argi;
	char	*cp;			/* only temporary used */
	register char **rts_ptr;	/* -> list of first args of ld */
	char * out;			/* output file of compilation */
	int objects = 0;		/* # objects for ld */
	int h_argc;			/* for first walk */
	char ** h_argv;			/* on the arguments */
	int exit_state = 0;		/* only if ignore is on */
#ifdef MODPATH
	char * modpath;
#endif MODPATH

	/*
	 * check for filename conventions,
	 * collect symbol files and archives with symbol files,
	 * and count objects
	 */
	h_argc = argc;
	h_argv = argv;
	while (h_argc)
	{	if (**h_argv != '-')
		{	if (test_filename(* h_argv, NULL))
				if (ignore)
					++exit_state;
				else
					return 1;
			else if (! test_sym(* h_argv))
				* sym_ptr++ = * h_argv;
			switch (getsuf(*h_argv))
			{	case 'm' :
				case 'o' :
				case 's' :
					if (! cflag && !sflag)
						++objects;
					break;
			}
		}
		++h_argv, --h_argc;
	}
	symbolfile_list();	/* collect symbol files from MODPATH,... */

	if (! cflag)
	{	for ( rts_ptr = pflag ? mrts : rts ; *rts_ptr ; ++rts_ptr )
		{	* ptr = alloc(strlen(* rts_ptr) + strlen(rts_prefix)
					+ strlen(get_libdir()));
			strcpy(* ptr, get_libdir());
			strcat(*ptr, rts_prefix);
			strcat(*ptr, *rts_ptr);
			++ptr;
		}
	}

	do	/* for all arguments ... */
	{	if (**argv == '-') switch (*++*argv) {
		case 'l' :
			*ptr++ = test_lib(--*argv);
			break;
		case 'm' :
			if (main_flag++ == 2)
			{	fprintf(stderr, "%s: -m specified twice\n",
					name);
				die(100);
			}
			break;
		case 'i' :
			++ignore;
			break;
		default :
			USAGE;
		}
		else if (! test_sym(* argv))
			;	/* symbolfile already entered in list */
		else switch (getsuf(*argv)) {
		case 'o' :
		case 'a' :
			if (! sflag && ! cflag)
				*ptr++ = *argv;
			break;
		case 's' :
			/* cannot be `.sy' */
			if (sflag)
				break;
			argi = 0;
			args[argi++] = as;
			args[argi++] = "-o";
			args[argi++] = setsuf(*argv, "o");
			*ptr++ = args[argi-1];	/* pick up object for ld */
			args[argi++] = *argv;
			args[argi++] = NULL;
			if ( callsys(as, args) )
				if (ignore)
					++exit_state;
				else
					return 1;
			break;
		case 'd' :
			/*
			 *	compile definition module
			 */

			if (test_filename(* argv, ".d") ||
			    callm(* argv) == NULL)
				if (ignore)
				{	++exit_state;
					break;
				}
				else
					return 1;
			/* exist SYM-library ??? */
			if (SYM_found ||
			    !aflag && e_access(symlib, /* write = */ 2) == 0)
			{	args[0] = ar;
				args[1] = "r";
				args[2] = symlib;
				args[3] = setsuf(*argv, "sy");
				args[4] = NULL;
				if (callsys(ar, args) == 0)
					remove_file(args[3]);
				SYM_found = 1;
			}
			break;
		case 'm' : /* .m2 */
			/* check for `.m2' */
			if (test_filename(* argv, ".m2"))
				if (ignore)
				{	++ exit_state;
					break;
				}
				else
					return 1;

			/*
			 *	compile
			 */

			if (! (out = callm(* argv)))
				if (ignore)
				{	++ exit_state;
					break;
				}
				else
					return 1;

			/*
			 *	if REF-library exists try to archive
			 *	the reference file
			 */

			if (!aflag && e_access(reflib, /* write = */ 2) == 0)
			{	args[0] = ar;
				args[1] = "r";
				args[2] = reflib;
				args[3] = setsuf(* argv, "r");
				args[4] = NULL;
				if (callsys(ar, args) == 0)
					remove_file(args[3]);
			}

			if (sflag) /* run the compiler only */
				break;

			/*
			 *	assembly
			 */

			argi = 0;
			args[argi++] = as;
			args[argi++] = "-o";
			if (objects == 1)
				args[argi++] = gettmp(LNK_TMP);
			else
				args[argi++] = setsuf(*argv, "o");
			*ptr++ = args[argi-1];	/* pick it up for linkage */
			args[argi++] = out;
			args[argi++] = NULL;
			if (callsys(as, args))
			{	fprintf(stderr,
				   "%s: error in compiler output\n", * argv);
				copy(out, setsuf(* argv, "s"));
				return 1;
			}
			
			rmall(ASS_TMP);
			break;
		default :
			fprintf(stderr,"%s: unknown suffix\n", *argv);
			if (ignore)
				++exit_state;
			else
				return 1;
		}
 	}
	while (--argc && **++argv);
	if (exit_state)
		return exit_state;
	if (cflag)
		return 0;
	/*
	 *	enter libraries into the arglist of ld
	 */

#ifdef MODPATH
	if (modpath = getenv(MODPATH))
		while (1)
		{	if (cp = strchr(modpath, ':'))
				* cp = '\0';
			while (* ptr = search_lib(modpath))
				++ ptr;
			if (cp)
			{	* cp = ':';
				modpath = cp + 1;
			}
			else
				break;
		}
#endif MODPATH

	/*
	 *	standard library
	 */

	*ptr++ = m2lib;
	*ptr = NULL;
	if (objects == 0)
		return 0;
	if (callsys(ld, ld_args))
		return 1;
	rmall(LNK_TMP);
	return 0;
}	/* compile */


static char * callm(src)
	char * src;
{	char * il1, * il2, * asc, * err;	/* interpass files */
	char * args[MAXARG];
	char buf[512];			/* libdir + passname */
	int argi;
	int defmod;			/* if on: src is a definition mod */
	char * out, *ref;		/* output files */
	int passes;
	int errors;
	char * callm = NULL;
	char ** sp;			/* -> symfiles */

	if (! Vflag && e_access(src, /* read = */ 4))
	{	perror(src);
		return callm;
	}

	defmod = getsuf(src) == 'd';
	if (Lflag)
	{	if (defmod)
			listing = setsuf(src, "ld");
		else
			listing = setsuf(src, "l");
		header(src);
	}
	il1 = gettmp(CMP_TMP);
	il2 = gettmp(CMP_TMP);
	asc = gettmp(CMP_TMP);
	err = gettmp(CMP_TMP);
	if (defmod)
		ref = NULL;
	else
		ref = gettmp(CMP_TMP);
	if (defmod)
		out = setsuf(src, "sy");
	else if (! sflag)
		out = gettmp(ASS_TMP);
	else
		out = setsuf(src, "s");

	argi = 0;
	args[argi++] = modula;

	/* temp files */
	args[argi++] = "-T";
	args[argi++] = il1;
	args[argi++] = il2;
	args[argi++] = asc;
	args[argi++] = err;

	/* output file */
	args[argi++] = "-o";
	args[argi++] = out;

	/* Modula-2 version */
	if (revision != 2) {
		args[argi++] = "-r";
		sprintf(buf, "%d", revision);
		args[argi++] = strsave(buf);
	}

	if (ref) {
		args[argi++] = "-f";
		args[argi++] = ref;
	}
	if (vflag) {
		args[argi++] = "-v";
	}
	if (Kflag) {
		args[argi++] = "-k";
	}
	if (Hflag) {
		/* -H was once -K... */
		args[argi++] = "-K";
	}
	args[argi++] = src;
	for (sp = symfiles; sp < sym_ptr; ++sp)
		args[argi++] = * sp;
	args[argi++] = NULL;

	errors = 1;
	strcpy(buf, get_libdir());
	strcat(buf, args[0]);
	args[0] = buf;
	switch (passes = callsys(buf, args))
	{	case 0 :
			errors = 0;
			break;
		case 1 :
			/* fatal errors printed by compiler itself */
			break;
		case 2 :
		case 3 :
			fprintf(stderr, "errors in %s\n", src);
			call_m2e(passes - 1, src, il1, il2);
			break;
		case 4 :
			fprintf(stderr, "errors in %s\n", src);
			call_m2e(3, src, err, err);
			break;
		default :
			fprintf(stderr, "%s aborted on %s\n", modula, src);
			break;
	}
	if (Lflag)
		lprintf("lister\n");
	if (! errors)
	{	if (! defmod)
			copy(ref, setsuf(src, "r"));
		else if (! SYM_found)
			* sym_ptr++ = setsuf(src, "sy");
		if (Lflag)
		{	call_m2e(0, src, il1, il2);
		}
		callm = out;
	}
	if (Lflag)
		lprintf("end compilation\n");
	rmall(CMP_TMP);
	listing = NULL;
	if (listp)
	{	pclose(listp);
		listp = NULL;
	}
	return callm;
}

/*
 *	print header of listing
 */

static void header(src)
	char * src;
{	FILE * fp;
	struct stat st_buf;
	struct tm * tm_buf;
	time_t today;
	static time_t compiler = 0;
	char ** cpp;
	char buf[128];
	char header[80];

	if (Vflag) return;

	/*
	 *	get time of compiler version
	 */
	if (! compiler)
		for (cpp = passes; * cpp; ++cpp)
		{	strcpy(buf, get_libdir());
			strcat(buf, * cpp);
			if (! stat(buf, & st_buf) && st_buf.st_mtime > compiler)
				compiler = st_buf.st_mtime;
		}
	tm_buf = localtime(& compiler);
	sprintf(header,
		"**** Modula-2 Compiler --- Version %6s --- %2d.%02d.%02d ****",
		VERSION,
		tm_buf->tm_mday,
		tm_buf->tm_mon+1, tm_buf->tm_year);
	if ((fp = fopen(listing, "w")) == NULL)
		perror(listing), die(100);
	fprintf(fp, "%s\n\n", header);

	/*
	 *	get actual time
	 */
	time(& today);
	tm_buf = localtime(& today);
	fprintf(fp, "**** Time: %2d:%02d:%02d", tm_buf->tm_hour,
		tm_buf->tm_min, tm_buf->tm_sec);
	fprintf(fp, "                     ");
	fprintf(fp, "Date: %2d.%02d.%02d ****\n\n", tm_buf->tm_mday,
		tm_buf->tm_mon+1, tm_buf->tm_year);

	if (Bflag)
		fprintf(fp, "**** backup version: >%s<\n", prefix);
	if (pflag)
		fprintf(fp, "**** profiling\n");
	if (Bflag || pflag)
		fprintf(fp, "\n");
	fprintf(fp, " source: %s\n", src);
	fclose(fp);
}

/*
 *	append line to listing file
 */

static void lprintf(char * fmt, ...)
{	
	va_list ap;
	FILE * fp;

	if (Vflag) return;
	va_start(ap, fmt);
	if ((fp = fopen(listing, "a")) == NULL)
		perror(listing), die(100);
	vfprintf(fp, fmt, ap);
	fclose(fp);
	va_end(ap);
}

/*
 *	call m2e in dependency of the last pass
 */

#define ERRFILE "m2_error"

static void call_m2e(pass, src, il1, il2)
	int pass;		/* pass number, ranges from 0 to 3 */
	char * src;		/* source file */
	char * il1, * il2;	/* interpass files */
{	char * args[8];
	int argi = 0;
	char * lib_m2e;
	char * errfile;

	if ((lib_m2e = malloc(strlen(libdir) +
			      (prefix ? strlen(prefix)+1 : 0) +
			      strlen(m2e) + 1)) == NULL)
	{	perror("malloc"); die(1);
	}
	strcpy(lib_m2e, libdir);
	if (prefix)
	{	strcat(lib_m2e, prefix);
		strcat(lib_m2e, "/");
	}
	strcat(lib_m2e, m2e);

	args[argi++] = lib_m2e;
	if (pass == 3)
		args[argi++] = "-4";
	if (Lflag)
		args[argi++] = "-L";

	/* give errorfile as argument */
	args[argi++] = "-e";
	errfile = malloc(strlen(get_libdir()) + strlen(ERRFILE) + 2);
	if (! errfile)
	{	perror("malloc"); die(1);
	}
	strcpy(errfile, get_libdir());
	strcat(errfile, ERRFILE);
	args[argi++] = errfile;

	args[argi++] = src;
	if (pass > 0)
		args[argi++] = pass % 2 ? il2 : il1;
	/* else symbolfiles missing */
	args[argi++] = NULL;
	callsys(lib_m2e, args);	/* ignore results */
}

/*
 *	test if library is in lib-directory
 *	and expand '-lxxx' to 'libdir/libxxx.a'
 */

static char * test_lib(option)
	char * option;
{	char * libdir;
	char * lib;

	libdir = get_libdir();
	lib = alloc(strlen(libdir) + strlen(option) + /* lib */ 3+ /* .a */ 2);
	strcpy(lib, libdir);
	strcat(lib, "lib");
	option += 2;	/* scan -l */
	strcat(lib, option);
	strcat(lib, ".a");
	if (e_access(lib, /* read = */ 4))
		return option;
	return lib;
}

/*
 *	test if file ends in `SYM' or `.sy'
 *	return 0 if ok
 */

static int test_sym(file)
	char * file;
{	char * cp;

	(cp = strrchr(file, '/')) ? ++cp : (cp = file);
	if (strcmp(cp, symlib) == 0)
		return 0; /* ok */
	if ((cp = strrchr(cp, '.')) && strcmp(cp, ".sy") == 0)
		return test_filename(file, ".sy");
	return 1; /* not ok */
}

/*
 *	check string length of basename of filename <= DIRSIZ-3
 *	suffix may be NULL
 *	return 0 if ok
 */

static int test_filename(file, suffix)
	char * file;
	char * suffix;
{	char * cp;

	(cp = strrchr(file, '/')) ? ++cp : (cp = file);
	if (! suffix && strcmp(cp, symlib) == 0)
		return 0; /* ok */
	if (! suffix)
		if(! (suffix = strrchr(cp, '.')))
			suffix = "";
	if (strlen(cp) > DIRSIZ)
	{	fprintf(stderr, "%s: Name too long", file);
		strcpy(cp+DIRSIZ-3, suffix);
		fprintf(stderr, "; name your file better %s\n", cp);
		return 1; /* not ok */
	}
	cp = strrchr(cp, '.');
	if (! cp)
	{	fprintf(stderr, "%s: suffix missing\n", file);
		return 1;
	}
	else if (strcmp(cp, suffix))
	{	fprintf(stderr, "%s: incorrect suffix", file);
		strcpy(cp, suffix);
		fprintf(stderr, "; name your file better %s\n", file);
		return 1;
	}
	return 0;	/* ok */
}

/*
 *	return library directory = libdir + prefix
 */

static char * get_libdir()
{	static char * dir = NULL;

	if (dir)
		return dir;
	if (prefix)
	{	dir = alloc(strlen(libdir) + strlen(prefix));
		strcpy(dir, libdir);
		strcat(dir, prefix);
		strcat(dir, "/");
	}
	else
		dir = libdir;
	return dir;
}

/*
 *	search for `.a' file in specified directory
 */
static char * search_lib(dir)
	char * dir;
{
	if (pflag)
		return scandir(dir, "p", "a");
	else
		return scandir(dir, "^p", "a");
}

/*
 *	get all files with prefix `prefix' (or files not starting with
 *	`prefix', if the first char of prefix is '^') and
 *	suffix `suffix' in directory `dir'
 *	prefix (but not suffix) may be NULL
 */

static char * scandir(dir, prefix, suffix)
	char * dir;
	char * prefix;
	char * suffix;
{	struct dirent * dirbuf;
	static DIR * fp = NULL;		/* for reading the directory */
	static char * old_dir = NULL;	/* last dir argument */
	char * file;			/* qualified filename in dir */
	char * cp;			/* cp -> suffix of file in dir */
	int neg = 0;			/* if on: 1st char of prefix was '^' */

	if (dir != old_dir)
	{	if (fp)
			closedir(fp);
		if ((fp = opendir(dir)) == NULL)
		{	perror(dir);
			return NULL;
		}
		old_dir = dir;
	}

	if (prefix && * prefix == '^')
	{	++ neg;
		++ prefix;
	}
	while (dirbuf = readdir(fp))
	{	cp = strrchr(dirbuf->d_name, '.');
		if (! cp)
			continue;
		++cp;
		if (strcmp(cp, suffix))
			continue;
		if (prefix)
			if (strncmp(prefix, dirbuf->d_name, strlen(prefix)))
			{	if (! neg)
					continue;
			}
			else if (neg)
				continue;
		file = alloc(strlen(dir)+strlen(dirbuf->d_name)+1);
		strcpy(file, dir);
		strcat(file, "/");
		strcat(file, dirbuf->d_name);
		if (e_access(file, /* read = */ 4))
			continue;
		return file;
	}
	closedir(fp);
	old_dir = NULL;
	fp = NULL;
	return NULL;
}

/*
 *	get symbol file list from MODPATH and standard places
 */

static void symbolfile_list()
{	char * modpath;
	char * cp;
	char * dir;
	char * file;

#ifdef MODPATH
	if (modpath = getenv(MODPATH))
		while (1)
		{	if (cp = strchr(modpath, ':'))
				* cp = '\0';
			dir = alloc(strlen(modpath)+DIRSIZ+1);
			if (modpath[0] == '\0')
				strcpy(dir, ".");
			else
				strcpy(dir, modpath);
			file = alloc(strlen(dir)+1+strlen(symlib));
			strcpy(file, dir);
			strcat(file, "/");
			strcat(file, symlib);
			if (! e_access(file, 4))
				* sym_ptr++ = file;
			while (* sym_ptr = scandir(dir, NULL, "sy"))
				++sym_ptr;
			if (cp)
			{	* cp = ':';
				modpath = cp + 1;
			}
			else
				break;
		}
#endif MODPATH
	/* check for `SYM' file */
	if (! e_access(symlib, /* read = */ 4))
		* sym_ptr++ = symlib;
	/* check for libdir/SYM file */
	cp = alloc(strlen(get_libdir()) + strlen(symlib));
	strcpy(cp, get_libdir());
	strcat(cp, symlib);
	if (! e_access(cp, /* read = */ 4))
		* sym_ptr++ = cp;
}

/*
 *	string allocation routine with check
 *	`length' means string length; not size of string
 */

static char * alloc(length)
	int length;
{	char * cp;

	if ((cp = calloc(length+1, sizeof(char))) == NULL)
	{	perror(name);
		die(100);
	}
	return cp;
}

static char * strsave(s)
	char * s;
{	char * cp;

	cp = alloc(strlen(s));
	strcpy(cp, s);
	return cp;
}

/*
 *	execute file with args
 */

int pid = 0;	/* process id of son */

static int callsys(file, args)
	char *file, **args; 
{	int son, status, t;
	register char **ptr;
	int exit_code;
	void (*sigint)();
	void (*sigquit)();

	switch (son = fork()) {
	case 0 :
		if (vflag)
		{	fprintf(stderr, "%s", *args);
			for (ptr = args+1; *ptr; ++ptr)
				fprintf(stderr, " %s", *ptr);
			fprintf(stderr, "\n");
		}
		if (Vflag)
			exit(0);
		if (Lflag && listing)
		{	int fd;

			fd = open(listing, /* write = */ 1);
			dup2(fd, /* stderr = */ 2);
			close(fd);
			lseek(2, 0L, 2); /* append */
			dup2(2, /* stdout = */ 1);
		}
		execvp(file, args);
		perror(file);
		die(100);
	case -1 :
		perror(name);
		die(100);
	default :
		sigint = signal(SIGINT, SIG_IGN);
		sigquit = signal(SIGQUIT, SIG_IGN);
		pid = son;
		while(son != wait(&status))
			;
		pid = 0;
		signal(SIGINT, sigint);
		signal(SIGQUIT, sigquit);
		if (t = status&0377)
		{	if (t == SIGINT || t == SIGQUIT)
				fprintf(stderr, "%s has been interrupted\n",
					file);
			else if (t == SIGILL || t == SIGFPE || t == SIGBUS ||
				 t == SIGSEGV || t == SIGSYS)
				fprintf(stderr, "Fatal error in %s\n", file);
			else if (t == SIGHUP)
				fprintf(stderr, "Hangup signal send to %s\n",
					file);
			else if (t == SIGTERM)
				fprintf(stderr, "Termination signal send to %s\n", file);
			else
				fprintf(stderr, "%s aborted\n", file);
			die(100);
		}
		exit_code = (status >> 8) & 0377;
		if (exit_code && vflag && ! Vflag)
			fprintf(stderr, "*** exit code = %d\n", exit_code);
		return exit_code;
	}
}

/*
 *	filename manipulation
 */

static char getsuf(filename)
	char	*filename;
{	char	*ptr;

	if ( (ptr = strrchr(filename,'.')) == NULL )
		return '\0';
	else
		return *++ptr;
}

static char * setsuf(filename, suf)
	char	*filename;
	char	*suf;
{	register char *ptr;

	if ( (ptr = malloc(strlen(filename)+strlen(suf)+1)) == NULL )
		perror(name), die(1);
	strcpy(ptr, filename);
	filename = ptr;
	if (ptr = strrchr(filename, '.'))
		*++ptr = '\0';
	else
	{	ptr = filename + strlen(filename);
		*ptr = '.';
		*++ptr = '\0';
	}
	strcat(filename, suf);
	return filename;
}

/*
 *	temp file handling
 */

struct chain {
	char * c_name;
	int c_level;
	struct chain * c_next;
} * tmpfiles = NULL;

static char * gettmp(level)
	int level;
{	char buf[128];
	char * tmp;
	struct chain * new;
	static int unique = 0;

	sprintf(buf, "%s/mtm%d_%d", tmpdir, (int)getpid(), unique++);
	tmp = strsave(buf);
	if ((new = (struct chain *) calloc(sizeof(struct chain), 1)) == NULL)
	{	perror("calloc");
		die(100);
	}
	new->c_name = tmp;
	new->c_next = tmpfiles;
	new->c_level = level;
	tmpfiles = new;
	return tmp;
}

/*
 *	exit with clean up
 */

static void die(code)
	int	code;
{
	rmall(ALL_TMP);
	exit(code);
}

/*
 *	interrupt service routine
 */

static void isr(code)
	int code;
{
	signal(code, SIG_IGN);
	fprintf(stderr, "%s has been interrupted\n", name);
	if (pid)
		kill(pid, code);
	die(code);
}

/*
 *	remove_file all temp files with c_level <= level
 */

static void rmall(level)
	int level;
{	struct chain * tp;
	struct chain * prev = NULL;

	tp = tmpfiles;
	while (tp)
	{	if (tp->c_level <= level)
		{	remove_file(tp->c_name);	/* ignore result */
			if (prev)
				prev->c_next = tp->c_next;
			else
				tmpfiles = tp->c_next;
		}
		else
			prev = tp;
		tp = tp->c_next;
	}
}

/*
 *	own check routine;
 *	if real user id != effective user id
 *	access(2) would return wrong results
 *	set (if needed) errno
 *
 *	return 0 if ok
 */

static int e_access(file, mode)
	char * file;
	int mode;
{	static int use_access = 0;	/* if on: use access(2) */
	static int init = 0;
	struct stat statbuf;
	int fd;
	extern int errno;

	if (! init)
	{	init = 1;
		use_access = getuid() == geteuid() &&
			     getgid() == getegid();
	}
	if (use_access)
		return access(file, mode);
	switch (mode)
	{	case 1 : /* execute */
			if (stat(file, & statbuf))
				return -1;
			errno = EACCES;	/* for perror */
			return (statbuf.st_mode & S_IEXEC) == 0;
		case 2 : /* write */
		case 4 : /* read */
			/*	2 ---> 1	*/
			/*	4 ---> 0	*/
			mode = (4 - mode) / 2;
			return ((fd = open(file, mode)) < 0) || close(fd);
		case 0 : /* existance */
			return stat(file, & statbuf);
	}
	return 0;
}

/*
 *	built-in commands for efficiency
 */

static void copy(from, to)
	char * from;
	char * to;
{	FILE * f, * t;
	int ch;

	if (vflag || Vflag)
	{	fprintf(stderr, "cp %s %s\n", from, to);
		if (Vflag)
			return;
	}
	if ((f = fopen(from, "r")) == NULL)
	{	perror(from);
		return;
	}
	if ((t = fopen(to, "w")) == NULL)
	{	perror(to);
		fclose(f);
		return;
	}
	while ((ch = getc(f)) != EOF)
		putc(ch, t);
	fclose(f);
	fclose(t);
}

static void remove_file(file)
	char * file;
{
	if (Rflag)
		return;
	if (Vflag || vflag)
	{	fprintf(stderr, "rm -f %s\n", file);
		if (Vflag)
			return;
	}
	unlink(file); /* ignore result */
}

static void print_version()
{
	printf("\n");
	printf("This is Ulm's Modula-2 Compiler, version %s.\n\n", VERSION);
	printf("Copyright (C) 1983-1999 Universitaet Ulm, SAI, 89069 Ulm, Germany\n");
	printf("          (C) 1979-1981 Institut fuer Informatik, ETH Zuerich, Switzerland\n");
	printf("\n");
	printf("Ulm's Modula Compiler may be distributed under the terms of the\n");
	printf("GNU General Public License which may be found in the source kit\n");
	printf("or under http://www.gnu.org/copyleft/gpl.html.\n");
	printf("\n");
#ifdef URL
	printf("Documentation about Ulm's Modula-2 Compiler may be found at:\n");
	printf("   %s\n\n", URL);
#endif
}
