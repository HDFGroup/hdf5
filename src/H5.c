/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char             RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*LINTLIBRARY */
/*+
   FILE
   hdf5.c
   HDF library support routines

   EXPORTED ROUTINES
   H5dont_atexit    -- Indicate that an 'atexit' routine is _not_ to be installed
   H5version        -- Check the version of the library

   LIBRARY-SCOPED ROUTINES
   H5_init_library      -- initialize the HDF5 library
   H5_term_library      -- shut-down the HDF5 library
   H5_init_thread       -- initialize thread-specific information

   LOCAL ROUTINES
   H5_init_interface    -- initialize the H5 interface
   + */

#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/resource.h>

/* We need this on Irix64 even though we've included stdio.h as documented */
FILE *fdopen(int fd, const char *mode);

/* private headers */
#include <H5private.h>          /*library                 		*/
#include <H5ACprivate.h>        /*cache                           	*/
#include <H5Bprivate.h>         /*B-link trees                    	*/
#include <H5Eprivate.h>         /*error handling          		*/
#include <H5Iprivate.h>		/*atoms					*/
#include <H5MMprivate.h>        /*memory management               	*/
#include <H5Pprivate.h>		/*property lists			*/
#include <H5Sprivate.h>		/*data spaces				*/
#include <H5Tprivate.h>         /*data types                      	*/
#include <H5Zprivate.h>		/*filters				*/

#define PABLO_MASK      H5_mask

hbool_t                 library_initialize_g = FALSE;
hbool_t                 thread_initialize_g = FALSE;
hbool_t                 install_atexit_g = TRUE;
static FILE		*H5_trace_g = NULL;

typedef struct H5_exit {
    void                    (*func) (void);     /* Interface function to call during exit */
    struct H5_exit         *next;       /* Pointer to next node with exit function */
} H5_exit_t;

H5_exit_t              *lib_exit_head;  /* Pointer to the head of the list of 'atexit' functions */

/* Interface initialization */
static hbool_t          interface_initialize_g = FALSE;
#define INTERFACE_INIT H5_init_interface
static herr_t           H5_init_interface(void);

/*--------------------------------------------------------------------------
NAME
   H5_init_library -- Initialize library-global information
USAGE
    herr_t H5_init_library()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any library-global data or routines.

--------------------------------------------------------------------------*/
herr_t 
H5_init_library(void)
{
    FUNC_ENTER_INIT(H5_init_library, NULL, FAIL);

    /* Install atexit() library cleanup routine */
    if (install_atexit_g == TRUE)
        if (HDatexit(&H5_term_library) != 0)
            HRETURN_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL,
                          "unable to register atexit function");

    /*
     * Initialize interfaces that might not be able to initialize themselves
     * soon enough.
     */
    if (H5T_init_interface() < 0) {
        HRETURN_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL,
                      "unable to initialize type interface");
    }

#ifdef H5_DEBUG_API
    {
	/* Turn on tracing? */
	const char *s = getenv ("HDF5_TRACE");
	if (s && isdigit(*s)) {
	    int fd = (int)HDstrtol (s, NULL, 0);
	    H5_trace_g = HDfdopen (fd, "w");
	}
    }
#endif

    FUNC_LEAVE(SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5_add_exit
 PURPOSE
    Add an exit routine to the list of routines to call during 'atexit'
 USAGE
    herr_t H5_add_exit(func)
        void (*func)(void);     IN: Function pointer of routine to add to chain

 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Pre-pend the new function to the list of function to call during the exit
    process.  These routines are responsible for free'ing static buffers, etc.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Don't make assumptions about the environment during the exit procedure...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5_add_exit(void (*func)(void))
{
    H5_exit_t              *new_exit;

    FUNC_ENTER_INIT(H5_add_exit, NULL, FAIL);

    assert(func);

    if (NULL==(new_exit = H5MM_calloc(sizeof(H5_exit_t)))) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
		       "memory allocation failed");
    }

    new_exit->func = func;
    new_exit->next = lib_exit_head;
    lib_exit_head = new_exit;

    FUNC_LEAVE(SUCCEED);
}                               /* end H5_add_exit() */

/*--------------------------------------------------------------------------
 NAME
    H5_term_library
 PURPOSE
    Terminate various static buffers and shutdown the library.
 USAGE
    void H5_term_library()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Walk through the shutdown routines for the various interfaces and 
    terminate them all.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Should only ever be called by the "atexit" function, or real power-users.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
void
H5_term_library(void)
{
    H5_exit_t              *temp;

    temp = lib_exit_head;
    while (lib_exit_head != NULL) {
        (*lib_exit_head->func) ();
        lib_exit_head = lib_exit_head->next;
        HDfree(temp);
        temp = lib_exit_head;
    }                           /* end while */
}                               /* end H5_term_library() */

/*--------------------------------------------------------------------------
NAME
   H5_init_thread -- Initialize thread-specific information
USAGE
    void H5_init_thread()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any thread-specific data or routines.

--------------------------------------------------------------------------*/
herr_t 
H5_init_thread(void)
{
    FUNC_ENTER_INIT(H5_init_thread, NULL, FAIL);

    /* Add the "thread termination" routine to the exit chain */
    if (H5_add_exit(&H5_term_thread) == FAIL)
        HRETURN_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL,
                      "unable to set thread atexit function");

    FUNC_LEAVE(SUCCEED);
}                               /* H5_init_thread */

/*--------------------------------------------------------------------------
 NAME
    H5_term_thread
 PURPOSE
    Terminate various thread-specific objects
 USAGE
    void H5_term_thread()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Release the error stack and any other thread-specific resources allocated
    on a "per thread" basis.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
void
H5_term_thread(void)
{/*void*/}

/*--------------------------------------------------------------------------
NAME
   H5_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t 
H5_init_interface(void)
{
    FUNC_ENTER(H5_init_interface, FAIL);

    FUNC_LEAVE(SUCCEED);
}                               /* H5_init_interface */

/*--------------------------------------------------------------------------
 NAME
    H5dont_atexit
 PURPOSE
    Indicates to the library that an 'atexit()' routine is _not_ to be installed
 USAGE
    herr_t H5dont_atexit(void)
 RETURNS
    Returns SUCCEED/FAIL
 DESCRIPTION
        This routine indicates to the library that an 'atexit()' cleanip routine
    should not be installed.  The major (only?) purpose for this is in
    situations where the library is dynamically linked into an application and
    is un-linked from the application before 'exit()' gets callled.  In those
    situations, a routine installed with 'atexit()' would jump to a routine
    which was no longer in memory, causing errors.
        In order to be effective, this routine _must_ be called before any other
    HDF function calls, and must be called each time the library is loaded/
    linked into the application. (the first time and after it's been un-loaded) 
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    If this routine is used, certain memory buffers will not be de-allocated,
    although in theory a user could call HPend on their own...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t 
H5dont_atexit(void)
{
    FUNC_ENTER_INIT(H5dont_atexit, NULL, FAIL);

    if (install_atexit_g == TRUE)
        install_atexit_g = FALSE;

    FUNC_LEAVE(SUCCEED);
}                               /* end H5dont_atexit() */


/*-------------------------------------------------------------------------
 * Function:	H5version
 *
 * Purpose:	Returns the library version numbers through arguments. MAJNUM
 *		will be the major revision number of the library, MINNUM the
 *		minor revision number, and RELNUM the release revision number.
 *
 * Note:	When printing an HDF5 version number it should be printed as
 *
 * 		printf("%u.%u.%u", maj, min, rel)		or
 *		printf("version %u.%u release %u", maj, min, rel)
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 * Modifications:
 * 	Robb Matzke, 4 Mar 1998
 *	Now use "normal" data types for the interface.  Any of the arguments
 *	may be null pointers
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5version(unsigned *majnum, unsigned *minnum, unsigned *relnum)
{
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5version, FAIL);

    /* Set the version information */
    if (majnum) *majnum = H5_VERS_MAJOR;
    if (minnum) *minnum = H5_VERS_MINOR;
    if (relnum) *relnum = H5_VERS_RELEASE;

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5vers_check
 *
 * Purpose:	Verifies that the arguments match the version numbers
 *		compiled into the library.  This function is intended to be
 *		called from user to verify that the versions of header files
 *		compiled into the application match the version of the hdf5
 *		library.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	abort()
 *
 * Programmer:	Robb Matzke
 *              Tuesday, April 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5vers_check (unsigned majnum, unsigned minnum, unsigned relnum)
{
    /* Don't initialize the library quite yet */
    
    if (H5_VERS_MAJOR!=majnum || H5_VERS_MINOR!=minnum ||
	H5_VERS_RELEASE!=relnum) {
	fputs ("Warning! The HDF5 header files included by this application "
	       "do not match the\nversion used by the HDF5 library to which "
	       "this application is linked. Data\ncorruption or segmentation "
	       "faults would be likely if the application were\nallowed to "
	       "continue.\n", stderr);
	fprintf (stderr, "Headers are %u.%u.%u, library is %u.%u.%u\n",
		 majnum, minnum, relnum, 
		 H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);
	fputs ("Bye...\n", stderr);
	abort ();
    }
    return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:    H5open
 *
 * Purpose:     Initialize the library.  This is normally called
 *              automatically, but if you find that an HDF5 library function
 *              is failing inexplicably, then try calling this function
 *              first.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5open(void)
{
    FUNC_ENTER(H5open, FAIL);
    /* all work is done by FUNC_ENTER() */
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5close
 *
 * Purpose:	Terminate the library and release all resources.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5close (void)
{
    /*
     * Don't call FUNC_ENTER() since we don't want to initialize the whole
     * thing just to release it all right away.  It is safe to call this
     * function for an uninitialized library.
     */
    H5_term_library ();
    return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:	HDfprintf
 *
 * Purpose:	Prints the optional arguments under the control of the format
 *		string FMT to the stream STREAM.  This function takes the
 *		same format as fprintf(3c) with a few added features:
 *
 *		The conversion modifier `H' refers to the size of an
 *		`hsize_t' or `hssize_t' type.  For instance, "0x%018Hx"
 *		prints an `hsize_t' value as a hex number right justified and
 *		zero filled in an 18-character field.
 *
 *		The conversion `a' refers to an `haddr_t*' type. Field widths
 *		and other flags are ignored and the address is printed by
 *		calling H5F_addr_print().
 *
 * Bugs:	Return value will be incorrect if `%a' appears in the format
 *		string.
 *
 * Return:	Success:	Number of characters printed
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
HDfprintf (FILE *stream, const char *fmt, ...)
{
    int		n=0, nout = 0;
    int		fwidth, prec;
    int		zerofill;
    int		leftjust;
    int		plussign;
    int		ldspace;
    int		prefix;
    char	modifier[8];
    int		conv;
    char	*rest, template[128];
    const char	*s;
    va_list	ap;
    
    assert (stream);
    assert (fmt);

    va_start (ap, fmt);
    while (*fmt) {
	fwidth = prec = 0;
	zerofill = 0;
	leftjust = 0;
	plussign = 0;
	prefix = 0;
	ldspace = 0;
	modifier[0] = '\0';

	if ('%'==fmt[0] && '%'==fmt[1]) {
	    putc ('%', stream);
	    fmt += 2;
	    nout++;
	} else if ('%'==fmt[0]) {
	    s = fmt+1;

	    /* Flags */
	    while (strchr ("-+ #", *s)) {
		switch (*s) {
		case '-':
		    leftjust = 1;
		    break;
		case '+':
		    plussign = 1;
		    break;
		case ' ':
		    ldspace = 1;
		    break;
		case '#':
		    prefix = 1;
		    break;
		}
		s++;
	    }
	    
	    /* Field width */
	    if (isdigit (*s)) {
		zerofill = ('0'==*s);
		fwidth = (int)strtol (s, &rest, 10);
		s = rest;
	    } else if ('*'==*s) {
		fwidth = va_arg (ap, int);
		if (fwidth<0) {
		    leftjust = 1;
		    fwidth = -fwidth;
		}
		s++;
	    }

	    /* Precision */
	    if ('.'==*s) {
		s++;
		if (isdigit (*s)) {
		    prec = (int)strtol (s, &rest, 10);
		    s = rest;
		} else if ('*'==*s) {
		    prec = va_arg (ap, int);
		    s++;
		}
		if (prec<1) prec = 1;
	    }

	    /* Type modifier */
	    if (strchr ("ZHhlq", *s)) {
		switch (*s) {
		case 'H':
		    if (sizeof(hsize_t)==sizeof(long)) {
			strcpy (modifier, "l");
		    } else if (sizeof(hsize_t)==sizeof(long long)) {
			strcpy (modifier, PRINTF_LL_WIDTH);
		    }
		    break;
		case 'Z':
		    if (sizeof(size_t)==sizeof(long)) {
			strcpy (modifier, "l");
		    } else if (sizeof(size_t)==sizeof(long long)) {
			strcpy (modifier, PRINTF_LL_WIDTH);
		    } else if (sizeof(size_t)==sizeof(int)) {
			modifier[0] = '\0';
		    }
		    break;
		    
		default:
		    modifier[0] = *s;
		    modifier[1] = '\0';
		    break;
		}
		s++;
	    }

	    /* Conversion */
	    conv = *s++;

	    /* Create the format template */
	    sprintf (template, "%%%s%s%s%s%s",
		     leftjust?"-":"", plussign?"+":"",
		     ldspace?" ":"", prefix?"#":"", zerofill?"0":"");
	    if (fwidth>0) {
		sprintf (template+strlen (template), "%d", fwidth);
	    }
	    if (prec>0) {
		sprintf (template+strlen (template), ".%d", prec);
	    }
	    if (*modifier) {
		sprintf (template+strlen (template), "%s", modifier);
	    }
	    sprintf (template+strlen (template), "%c", conv);
	    

	    /* Conversion */
	    switch (conv) {
	    case 'd':
	    case 'i':
		if (!strcmp (modifier, "h")) {
		    short x = va_arg (ap, short);
		    n = fprintf (stream, template, x);
		} else if (!*modifier) {
		    int x = va_arg (ap, int);
		    n = fprintf (stream, template, x);
		} else if (!strcmp (modifier, "l")) {
		    long x = va_arg (ap, long);
		    n = fprintf (stream, template, x);
		} else {
		    long long x = va_arg (ap, long long);
		    n = fprintf (stream, template, x);
		}
		break;

	    case 'o':
	    case 'u':
	    case 'x':
	    case 'X':
		if (!strcmp (modifier, "h")) {
		    unsigned short x = va_arg (ap, unsigned short);
		    n = fprintf (stream, template, x);
		} else if (!*modifier) {
		    unsigned int x = va_arg (ap, unsigned int);
		    n = fprintf (stream, template, x);
		} else if (!strcmp (modifier, "l")) {
		    unsigned long x = va_arg (ap, unsigned long);
		    n = fprintf (stream, template, x);
		} else {
		    unsigned long long x = va_arg (ap, unsigned long long);
		    n = fprintf (stream, template, x);
		}
		break;

	    case 'f':
	    case 'e':
	    case 'E':
	    case 'g':
	    case 'G':
		if (!strcmp (modifier, "h")) {
		    float x = va_arg (ap, float);
		    n = fprintf (stream, template, x);
		} else if (!*modifier || !strcmp (modifier, "l")) {
		    double x = va_arg (ap, double);
		    n = fprintf (stream, template, x);
		} else {
		    /*
		     * Some compilers complain when `long double' and
		     * `double' are the same thing.
		     */
#if SIZEOF_LONG_DOUBLE != SIZEOF_DOUBLE
		    long double x = va_arg (ap, long double);
		    n = fprintf (stream, template, x);
#else
		    double x = va_arg (ap, double);
		    n = fprintf (stream, template, x);
#endif
		}
		break;

	    case 'a':
		if (1) {
		    haddr_t *x = va_arg (ap, haddr_t*);
		    H5F_addr_print (stream, x);
		    n = 0;
		}
		break;

	    case 'c':
		if (1) {
		    char x = va_arg (ap, char);
		    n = fprintf (stream, template, x);
		}
		break;

	    case 's':
	    case 'p':
		if (1) {
		    char *x = va_arg (ap, char*);
		    n = fprintf (stream, template, x);
		}
		break;

	    case 'n':
		if (1) {
		    template[strlen(template)-1] = 'u';
		    n = fprintf (stream, template, nout);
		}
		break;

	    default:
		fputs (template, stream);
		n = (int)strlen (template);
		break;
	    }
	    nout += n;
	    fmt = s;
	} else {
	    putc (*fmt, stream);
	    fmt++;
	    nout++;
	}
    }
    va_end (ap);
    return nout;
}


/*-------------------------------------------------------------------------
 * Function:	HDstrtoll
 *
 * Purpose:	Converts the string S to an int64 value according to the
 *		given BASE, which must be between 2 and 36 inclusive, or be
 *		the special value zero.
 *
 *		The string must begin with an arbitrary amount of white space
 *		(as determined by isspace(3c)) followed by a single optional
 *              `+' or `-' sign.  If BASE is zero or 16 the string may then
 *		include a `0x' or `0X' prefix, and the number will be read in
 *		base 16; otherwise a zero BASE is taken as 10 (decimal)
 *		unless the next character is a `0', in which case it is taken
 *		as 8 (octal).
 *
 *		The remainder of the string is converted to an int64 in the
 *		obvious manner, stopping at the first character which is not
 *		a valid digit in the given base.  (In bases above 10, the
 *		letter `A' in either upper or lower case represetns 10, `B'
 *		represents 11, and so forth, with `Z' representing 35.)
 *
 *		If REST is not null, the address of the first invalid
 *		character in S is stored in *REST.  If there were no digits
 *		at all, the original value of S is stored in *REST.  Thus, if
 *		*S is not `\0' but **REST is `\0' on return the entire string
 *		was valid.
 *
 * Return:	Success:	The result.
 *
 *		Failure:	If the input string does not contain any
 *				digits then zero is returned and REST points
 *				to the original value of S.  If an overflow
 *				or underflow occurs then the maximum or
 *				minimum possible value is returned and the
 *				global `errno' is set to ERANGE.  If BASE is
 *				incorrect then zero is returned.
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int64
HDstrtoll (const char *s, const char **rest, int base)
{
    int64	sign=1, acc=0;
    hbool_t	overflow = FALSE;
    
    errno = 0;
    if (!s || (base && (base<2 || base>36))) {
	if (rest) *rest = s;
	return 0;
    }
    
    /* Skip white space */
    while (isspace (*s)) s++;

    /* Optional minus or plus sign */
    if ('+'==*s) {
	s++;
    } else if ('-'==*s) {
	sign = -1;
	s++; 
   }

    /* Zero base prefix */
    if (0==base && '0'==*s && ('x'==s[1] || 'X'==s[1])) {
	base = 16;
	s += 2;
    } else if (0==base && '0'==*s) {
	base = 8;
	s++;
    } else if (0==base) {
	base = 10;
    }
    
    /* Digits */
    while ((base<=10 && *s>='0' && *s<'0'+base) ||
	   (base>10 && ((*s>='0' && *s<='9') ||
			(*s>='a' && *s<'a'+base-10) ||
			(*s>='A' && *s<'A'+base-10)))) {
	if (!overflow) {
	    int64 digit = 0;
	    if (*s>='0' && *s<='9') digit = *s - '0';
	    else if (*s>='a' && *s<='z') digit = *s-'a'+10;
	    else digit = *s-'A'+10;

	    if (acc*base+digit < acc) {
		overflow = TRUE;
	    } else {
		acc = acc*base + digit;
	    }
	}
	s++;
    }

    /* Overflow */
    if (overflow) {
	if (sign>0) {
	    acc = ((uint64)1<<(8*sizeof(int64)-1))-1;
	} else {
	    acc = (uint64)1<<(8*sizeof(int64)-1);
	}
	errno = ERANGE;
    }

    /* Return values */
    acc *= sign;
    if (rest) *rest = s;
    return acc;
}


/*-------------------------------------------------------------------------
 * Function:	H5_timer_reset
 *
 * Purpose:	Resets the timer struct to zero.  Use this to reset a timer
 *		that's being used as an accumulator for summing times.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5_timer_reset (H5_timer_t *timer)
{
    assert (timer);
    HDmemset (timer, 0, sizeof *timer);
}


/*-------------------------------------------------------------------------
 * Function:	H5_timer_begin
 *
 * Purpose:	Initialize a timer to time something.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5_timer_begin (H5_timer_t *timer)
{
#ifdef HAVE_GETRUSAGE
    struct rusage	rusage;
#endif
    struct timeval	etime;

    assert (timer);

#ifdef HAVE_GETRUSAGE
    getrusage (RUSAGE_SELF, &rusage);
    timer->utime = (double)rusage.ru_utime.tv_sec +
                   (double)rusage.ru_utime.tv_usec/1e6;
    timer->stime = (double)rusage.ru_stime.tv_sec +
                   (double)rusage.ru_stime.tv_usec/1e6;
#else
    timer->utime = 0.0;
    timer->stime = 0.0;
#endif

    gettimeofday (&etime, NULL);
    timer->etime = (double)etime.tv_sec + (double)etime.tv_usec/1e6;
}



/*-------------------------------------------------------------------------
 * Function:	H5_timer_end
 *
 * Purpose:	This function should be called at the end of a timed region.
 *		The SUM is an optional pointer which will accumulate times.
 *		TMS is the same struct that was passed to H5_timer_start().
 *		On return, TMS will contain total times for the timed region.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5_timer_end (H5_timer_t *sum/*in,out*/, H5_timer_t *timer/*in,out*/)
{
    H5_timer_t		now;
    
    assert (timer);
    H5_timer_begin (&now);

    timer->utime = MAX(0.0, now.utime - timer->utime);
    timer->stime = MAX(0.0, now.stime - timer->stime);
    timer->etime = MAX(0.0, now.etime - timer->etime);

    if (sum) {
	sum->utime += timer->utime;
	sum->stime += timer->stime;
	sum->etime += timer->etime;
    }
}


/*-------------------------------------------------------------------------
 * Function:	H5_bandwidth
 *
 * Purpose:	Prints the bandwidth (bytes per second) in a field 10
 *		characters wide widh four digits of precision like this:
 *
 * 			       NaN	If <=0 seconds
 *			1234. TB/s
 * 			123.4 TB/s
 *			12.34 GB/s
 *			1.234 MB/s
 *			4.000 kB/s
 *			1.000  B/s
 *			0.000  B/s	If NBYTES==0
 *			1.2345e-10	For bandwidth less than 1
 *			6.7893e+94	For exceptionally large values
 *			6.678e+106	For really big values
 *			
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5_bandwidth(char *buf/*out*/, double nbytes, double nseconds)
{
    double	bw;

    if (nseconds<=0.0) {
	strcpy(buf, "       NaN");
    } else {
	bw = nbytes/nseconds;
	if (bw==0.0) {
	    strcpy(buf, "0.000  B/s");
	} else if (bw<1.0) {
	    sprintf(buf, "%10.4e", bw);
	} else if (bw<1024.0) {
	    sprintf(buf, "%05.4f", bw);
	    strcpy(buf+5, "  B/s");
	} else if (bw<1024.0*1024.0) {
	    sprintf(buf, "%05.4f", bw/1024.0);
	    strcpy(buf+5, " kB/s");
	} else if (bw<1024.0*1024.0*1024.0) {
	    sprintf(buf, "%05.4f", bw/(1024.0*1024.0));
	    strcpy(buf+5, " MB/s");
	} else if (bw<1024.0*1024.0*1024.0*1024.0) {
	    sprintf(buf, "%05.4f",
		    bw/(1024.0*1024.0*1024.0));
	    strcpy(buf+5, " GB/s");
	} else if (bw<1024.0*1024.0*1024.0*1024.0*1024.0) {
	    sprintf(buf, "%05.4f",
		    bw/(1024.0*1024.0*1024.0*1024.0));
	    strcpy(buf+5, " TB/s");
	} else {
	    sprintf(buf, "%10.4e", bw);
	    if (strlen(buf)>10) {
		sprintf(buf, "%10.3e", bw);
	    }
	}
    }
}


/*-------------------------------------------------------------------------
 * Function:	H5_trace
 *
 * Purpose:	This function is called whenever an API function is called
 *		and tracing is turned on.  If RETURNING is non-zero then
 *		the caller is about to return.  Otherwise we print the
 *		function name and the arguments.
 *
 *		The TYPE argument is a string which gives the type of each of
 *		the following argument pairs.  Each type is zero or more
 *		asterisks (one for each level of indirection, although some
 *		types have one level of indirection already implied) followed
 *		by either one letter (lower case) or two letters (first one
 *		uppercase).
 *
 *		The variable argument list consists of pairs of values. Each
 *		pair is a string which is the formal argument name in the
 *		calling function, followed by the argument value.  The type
 *		of the argument value is given by the TYPE string.
 *
 * Note:	The TYPE string is meant to be terse and is generated by a
 *		separate perl script.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5_trace (hbool_t returning, const char *func, const char *type, ...)
{
    va_list		ap;
    char		buf[64], *rest;
    const char		*argname;
    intn		argno=0, ptr, n, asize_idx;
    hssize_t		asize[16];
    hssize_t		i;
    void		*vp = NULL;
    FILE		*out = H5_trace_g;

    if (!out) return;	/*tracing is off*/
    va_start (ap, type);

    if (returning) {
	fprintf (out, " = ");
    } else {
	fprintf (out, "%s(", func);
    }

    /* Clear array sizes */
    for (i=0; i<NELMTS(asize); i++) asize[i] = -1;

    /* Parse the argument types */
    for (argno=0; *type; argno++, type+=isupper(*type)?2:1) {
	/* Count levels of indirection */
	for (ptr=0; '*'==*type; type++) ptr++;
	if ('['==*type) {
	    if ('a'==type[1]) {
		asize_idx = (int)strtol(type+2, &rest, 10);
		assert(']'==*rest);
		type = rest+1;
	    } else {
		rest = strchr(type, ']');
		assert(rest);
		type = rest+1;
		asize_idx = -1;
	    }
	} else {
	    asize_idx = -1;
	}
	
	/*
	 * The argument name.  Leave off the `_id' part.  If the argument
	 * name is the null pointer then don't print the argument or the
	 * following `='.  This is used for return values.
	 */
	argname = va_arg (ap, char*);
	if (argname) {
	    n = MAX (0, (int)strlen(argname)-3);
	    if (!strcmp (argname+n, "_id")) {
		strncpy (buf, argname, MIN ((int)sizeof(buf)-1, n));
		buf[MIN((int)sizeof(buf)-1, n)] = '\0';
		argname = buf;
	    }
	    fprintf (out, "%s%s=", argno?", ":"", argname);
	} else {
	    argname = "";
	}

	/* The value */
	if (ptr) vp = va_arg (ap, void*);
	switch (type[0]) {
	case 'b':
	    if (ptr) {
		if (vp) {
		    fprintf (out, "0x%lx", (unsigned long)vp);
		} else {
		    fprintf(out, "NULL");
		}
	    } else {
		hbool_t bool = va_arg (ap, hbool_t);
		if (TRUE==bool) fprintf (out, "TRUE");
		else if (!bool) fprintf (out, "FALSE");
		else fprintf (out, "TRUE(%u)", (unsigned)bool);
	    }
	    break;

	case 'd':
	    if (ptr) {
		if (vp) {
		    fprintf (out, "0x%lx", (unsigned long)vp);
		} else {
		    fprintf(out, "NULL");
		}
	    } else {
		double dbl = va_arg (ap, double);
		fprintf (out, "%g", dbl);
	    }
	    break;

	case 'D':
	    switch (type[1]) {
	    case 'l':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5D_layout_t layout = va_arg (ap, H5D_layout_t);
		    switch (layout) {
		    case H5D_LAYOUT_ERROR:
			fprintf (out, "H5D_LAYOUT_ERROR");
			break;
		    case H5D_COMPACT:
			fprintf (out, "H5D_COMPACT");
			break;
		    case H5D_CONTIGUOUS:
			fprintf (out, "H5D_CONTIGUOUS");
			break;
		    case H5D_CHUNKED:
			fprintf (out, "H5D_CHUNKED");
			break;
		    default:
			fprintf (out, "%ld", (long)layout);
			break;
		    }
		}
		break;
		
	    case 't':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5D_transfer_t transfer = va_arg (ap, H5D_transfer_t);
		    switch (transfer) {
		    case H5D_XFER_INDEPENDENT:
			fprintf (out, "H5D_XFER_INDEPENDENT");
			break;
		    case H5D_XFER_COLLECTIVE:
			fprintf (out, "H5D_XFER_COLLECTIVE");
			break;
		    case H5D_XFER_DFLT:
			fprintf (out, "H5D_XFER_DFLT");
			break;
		    default:
			fprintf (out, "%ld", (long)transfer);
			break;
		    }
		}
		break;
		
	    default:
		fprintf (out, "BADTYPE(D%c)", type[1]);
		goto error;
	    }
	    break;

	case 'e':
	    if (ptr) {
		if (vp) {
		    fprintf (out, "0x%lx", (unsigned long)vp);
		} else {
		    fprintf(out, "NULL");
		}
	    } else {
		herr_t status = va_arg (ap, herr_t);
		if (SUCCEED==status) fprintf (out, "SUCCEED");
		else if (FAIL==status) fprintf (out, "FAIL");
		else fprintf (out, "%d", (int)status);
	    }
	    break;
	    
	case 'E':
	    switch (type[1]) {
	    case 'd':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5E_direction_t direction = va_arg (ap, H5E_direction_t);
		    switch (direction) {
		    case H5E_WALK_UPWARD:
			fprintf (out, "H5E_WALK_UPWARD");
			break;
		    case H5E_WALK_DOWNWARD:
			fprintf (out, "H5E_WALK_DOWNWARD");
			break;
		    default:
			fprintf (out, "%ld", (long)direction);
			break;
		    }
		}
		break;
		
	    case 'e':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5E_error_t *error = va_arg (ap, H5E_error_t*);
		    fprintf (out, "0x%lx", (unsigned long)error);
		}
		break;
		
	    default:
		fprintf (out, "BADTYPE(E%c)", type[1]);
		goto error;
	    }
	    break;

	case 'F':
	    switch (type[1]) {
	    case 'd':
		if (ptr) {
		    if (vp) {
			fprintf(out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5F_driver_t driver = va_arg(ap, H5F_driver_t);
		    switch (driver) {
		    case H5F_LOW_ERROR:
			fprintf(out, "H5F_LOW_ERROR");
			break;
		    case H5F_LOW_STDIO:
			fprintf(out, "H5F_LOW_STDIO");
			break;
		    case H5F_LOW_SEC2:
			fprintf(out, "H5F_LOW_SEC2");
			break;
		    case H5F_LOW_MPIO:
			fprintf(out, "H5F_LOW_MPIO");
			break;
		    case H5F_LOW_CORE:
			fprintf(out, "H5F_LOW_CORE");
			break;
		    case H5F_LOW_SPLIT:
			fprintf(out, "H5F_LOW_SPLIT");
			break;
		    case H5F_LOW_FAMILY:
			fprintf(out, "H5F_LOW_FAMILY");
			break;
		    default:
			fprintf(out, "%ld", (long)driver);
			break;
		    }
		}
		break;

	    default:
		fprintf(out, "BADTYPE(F%c)", type[1]);
		goto error;
	    }
	    break;

	case 'G':
	    switch (type[1]) {
	    case 'l':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5G_link_t link_type = va_arg (ap, H5G_link_t);
		    switch (link_type) {
		    case H5G_LINK_ERROR:
			fprintf (out, "H5G_LINK_ERROR");
			break;
		    case H5G_LINK_HARD:
			fprintf (out, "H5G_LINK_HARD");
			break;
		    case H5G_LINK_SOFT:
			fprintf (out, "H5G_LINK_SOFT");
			break;
		    default:
			fprintf (out, "%ld", (long)link_type);
			break;
		    }
		}
		break;

	    case 's':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5G_stat_t *statbuf = va_arg (ap, H5G_stat_t*);
		    fprintf (out, "0x%lx", (unsigned long)statbuf);
		}
		break;

	    default:
		fprintf (out, "BADTYPE(G%c)", type[1]);
		goto error;
	    }
	    break;

	case 'h':
	    if (ptr) {
		if (vp) {
		    fprintf (out, "0x%lx", (unsigned long)vp);
		    if (asize_idx>=0 && asize[asize_idx]>=0) {
			hsize_t *p = (hsize_t*)vp;
			fprintf(out, " {");
			for (i=0; i<asize[asize_idx]; i++) {
			    if (H5S_UNLIMITED==p[i]) {
				HDfprintf(out, "%sH5S_UNLIMITED", i?", ":"");
			    } else {
				HDfprintf(out, "%s%Hu", i?", ":"", p[i]);
			    }
			}
			fprintf(out, "}");
		    }
		} else {
		    fprintf(out, "NULL");
		}
	    } else {
		hsize_t hsize = va_arg (ap, hsize_t);
		if (H5S_UNLIMITED==hsize) {
		    HDfprintf(out, "H5S_UNLIMITED");
		} else {
		    HDfprintf (out, "%Hu", hsize);
		    asize[argno] = (hssize_t)hsize;
		}
	    }
	    break;

	case 'H':
	    switch (type[1]) {
	    case 's':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
			if (asize_idx>=0 && asize[asize_idx]>=0) {
			    hssize_t *p = (hssize_t*)vp;
			    fprintf(out, " {");
			    for (i=0; i<asize[asize_idx]; i++) {
				HDfprintf(out, "%s%Hd", i?", ":"", p[i]);
			    }
			    fprintf(out, "}");
			}
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    hssize_t hssize = va_arg (ap, hssize_t);
		    HDfprintf (out, "%Hd", hssize);
		    asize[argno] = (hssize_t)hssize;
		}
		break;
		
	    default:
		fprintf (out, "BADTYPE(H%c)", type[1]);
		goto error;
	    }
	    break;

	case 'i':
	    if (ptr) {
		if (vp) {
		    fprintf (out, "0x%lx", (unsigned long)vp);
		} else {
		    fprintf(out, "NULL");
		}
	    } else {
		hid_t obj = va_arg (ap, hid_t);
		if (-2 == obj) {
		    fprintf (out, "H5P_DEFAULT");
		} else if (FAIL==obj) {
		    fprintf (out, "FAIL");
		} else {
		    switch (H5I_group (obj)) {
		    case BADGROUP:
			fprintf (out, "%ld (error)", (long)obj);
			break;
		    case H5_FILE:
			fprintf(out, "%ld", (long)obj);
			if (strcmp (argname, "file")) {
			    fprintf (out, " (file)");
			}
			break;
		    case H5_TEMPLATE_0:
		    case H5_TEMPLATE_1:
		    case H5_TEMPLATE_2:
		    case H5_TEMPLATE_3:
		    case H5_TEMPLATE_4:
		    case H5_TEMPLATE_5:
		    case H5_TEMPLATE_6:
		    case H5_TEMPLATE_7:
			fprintf(out, "%ld", (long)obj);
			if (strcmp (argname, "plist")) {
			    fprintf (out, " (plist)");
			}
			break;
		    case H5_GROUP:
			fprintf(out, "%ld", (long)obj);
			if (strcmp (argname, "group")) {
			    fprintf (out, " (group)");
			}
			break;
		    case H5_DATATYPE:
			if (obj==H5T_NATIVE_CHAR_g) {
			    fprintf(out, "H5T_NATIVE_CHAR");
			} else if (obj==H5T_NATIVE_UCHAR_g) {
			    fprintf(out, "H5T_NATIVE_UCHAR");
			} else if (obj==H5T_NATIVE_SHORT_g) {
			    fprintf(out, "H5T_NATIVE_SHORT");
			} else if (obj==H5T_NATIVE_USHORT_g) {
			    fprintf(out, "H5T_NATIVE_USHORT");
			} else if (obj==H5T_NATIVE_INT_g) {
			    fprintf(out, "H5T_NATIVE_INT");
			} else if (obj==H5T_NATIVE_UINT_g) {
			    fprintf(out, "H5T_NATIVE_UINT");
			} else if (obj==H5T_NATIVE_LONG_g) {
			    fprintf(out, "H5T_NATIVE_LONG");
			} else if (obj==H5T_NATIVE_ULONG_g) {
			    fprintf(out, "H5T_NATIVE_ULONG");
			} else if (obj==H5T_NATIVE_LLONG_g) {
			    fprintf(out, "H5T_NATIVE_LLONG");
			} else if (obj==H5T_NATIVE_ULLONG_g) {
			    fprintf(out, "H5T_NATIVE_ULLONG");
			} else if (obj==H5T_NATIVE_FLOAT_g) {
			    fprintf(out, "H5T_NATIVE_FLOAT");
			} else if (obj==H5T_NATIVE_DOUBLE_g) {
			    fprintf(out, "H5T_NATIVE_DOUBLE");
			} else if (obj==H5T_NATIVE_LDOUBLE_g) {
			    fprintf(out, "H5T_NATIVE_LDOUBLE");
			} else if (obj==H5T_IEEE_F32BE_g) {
			    fprintf(out, "H5T_IEEE_F32BE");
			} else if (obj==H5T_IEEE_F32LE_g) {
			    fprintf(out, "H5T_IEEE_F32LE");
			} else if (obj==H5T_IEEE_F64BE_g) {
			    fprintf(out, "H5T_IEEE_F64BE");
			} else if (obj==H5T_IEEE_F64LE_g) {
			    fprintf(out, "H5T_IEEE_F64LE");
			} else if (obj==H5T_STD_I8BE_g) {
			    fprintf(out, "H5T_STD_I8BE");
			} else if (obj==H5T_STD_I8LE_g) {
			    fprintf(out, "H5T_STD_I8LE");
			} else if (obj==H5T_STD_I16BE_g) {
			    fprintf(out, "H5T_STD_I16BE");
			} else if (obj==H5T_STD_I16LE_g) {
			    fprintf(out, "H5T_STD_I16LE");
			} else if (obj==H5T_STD_I32BE_g) {
			    fprintf(out, "H5T_STD_I32BE");
			} else if (obj==H5T_STD_I32LE_g) {
			    fprintf(out, "H5T_STD_I32LE");
			} else if (obj==H5T_STD_I64BE_g) {
			    fprintf(out, "H5T_STD_I64BE");
			} else if (obj==H5T_STD_I64LE_g) {
			    fprintf(out, "H5T_STD_I64LE");
			} else if (obj==H5T_STD_U8BE_g) {
			    fprintf(out, "H5T_STD_U8BE");
			} else if (obj==H5T_STD_U8LE_g) {
			    fprintf(out, "H5T_STD_U8LE");
			} else if (obj==H5T_STD_U16BE_g) {
			    fprintf(out, "H5T_STD_U16BE");
			} else if (obj==H5T_STD_U16LE_g) {
			    fprintf(out, "H5T_STD_U16LE");
			} else if (obj==H5T_STD_U32BE_g) {
			    fprintf(out, "H5T_STD_U32BE");
			} else if (obj==H5T_STD_U32LE_g) {
			    fprintf(out, "H5T_STD_U32LE");
			} else if (obj==H5T_STD_U64BE_g) {
			    fprintf(out, "H5T_STD_U64BE");
			} else if (obj==H5T_STD_U64LE_g) {
			    fprintf(out, "H5T_STD_U64LE");
			} else if (obj==H5T_STD_B8BE_g) {
			    fprintf(out, "H5T_STD_B8BE");
			} else if (obj==H5T_STD_B8LE_g) {
			    fprintf(out, "H5T_STD_B8LE");
			} else if (obj==H5T_STD_B16BE_g) {
			    fprintf(out, "H5T_STD_B16BE");
			} else if (obj==H5T_STD_B16LE_g) {
			    fprintf(out, "H5T_STD_B16LE");
			} else if (obj==H5T_STD_B32BE_g) {
			    fprintf(out, "H5T_STD_B32BE");
			} else if (obj==H5T_STD_B32LE_g) {
			    fprintf(out, "H5T_STD_B32LE");
			} else if (obj==H5T_STD_B64BE_g) {
			    fprintf(out, "H5T_STD_B64BE");
			} else if (obj==H5T_STD_B64LE_g) {
			    fprintf(out, "H5T_STD_B64LE");
			} else if (obj==H5T_C_S1_g) {
			    fprintf(out, "H5T_C_S1");
			} else if (obj==H5T_FORTRAN_S1_g) {
			    fprintf(out, "H5T_FORTRAN_S1");
			} else {
			    fprintf(out, "%ld", (long)obj);
			    if (strcmp (argname, "type")) {
				fprintf (out, " (type)");
			    }
			}
			break;
		    case H5_DATASPACE:
			fprintf(out, "%ld", (long)obj);
			if (strcmp (argname, "space")) {
			    fprintf (out, " (space)");
			}
			/*Save the rank of simple data spaces for arrays*/
			{
			    H5S_t *space = H5I_object(obj);
			    if (H5S_SIMPLE==space->extent.type) {
				asize[argno] = space->extent.u.simple.rank;
			    }
			}
			break;
		    case H5_DATASET:
			fprintf(out, "%ld", (long)obj);
			if (strcmp (argname, "dset")) {
			    fprintf (out, " (dset)");
			}
			break;
		    case H5_ATTR:
			fprintf(out, "%ld", (long)obj);
			if (strcmp (argname, "attr")) {
			    fprintf (out, " (attr)");
			}
			break;
		    case H5_TEMPBUF:
			fprintf(out, "%ld", (long)obj);
			if (strcmp(argname, "tbuf")) {
			    fprintf(out, " (tbuf");
			}
			break;
		    default:
			fprintf(out, "%ld", (long)obj);
			fprintf (out, " (unknown class)");
			break;
		    }
		}
	    }
	    break;

	case 'I':
	    switch (type[1]) {
	    case 's':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
			if (asize_idx>=0 && asize[asize_idx]>=0) {
			    int *p = (int*)vp;
			    fprintf(out, " {");
			    for (i=0; i<asize[asize_idx]; i++) {
				fprintf(out, "%s%d", i?", ":"", p[i]);
			    }
			    fprintf(out, "}");
			}
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    int is = va_arg (ap, int);
		    fprintf (out, "%d", is);
		    asize[argno] = is;
		}
		break;
		
	    case 'u':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
			if (asize_idx>=0 && asize[asize_idx]>=0) {
			    int *p = (int*)vp;
			    fprintf(out, " {");
			    for (i=0; i<asize[asize_idx]; i++) {
				HDfprintf(out, "%s%Hu", i?", ":"", p[i]);
			    }
			    fprintf(out, "}");
			}
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    unsigned iu = va_arg (ap, unsigned);
		    fprintf (out, "%u", iu);
		    asize[argno] = iu;
		}
		break;

	    default:
		fprintf (out, "BADTYPE(I%c)", type[1]);
		goto error;
	    }
	    break;

	case 'M':
	    switch (type[1]) {
	    case 'c':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
#ifdef HAVE_PARALLEL
		    MPI_Comm comm = va_arg (ap, MPI_Comm);
		    fprintf (out, "%ld", (long)comm);
#endif
		}
		break;
	    case 'i':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
#ifdef HAVE_PARALLEL
		    MPI_Info info = va_arg (ap, MPI_Info);
		    fprintf (out, "%ld", (long)info);
#endif
		}
		break;
	    default:
		goto error;
	    }
	    break;

	case 'o':
	    if (ptr) {
		if (vp) {
		    fprintf (out, "0x%lx", (unsigned long)vp);
		} else {
		    fprintf(out, "NULL");
		}
	    } else {
		off_t offset = va_arg (ap, off_t);
		fprintf (out, "%ld", (long)offset);
	    }
	    break;

	case 'p':
	    if (ptr) {
		if (vp) {
		    fprintf (out, "0x%lx", (unsigned long)vp);
		} else {
		    fprintf(out, "NULL");
		}
	    } else {
		H5P_class_t plist_class = va_arg (ap, H5P_class_t);
		switch (plist_class) {
		case H5P_NO_CLASS:
		    fprintf (out, "H5P_NO_CLASS");
		    break;
		case H5P_FILE_CREATE:
		    fprintf (out, "H5P_FILE_CREATE");
		    break;
		case H5P_FILE_ACCESS:
		    fprintf (out, "H5P_FILE_ACCESS");
		    break;
		case H5P_DATASET_CREATE:
		    fprintf (out, "H5P_DATASET_CREATE");
		    break;
		case H5P_DATASET_XFER:
		    fprintf (out, "H5P_DATASET_XFER");
		    break;
		default:
		    fprintf (out, "%ld", (long)plist_class);
		    break;
		}
	    }
	    break;

	case 'S':
	    switch (type[1]) {
	    case 'c':
		if (ptr) {
		    if (vp) {
			fprintf(out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5S_class_t cls = va_arg(ap, H5S_class_t);
		    switch (cls) {
		    case H5S_NO_CLASS:
			fprintf(out, "H5S_NO_CLASS");
			break;
		    case H5S_SCALAR:
			fprintf(out, "H5S_SCALAR");
			break;
		    case H5S_SIMPLE:
			fprintf(out, "H5S_SIMPLE");
			break;
		    case H5S_COMPLEX:
			fprintf(out, "H5S_COMPLEX");
			break;
		    default:
			fprintf(out, "%ld", (long)cls);
			break;
		    }
		}
		break;
			
	    case 's':
		if (ptr) {
		    if (vp) {
			fprintf(out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5S_seloper_t so = va_arg(ap, H5S_seloper_t);
		    switch (so) {
		    case H5S_NOOP:
			fprintf(out, "H5S_NOOP");
			break;
		    case H5S_SELECT_SET:
			fprintf(out, "H5S_SELECT_SET");
			break;
		    default:
			fprintf(out, "%ld", (long)so);
			break;
		    }
		}
		break;

	    default:
		fprintf(out, "BADTYPE(S%c)", type[1]);
		goto error;
	    }
	    break;

	case 's':
	    if (ptr) {
		if (vp) {
		    fprintf (out, "0x%lx", (unsigned long)vp);
		} else {
		    fprintf(out, "NULL");
		}
	    } else {
		const char *str = va_arg (ap, const char*);
		fprintf (out, "\"%s\"", str);
	    }
	    break;

	case 'T':
	    switch (type[1]) {
	    case 'c':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5T_cset_t cset = va_arg (ap, H5T_cset_t);
		    switch (cset) {
		    case H5T_CSET_ERROR:
			fprintf (out, "H5T_CSET_ERROR");
			break;
		    case H5T_CSET_ASCII:
			fprintf (out, "H5T_CSET_ASCII");
			break;
		    default:
			fprintf (out, "%ld", (long)cset);
			break;
		    }
		}
		break;

	    case 'n':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5T_norm_t norm = va_arg (ap, H5T_norm_t);
		    switch (norm) {
		    case H5T_NORM_ERROR:
			fprintf (out, "H5T_NORM_ERROR");
			break;
		    case H5T_NORM_IMPLIED:
			fprintf (out, "H5T_NORM_IMPLIED");
			break;
		    case H5T_NORM_MSBSET:
			fprintf (out, "H5T_NORM_MSBSET");
			break;
		    case H5T_NORM_NONE:
			fprintf (out, "H5T_NORM_NONE");
			break;
		    default:
			fprintf (out, "%ld", (long)norm);
			break;
		    }
		}
		break;

	    case 'o':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5T_order_t order = va_arg (ap, H5T_order_t);
		    switch (order) {
		    case H5T_ORDER_ERROR:
			fprintf (out, "H5T_ORDER_ERROR");
			break;
		    case H5T_ORDER_LE:
			fprintf (out, "H5T_ORDER_LE");
			break;
		    case H5T_ORDER_BE:
			fprintf (out, "H5T_ORDER_BE");
			break;
		    case H5T_ORDER_VAX:
			fprintf (out, "H5T_ORDER_VAX");
			break;
		    case H5T_ORDER_NONE:
			fprintf (out, "H5T_ORDER_NONE");
			break;
		    default:
			fprintf (out, "%ld", (long)order);
			break;
		    }
		}
		break;

	    case 'p':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5T_pad_t pad = va_arg (ap, H5T_pad_t);
		    switch (pad) {
		    case H5T_PAD_ERROR:
			fprintf (out, "H5T_PAD_ERROR");
			break;
		    case H5T_PAD_ZERO:
			fprintf (out, "H5T_PAD_ZERO");
			break;
		    case H5T_PAD_ONE:
			fprintf (out, "H5T_PAD_ONE");
			break;
		    case H5T_PAD_BACKGROUND:
			fprintf (out, "H5T_PAD_BACKGROUND");
			break;
		    default:
			fprintf (out, "%ld", (long)pad);
			break;
		    }
		}
		break;

	    case 's':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5T_sign_t sign = va_arg (ap, H5T_sign_t);
		    switch (sign) {
		    case H5T_SGN_ERROR:
			fprintf (out, "H5T_SGN_ERROR");
			break;
		    case H5T_SGN_NONE:
			fprintf (out, "H5T_SGN_NONE");
			break;
		    case H5T_SGN_2:
			fprintf (out, "H5T_SGN_2");
			break;
		    default:
			fprintf (out, "%ld", (long)sign);
			break;
		    }
		}
		break;

	    case 't':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5T_class_t type_class = va_arg (ap, H5T_class_t);
		    switch (type_class) {
		    case H5T_NO_CLASS:
			fprintf (out, "H5T_NO_CLASS");
			break;
		    case H5T_INTEGER:
			fprintf (out, "H5T_INTEGER");
			break;
		    case H5T_FLOAT:
			fprintf (out, "H5T_FLOAT");
			break;
		    case H5T_TIME:
			fprintf (out, "H5T_TIME");
			break;
		    case H5T_STRING:
			fprintf (out, "H5T_STRING");
			break;
		    case H5T_BITFIELD:
			fprintf (out, "H5T_BITFIELD");
			break;
		    case H5T_OPAQUE:
			fprintf (out, "H5T_OPAQUE");
			break;
		    case H5T_COMPOUND:
			fprintf (out, "H5T_COMPOUND");
			break;
		    default:
			fprintf (out, "%ld", (long)type_class);
			break;
		    }
		}
		break;

	    case 'z':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5T_str_t str = va_arg (ap, H5T_str_t);
		    switch (str) {
		    case H5T_STR_ERROR:
			fprintf (out, "H5T_STR_ERROR");
			break;
		    case H5T_STR_NULL:
			fprintf (out, "H5T_STR_NULL");
			break;
		    case H5T_STR_SPACE:
			fprintf (out, "H5T_STR_SPACE");
			break;
		    default:
			fprintf (out, "%ld", (long)str);
			break;
		    }
		}
		break;

	    default:
		fprintf (out, "BADTYPE(T%c)", type[1]);
		goto error;
	    }
	    break;

	case 'x':
	    if (ptr) {
		if (vp) {
		    fprintf (out, "0x%lx", (unsigned long)vp);
		} else {
		    fprintf(out, "NULL");
		}
	    } else {
		vp = va_arg (ap, void*);
		fprintf (out, "0x%lx", (unsigned long)vp);
	    }
	    break;

	case 'z':
	    if (ptr) {
		if (vp) {
		    fprintf (out, "0x%lx", (unsigned long)vp);
		    if (asize_idx>=0 && asize[asize_idx]>=0) {
			size_t *p = (size_t*)vp;
			fprintf(out, " {");
			for (i=0; i<asize[asize_idx]; i++) {
			    HDfprintf(out, "%s%Zu", i?", ":"", p[i]);
			}
			fprintf(out, "}");
		    }
		} else {
		    fprintf(out, "NULL");
		}
	    } else {
		size_t size = va_arg (ap, size_t);
		HDfprintf (out, "%Zu", size);
		asize[argno] = (hssize_t)size;
	    }
	    break;

	case 'Z':
	    switch (type[1]) {
	    case 'f':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    H5Z_filter_t id = va_arg (ap, H5Z_filter_t);
		    if (H5Z_FILTER_DEFLATE==id) {
			fprintf (out, "H5Z_FILTER_DEFLATE");
		    } else {
			fprintf (out, "%ld", (long)id);
		    }
		}
		break;

	    case 's':
		if (ptr) {
		    if (vp) {
			fprintf (out, "0x%lx", (unsigned long)vp);
			if (vp && asize_idx>=0 && asize[asize_idx]>=0) {
			    ssize_t *p = (ssize_t*)vp;
			    fprintf(out, " {");
			    for (i=0; i<asize[asize_idx]; i++) {
				HDfprintf(out, "%s%Zd", i?", ":"", p[i]);
			    }
			    fprintf(out, "}");
			}
		    } else {
			fprintf(out, "NULL");
		    }
		} else {
		    ssize_t ssize = va_arg (ap, ssize_t);
		    HDfprintf (out, "%Zd", ssize);
		    asize[argno] = (hssize_t)ssize;
		}
		break;

	    default:
		fprintf (out, "BADTYPE(Z%c)", type[1]);
		goto error;
	    }
	    break;

	default:
	    if (isupper (type[0])) {
		fprintf (out, "BADTYPE(%c%c)", type[0], type[1]);
	    } else {
		fprintf (out, "BADTYPE(%c)", type[0]);
	    }
	    goto error;
	}
    }

 error:
    va_end (ap);
    if (returning) {
	fprintf (out, ";\n");
    } else {
	fprintf (out, ")");
    }
    fflush (out);
    return;
}

    
