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
#include <sys/time.h>
#include <sys/resource.h>

/* private headers */
#include <H5private.h>          /*library                 */
#include <H5ACprivate.h>        /*cache                           */
#include <H5Bprivate.h>         /*B-link trees                    */
#include <H5Eprivate.h>         /*error handling          */
#include <H5MMprivate.h>        /*memory management               */
#include <H5Tprivate.h>         /*data types                      */

#define PABLO_MASK      H5_mask

/*--------------------- Locally scoped variables -----------------------------*/

hbool_t                 library_initialize_g = FALSE;
hbool_t                 thread_initialize_g = FALSE;
hbool_t                 install_atexit_g = TRUE;

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
H5_add_exit(void        (*func) (void))
{
    herr_t                  ret_value = SUCCEED;
    H5_exit_t              *new_exit;

    FUNC_ENTER_INIT(H5_add_exit, NULL, FAIL);

    assert(func);

    new_exit = H5MM_xcalloc(1, sizeof(H5_exit_t));

    new_exit->func = func;
    new_exit->next = lib_exit_head;
    lib_exit_head = new_exit;

    FUNC_LEAVE(ret_value);
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
 *		minor revision number, RELNUM the release revision number,
 *		and PATNUM the patch revision number.
 *
 * Note:	When printing an HDF5 version number it should be printed as
 *		`printf ("HDF5-%d.%d.%d%c", maj, min, rel, 'a'+patch)'.
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
H5version(unsigned *majnum, unsigned *minnum, unsigned *relnum,
	  unsigned *patnum)
{
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5version, FAIL);

    /* Set the version information */
    if (majnum) *majnum = H5_VERS_MAJOR;
    if (minnum) *minnum = H5_VERS_MINOR;
    if (relnum) *relnum = H5_VERS_RELEASE;
    if (patnum) *patnum = H5_VERS_PATCH;

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
H5vers_check (unsigned majnum, unsigned minnum, unsigned relnum,
	      unsigned patnum)
{
    /* Don't initialize the library quite yet */
    
    if (H5_VERS_MAJOR!=majnum || H5_VERS_MINOR!=minnum ||
	H5_VERS_RELEASE!=relnum || H5_VERS_PATCH!=patnum) {
	fputs ("Warning! The HDF5 header files included by this application "
	       "do not match the\nversion used by the HDF5 library to which "
	       "this application is linked. Data\ncorruption or segmentation "
	       "faults would be likely if the application were\nallowed to "
	       "continue.\n", stderr);
	fprintf (stderr, "Headers are %u.%u.%u%c, library is %u.%u.%u%c\n",
		 majnum, minnum, relnum, 'a'+patnum,
		 H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE,
		 'a'+H5_VERS_PATCH);
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
	    if (strchr ("Hhlq", *s)) {
		switch (*s) {
		case 'H':
		    if (sizeof(hsize_t)==sizeof(long)) {
			strcpy (modifier, "l");
		    } else if (sizeof(hsize_t)==sizeof(long long)) {
			strcpy (modifier, PRINTF_LL_WIDTH);
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
	    if (modifier) {
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
		    long double x = va_arg (ap, long double);
		    n = fprintf (stream, template, x);
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
	    int64 digit;
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
    timer->etime = etime.tv_sec + etime.tv_usec/1e6;
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
    
