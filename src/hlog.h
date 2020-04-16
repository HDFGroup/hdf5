/*
 * Copyright (c) 2004, 2005, 2006, 2007 David Young.  All rights reserved.
 *
 * See COPYING at the top of the HDF5 distribution for license terms.
 */
/*
 * Copyright (c) 2004 Urbana-Champaign Independent Media Center.
 * All rights reserved.
 *
 * See COPYING at the top of the HDF5 distribution for license terms.
 */
#ifndef	_HLOG_H
#define	_HLOG_H

#include <stdarg.h>
#include <stdbool.h>
#include <syslog.h>
#include <sys/cdefs.h>

#include "bsdqueue.h"

#ifndef _unused
#define _unused __attribute__((unused))
#endif

#ifndef _constructor
#define _constructor __attribute__((constructor))
#endif

#ifndef _noreturn
#define _noreturn __attribute__((__noreturn__))
#endif

#ifndef _printflike
#define _printflike(_fmt, _args) \
    __attribute__((__format__(__printf__,_fmt,_args)))
#endif

enum hlog_outlet_state {
	  HLOG_OUTLET_S_PASS = 0
	, HLOG_OUTLET_S_OFF = 1
	, HLOG_OUTLET_S_ON = 2
};

typedef enum hlog_outlet_state hlog_outlet_state_t;

struct hlog_outlet {
        hlog_outlet_state_t             ls_resolved;
	struct hlog_outlet		*ls_parent;
	hlog_outlet_state_t		ls_state;
	const char			*ls_name;
	char				*ls_name0;
	bool				ls_rendezvous;
	TAILQ_ENTRY(hlog_outlet)	ls_next;
};

typedef struct hlog_outlet hlog_outlet_t;

#define	HLOG_CONSTRUCTOR(__sym)					        \
void hlog_constructor_##__sym(void) _constructor;	                \
void									\
hlog_constructor_##__sym(void)					        \
{									\
	hlog_outlet_register(&__sym);					\
}									\
void hlog_undefined_##__sym(void) _constructor

#define	HLOG_OUTLET_FOREACH(__le, __le0)				\
	for ((__le) = (__le0); (__le) != NULL; (__le) = (__le)->ls_parent)

#define	HLOG_OUTLET_DECL1(__sym)	extern struct hlog_outlet __sym

#define HLOG_JOIN_SYMS(x, y)    x ## y

#define HLOG_PREFIX(_sfx) HLOG_JOIN_SYMS(hlog_gbl_, _sfx)

#define	HLOG_OUTLET_DECL(__name)	HLOG_OUTLET_DECL1(HLOG_PREFIX(__name))

#define	HLOG_OUTLET_DEFN(__sym, __name, __parent, __state)	        \
	struct hlog_outlet __sym = {					\
		  .ls_name = __name					\
		, .ls_parent = (__parent)				\
		, .ls_state = (__state)					\
	};								\
	HLOG_CONSTRUCTOR(__sym)

#define	HLOG_OUTLET_MEDIUM_DEFN(__name, __parent, __state)	        \
    HLOG_OUTLET_DEFN(HLOG_PREFIX(__name), #__name, &HLOG_PREFIX(__parent),		\
        __state)

#define	HLOG_OUTLET_SHORT_DEFN(__name, __parent)			\
    HLOG_OUTLET_MEDIUM_DEFN(__name, __parent, HLOG_OUTLET_S_PASS)

#define	HLOG_OUTLET_TOP_DEFN(__name)					\
    HLOG_OUTLET_DEFN(HLOG_PREFIX(__name), #__name, NULL, HLOG_OUTLET_S_PASS)

HLOG_OUTLET_DECL(all);

#define hlog(_name, _fmt, ...)    \
    hlog_impl(&HLOG_PREFIX(_name), _fmt, __VA_ARGS__)

#define	hlog_fast(_name, ...)						    \
    do {								    \
            hlog_outlet_t *_ls0 = &HLOG_PREFIX(_name);		            \
                                                                            \
            if (_ls0->ls_resolved == HLOG_OUTLET_S_OFF)                     \
                break;                                                      \
            else if (_ls0->ls_resolved == HLOG_OUTLET_S_ON)                 \
                hlog_always(_ls0, __VA_ARGS__);                             \
            else                                                            \
                hlog_impl(_ls0, __VA_ARGS__);                               \
    } while (/*CONSTCOND*/0)

struct hlog_outlet *hlog_outlet_find_active(struct hlog_outlet *);
void hlog_outlet_register(struct hlog_outlet *);
struct hlog_outlet *hlog_outlet_lookup(const char *);
int hlog_set_state(const char *, hlog_outlet_state_t, bool);

void vhlog(const char *, va_list) _printflike(1,0);

void vhlog_warn(const char *, va_list) _printflike(1,0);
void vhlog_warnx(const char *, va_list) _printflike(1,0);
void vhlog_err(int, const char *, va_list) _printflike(2,0) _noreturn;
void vhlog_errx(int, const char *, va_list) _printflike(2,0) _noreturn;

void hlog_warnx(const char *, ...) _printflike(1,2);
void hlog_warn(const char *, ...) _printflike(1,2);

void hlog_err(int, const char *, ...) _printflike(2,3) _noreturn;
void hlog_errx(int, const char *, ...) _printflike(2,3) _noreturn;

void hlog_always(struct hlog_outlet *, const char *, ...)
    _printflike(2,3);

void hlog_impl(struct hlog_outlet *, const char *, ...)
    _printflike(2,3);

#endif	/* _HLOG_H */
