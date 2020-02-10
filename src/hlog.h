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
#include <syslog.h>
#include <sys/cdefs.h>

#include "bsdqueue.h"

#ifndef __unused
#define __unused __attribute__((unused))
#endif

#ifndef __constructor
#define __constructor __attribute__((constructor))
#endif

#ifndef __noreturn
#define __noreturn __attribute__((__noreturn__))
#endif

#ifndef __printflike
#define __printflike(_fmt, _args) \
    __attribute__((__format__(__printf__,_fmt,_args)))
#endif

enum hlog_sink_state {
	  HLOG_SINK_S_PASS = 0
	, HLOG_SINK_S_OFF = 1
	, HLOG_SINK_S_ON = 2
};

typedef enum hlog_sink_state hlog_sink_state_t;

struct hlog_sink {
	const char			*ls_name;
	char				*ls_name0;
	struct hlog_sink		*ls_parent;
	enum hlog_sink_state		ls_state;	
	bool				ls_rendezvous;
	TAILQ_ENTRY(hlog_sink)	        ls_next;
};

#define	HLOG_CONSTRUCTOR(__sym)					        \
void hlog_constructor_##__sym(void) __constructor;	                \
void									\
hlog_constructor_##__sym(void)					        \
{									\
	hlog_sink_register(&__sym);					\
}									\
void hlog_undefined_##__sym(void) __constructor

#define	HLOG_SINK_FOREACH(__le, __le0)				        \
	for ((__le) = (__le0); (__le) != NULL; (__le) = (__le)->ls_parent)

#define	HLOG_SINK_DECL1(__sym)	extern struct hlog_sink __sym

#define	HLOG_SINK_DECL(__name)	HLOG_SINK_DECL1(log_##__name)

#define	HLOG_SINK_DEFN(__sym, __name, __parent, __state)	        \
	struct hlog_sink __sym = {					\
		  .ls_name = __name					\
		, .ls_parent = (__parent)				\
		, .ls_state = (__state)					\
	};								\
	HLOG_CONSTRUCTOR(__sym)

#define	HLOG_SINK_MEDIUM_DEFN(__name, __parent)		                \
    HLOG_SINK_DEFN(log_##__name, #__name, &log_##__parent,		\
        HLOG_SINK_S_PASS)

#define	HLOG_SINK_SHORT_DEFN(__name, __parent)			        \
    HLOG_SINK_MEDIUM_DEFN(__name, __parent)

#define	HLOG_SINK_TOP_DEFN(__name)					\
    HLOG_SINK_DEFN(log_##__name, #__name, NULL, HLOG_SINK_S_OFF)

HLOG_SINK_DECL(all);
HLOG_SINK_DECL(arithmetic);
HLOG_SINK_DECL(mem);
HLOG_SINK_DECL(sys);
HLOG_SINK_DECL(notice);
HLOG_SINK_DECL(err);
HLOG_SINK_DECL(warn);

#define hlog(_name, _fmt, ...)    \
    hlog_impl(&log_##_name, _fmt, __VA_ARGS__)

#define	hlog_fast(_name, ...)						    \
    do {								    \
            struct hlog_sink *_ls;				            \
            if ((_ls = hlog_sink_find_active((&log_##_name))) != NULL)      \
                    hlog_always((_ls), __VA_ARGS__);		            \
    } while (/*CONSTCOND*/0)

struct hlog_sink *hlog_sink_find_active(struct hlog_sink *);
void hlog_sink_register(struct hlog_sink *);
struct hlog_sink *hlog_sink_lookup(const char *);
int hlog_set_state(const char *, enum hlog_sink_state, bool);

void vhlog(const char *, va_list) __printflike(1,0);

void vhlog_warn(const char *, va_list) __printflike(1,0);
void vhlog_warnx(const char *, va_list) __printflike(1,0);
void vhlog_err(int, const char *, va_list) __printflike(2,0) __noreturn;
void vhlog_errx(int, const char *, va_list) __printflike(2,0) __noreturn;

void hlog_warnx(const char *, ...) __printflike(1,2);
void hlog_warn(const char *, ...) __printflike(1,2);

void hlog_err(int, const char *, ...) __printflike(2,3) __noreturn;
void hlog_errx(int, const char *, ...) __printflike(2,3) __noreturn;

void hlog_always(struct hlog_sink *, const char *, ...)
    __printflike(2,3);

void hlog_impl(struct hlog_sink *, const char *, ...)
    __printflike(2,3);

#endif	/* _HLOG_H */
