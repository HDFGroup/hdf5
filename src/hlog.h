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
#ifndef	_LOGLIB_H
#define	_LOGLIB_H

#include <stdarg.h>
#include <syslog.h>
#include <sys/cdefs.h>
#include <sys/queue.h>

#include "misc/misc.h"

enum loglib_mode {LOGLIB_M_SYSLOG = 0, LOGLIB_M_STDERR};

enum loglib_sink_state	{
	  LOGLIB_SINK_S_PASS = 0
	, LOGLIB_SINK_S_OFF = 1
	, LOGLIB_SINK_S_ON = 2
};

struct loglib_sink {
	const char			*ls_name;
	char				*ls_name0;
	struct loglib_sink		*ls_parent;
	enum loglib_sink_state		ls_state;	
	int				ls_priority;
	int				ls_rendezvous;
	TAILQ_ENTRY(loglib_sink)	ls_next;
};

#define	LOGLIB_CONSTRUCTOR(__sym)					\
void loglib_constructor_##__sym(void) __attribute__((constructor));	\
void									\
loglib_constructor_##__sym(void)					\
{									\
	loglib_sink_register(&__sym);					\
}									\
void loglib_constructor_##__sym(void) __attribute__((constructor))

#define	LOGLIB_SINK_FOREACH(__le, __le0)				\
	for ((__le) = (__le0); (__le) != NULL; (__le) = (__le)->ls_parent)

#define	LOGLIB_SINK_DECL1(__sym)	extern struct loglib_sink __sym

#define	LOGLIB_SINK_DECL(__name)	LOGLIB_SINK_DECL1(log_##__name)

#define	LOGLIB_SINK_DEFN(__sym, __name, __parent, __state, __priority)	\
	struct loglib_sink __sym = {					\
		  .ls_name = __name					\
		, .ls_parent = (__parent)				\
		, .ls_state = (__state)					\
		, .ls_priority = (__priority)				\
	};								\
	LOGLIB_CONSTRUCTOR(__sym)

#define	LOGLIB_SINK_MEDIUM_DEFN(__name, __parent, __priority)		\
    LOGLIB_SINK_DEFN(log_##__name, #__name, &log_##__parent,		\
        LOGLIB_SINK_S_PASS, (__priority))

#define	LOGLIB_SINK_SHORT_DEFN(__name, __parent)			\
    LOGLIB_SINK_MEDIUM_DEFN(__name, __parent, LOG_INFO)

#define	LOGLIB_SINK_TOP_DEFN(__name)					\
    LOGLIB_SINK_DEFN(log_##__name, #__name, NULL, LOGLIB_SINK_S_ON,	\
        LOG_INFO)

#define	LOGLIB_F_ALL	(0xffffffff)

LOGLIB_SINK_DECL(all);
LOGLIB_SINK_DECL(arithmetic);
LOGLIB_SINK_DECL(mem);
LOGLIB_SINK_DECL(sys);
LOGLIB_SINK_DECL(notice);
LOGLIB_SINK_DECL(err);
LOGLIB_SINK_DECL(warn);

#define	LOGLIB_LOG	loglib_log

#define	LOGLIB_LAZY(__ls0, ...)						\
	do {								\
		struct loglib_sink *__ls;				\
		if ((__ls = loglib_sink_find_active((__ls0))) != NULL)	\
			loglib_always_log((__ls), __VA_ARGS__);		\
	} while (/*CONSTCOND*/0)

struct loglib_sink *loglib_sink_find_active(struct loglib_sink *);
extern void loglib_sink_register(struct loglib_sink *);
extern struct loglib_sink *loglib_sink_lookup(const char *);
extern int loglib_set_state(const char *, enum loglib_sink_state, int);
extern int loglib_start(enum loglib_mode, int);

void loglib_vlog(int, const char *, va_list)
    __attribute__((__format__(__printf__,2,0)));

void loglib_vwarn(const char *, va_list)
    __attribute__((__format__(__printf__,1,0)));
void loglib_vwarnx(const char *, va_list)
    __attribute__((__format__(__printf__,1,0)));
void loglib_verr(int, const char *, va_list)
    __attribute__((__format__(__printf__,2,0))) __attribute__((__noreturn__));
void loglib_verrx(int, const char *, va_list)
    __attribute__((__format__(__printf__,2,0))) __attribute__((__noreturn__));

void loglib_warnx(const char *, ...)
    __attribute__((__format__(__printf__,1,2)));
void loglib_warn(const char *, ...)
    __attribute__((__format__(__printf__,1,2)));

void loglib_err(int, const char *, ...)
    __attribute__((__format__(__printf__,2,3))) __attribute__((__noreturn__));
void loglib_errx(int, const char *, ...)
    __attribute__((__format__(__printf__,2,3))) __attribute__((__noreturn__));

void loglib_always_log(struct loglib_sink *, const char *, ...)
    __attribute__((__format__(__printf__,2,3)));

void loglib_log(struct loglib_sink *, const char *, ...)
    __attribute__((__format__(__printf__,2,3)));

int log_printer(void *, const char *, ...);

#endif	/* _LOGLIB_H */
