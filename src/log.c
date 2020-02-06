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
#include <err.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <sys/param.h>
#include <sys/cdefs.h>

#include "hlog/hlog.h"

#ifndef lint
__RCSID("$Id: log.c 4702 2007-06-22 07:01:28Z dyoung $");
#endif

static enum loglib_mode loglib_mode = LOGLIB_M_STDERR;
TAILQ_HEAD(, loglib_sink) loglib_sinks = TAILQ_HEAD_INITIALIZER(loglib_sinks);

LOGLIB_SINK_TOP_DEFN(all);

LOGLIB_SINK_SHORT_DEFN(arithmetic, all);
LOGLIB_SINK_SHORT_DEFN(mem, all);

LOGLIB_SINK_MEDIUM_DEFN(sys, all, LOG_NOTICE);
LOGLIB_SINK_MEDIUM_DEFN(notice, all, LOG_NOTICE);
LOGLIB_SINK_MEDIUM_DEFN(err, all, LOG_ERR);
LOGLIB_SINK_MEDIUM_DEFN(warn, all, LOG_WARNING);

int
loglib_start(enum loglib_mode mode, int facility)
{
	loglib_mode = mode;

	if (mode == LOGLIB_M_STDERR)
		return 0;
	else if (mode != LOGLIB_M_SYSLOG)
		return -1;

	openlog(getprogname(), 0, facility);
	(void)setlogmask(LOG_UPTO(LOG_DEBUG));
	return 0;
}

static void
loglib_print_time(void)
{
	time_t t;
	struct tm *tm;
	char buf[80];

	t = time(NULL);
	tm = gmtime(&t);
	if (strftime(buf, sizeof(buf),"%Y%m%d%H%M%S ", tm) == 0) {
		warnx("%s: strftime failed", __func__);
		return;
	}
	(void)fprintf(stderr, "%s", buf);
}

void
loglib_vlog(int priority, const char *fmt, va_list ap)
{
	if (loglib_mode == LOGLIB_M_SYSLOG) {
		vsyslog(priority, fmt, ap);
		return;
	} else {
		loglib_print_time();
		(void)vfprintf(stderr, fmt, ap);
		(void)fputc('\n', stderr);
	}
}

static char *
message_extend_stderr(const char *fmt0)
{
	static const char sep[] = ": ";
	const char *m;
	char *fmt;
	size_t fmtlen;

	m = strerror(errno);

	fmtlen = strlen(fmt0) + strlen(m) + sizeof(sep);

	if ((fmt = malloc(fmtlen)) == NULL) {
		syslog(LOG_ERR, "%s: malloc failed: %s", __func__,
		    strerror(errno));
		return NULL;
	}

	/* Note well the safe strcpy, strcat usage.  Thank you. */
	strcpy(fmt, fmt0);
	strcat(fmt, sep);
	strcat(fmt, m);

	return fmt;
}

static char *
message_extend_syslogd(const char *fmt0)
{
	static const char minterp[] = ": %m";
	char *fmt;
	size_t fmtlen;

	fmtlen = sizeof(minterp) + strlen(fmt0);

	if ((fmt = malloc(fmtlen)) == NULL) {
		syslog(LOG_ERR, "%s: malloc failed: %s", __func__,
		    strerror(errno));
		return NULL;
	}

	/* Note well the safe strcpy, strcat usage.  Thank you. */
	strcpy(fmt, fmt0);
	strcat(fmt, minterp);

	return fmt;
}

static char *
message_extend(const char *fmt0)
{
	switch (loglib_mode) {
	case LOGLIB_M_SYSLOG:
		return message_extend_syslogd(fmt0);
	default:
		return message_extend_stderr(fmt0);
	}
}

void
loglib_verr(int status, const char *fmt0, va_list ap)
{
	char *fmt;

	if ((fmt = message_extend(fmt0)) == NULL)
		exit(status);
	loglib_vlog(LOG_ERR, fmt, ap);
	free(fmt);

	exit(status);
}

void
loglib_verrx(int status, const char *fmt, va_list ap)
{
	loglib_vlog(LOG_ERR, fmt, ap);
	exit(status);
}

void
loglib_vwarn(const char *fmt0, va_list ap)
{
	char *fmt;

	if ((fmt = message_extend(fmt0)) == NULL)
		return;
	loglib_vlog(LOG_WARNING, fmt, ap);
	free(fmt);
}

void
loglib_vwarnx(const char *fmt, va_list ap)
{
	loglib_vlog(LOG_WARNING, fmt, ap);
}

void
loglib_err(int status, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	loglib_verr(status, fmt, ap);
	va_end(ap);
}

void
loglib_errx(int status, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	loglib_verrx(status, fmt, ap);
	va_end(ap);
}

void
loglib_warn(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	loglib_vwarn(fmt, ap);
	va_end(ap);
}

void
loglib_warnx(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	loglib_vwarnx(fmt, ap);
	va_end(ap);
}

struct loglib_sink *
loglib_sink_find_active(struct loglib_sink *ls0)
{
	struct loglib_sink *ls;

	LOGLIB_SINK_FOREACH(ls, ls0) {
		switch (ls->ls_state) {
		case LOGLIB_SINK_S_PASS:
			continue;
		case LOGLIB_SINK_S_OFF:
			return NULL;
		case LOGLIB_SINK_S_ON:
		default:
			return ls;
		}
	}
	return NULL;
}

void
loglib_always_log(struct loglib_sink *ls, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	loglib_vlog(ls->ls_priority, fmt, ap);
	va_end(ap);
}

void
loglib_log(struct loglib_sink *ls0, const char *fmt, ...)
{
	struct loglib_sink *ls;
	va_list ap;

	if ((ls = loglib_sink_find_active(ls0)) == NULL)
		return;

	va_start(ap, fmt);
	loglib_vlog(ls->ls_priority, fmt, ap);
	va_end(ap);
}

struct loglib_sink *
loglib_sink_lookup(const char *name)
{
	struct loglib_sink *ls;
	TAILQ_FOREACH(ls, &loglib_sinks, ls_next) {
		if (strcmp(ls->ls_name, name) == 0)
			return ls;
	}
	return NULL;
}

static struct loglib_sink *
loglib_sink_create(const char *name)
{
	struct loglib_sink *ls;
	static struct nmalloc_pool *np;

	if (np == NULL && (np = nmalloc_pool_create(__func__)) == NULL)
		return NULL;

	if ((ls = nzmalloc(np, sizeof(*ls))) == NULL)
		return NULL;
	else if ((ls->ls_name0 = strdup(name)) == NULL) {
		nfree(ls);
		return NULL;
	}
	ls->ls_name = ls->ls_name0;
	ls->ls_rendezvous = 1;
	return ls;
}

static void
loglib_sink_destroy(struct loglib_sink *ls)
{
	/*LINTED*/
	if (ls->ls_name0 != NULL)
		free(ls->ls_name0);
	nfree(ls);
}

int
loglib_set_state(const char *name, enum loglib_sink_state state, int rendezvous)
{
	struct loglib_sink *ls;
	errno = 0;

	switch (state) {
	case LOGLIB_SINK_S_PASS:
	case LOGLIB_SINK_S_OFF:
	case LOGLIB_SINK_S_ON:
		break;
	default:
		errno = EINVAL;
		return -1;
	}
	if ((ls = loglib_sink_lookup(name)) == NULL && !rendezvous) {
		errno = ESRCH;
		return -1;
	} else if (ls == NULL) {
		if ((ls = loglib_sink_create(name)) == NULL)
			return -1;
		TAILQ_INSERT_TAIL(&loglib_sinks, ls, ls_next);
	}
	ls->ls_state = state;
	return 0;
}

void
loglib_sink_register(struct loglib_sink *ls_arg)
{
	struct loglib_sink *ls;
	if ((ls = loglib_sink_lookup(ls_arg->ls_name)) == NULL ||
	    ls->ls_rendezvous) {
		TAILQ_INSERT_TAIL(&loglib_sinks, ls_arg, ls_next);
		if (ls == NULL)
			return;
		warnx("%s: rendezvous with log-sink '%s'", __func__,
		    ls->ls_name);
		ls_arg->ls_state = ls->ls_state;
		TAILQ_REMOVE(&loglib_sinks, ls, ls_next);
		loglib_sink_destroy(ls);
	} else
		warnx("%s: duplicate log-sink, '%s'", __func__, ls->ls_name);
}

/*ARGSUSED*/
int
log_printer(void *arg __unused, const char *fmt, ...)
{
	va_list ap;
	struct loglib_sink *ls;
	if ((ls = loglib_sink_find_active(&log_notice)) == NULL) {
		errno = EAGAIN;
		return -1;
	}

	va_start(ap, fmt);
	loglib_vlog(ls->ls_priority, fmt, ap);
	va_end(ap);
	return 0;
}
