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
#include <stdbool.h>
#include <stdint.h> /* for uintmax_t */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <sys/param.h>
#include <sys/cdefs.h>

#include "hlog.h"
#include "H5time_private.h"

TAILQ_HEAD(, hlog_outlet) hlog_outlets = TAILQ_HEAD_INITIALIZER(hlog_outlets);

HLOG_OUTLET_TOP_DEFN(all);

static struct timespec timestamp_zero;

void hlog_init(void) _constructor;
static void hlog_init_timestamps(void);

void
hlog_init(void)
{
    const char *settings0;
    char *item, *settings;

    if ((settings0 = getenv("HLOG")) == NULL)
        return;

    if ((settings = strdup(settings0)) == NULL) {
        warn("%s: cannot duplicate settings string", __func__);
        return;
    }

    while ((item = strsep(&settings, " ,")) != NULL) {
        hlog_outlet_state_t state;
        char key[64 + 1], val[4 + 1];   // + 1 for the terminating NUL
        int nconverted;

        nconverted = sscanf(item, " %64[0-9a-z_] = %4s ", key, val);
        if (nconverted != 2) {
            warnx("%s: malformed HLOG item \"%s\"", __func__, item);
            continue;
        }

        if (strcmp(val, "on") == 0 || strcmp(val, "yes") == 0)
            state = HLOG_OUTLET_S_ON;
        else if (strcmp(val, "off") == 0 || strcmp(val, "no") == 0)
            state = HLOG_OUTLET_S_OFF;
        else if (strcmp(val, "pass") == 0)
            state = HLOG_OUTLET_S_PASS;
        else {
            warnx("%s: bad HLOG value \"%s\" in item \"%s\"", __func__,
                val, item);
            continue;
        }

        if (hlog_set_state(key, state, true) == -1) {
            warn("%s: could not set state for HLOG item \"%s\"", __func__,
                item);
        }
    }

    free(settings);
}


static void
hlog_init_timestamps(void)
{
    static bool initialized = false;

    if (initialized)
        return;

    if (clock_gettime(CLOCK_MONOTONIC, &timestamp_zero) == -1)
        err(EXIT_FAILURE, "%s: clock_gettime", __func__);

    initialized = true;
}

static void
hlog_print_time(void)
{
    struct timespec elapsed, now;

    hlog_init_timestamps();

    if (clock_gettime(CLOCK_MONOTONIC, &now) == -1)
        err(EXIT_FAILURE, "%s: clock_gettime", __func__);

    timespecsub(&now, &timestamp_zero, &elapsed);

    fprintf(stderr, "%ju.%.9ld ", (uintmax_t)elapsed.tv_sec, elapsed.tv_nsec);
}

void
vhlog(const char *fmt, va_list ap)
{
        hlog_print_time();
        (void)vfprintf(stderr, fmt, ap);
        (void)fputc('\n', stderr);
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
		err(EXIT_FAILURE, "%s: malloc failed", __func__);
		return NULL;
	}

	/* Note well the safe strcpy, strcat usage.  Thank you. */
	strcpy(fmt, fmt0);
	strcat(fmt, sep);
	strcat(fmt, m);

	return fmt;
}

static char *
message_extend(const char *fmt0)
{
        return message_extend_stderr(fmt0);
}

void
vhlog_err(int status, const char *fmt0, va_list ap)
{
	char *fmt;

	if ((fmt = message_extend(fmt0)) == NULL)
		exit(status);
	vhlog(fmt, ap);
	free(fmt);

	exit(status);
}

void
vhlog_errx(int status, const char *fmt, va_list ap)
{
	vhlog(fmt, ap);
	exit(status);
}

void
vhlog_warn(const char *fmt0, va_list ap)
{
	char *fmt;

	if ((fmt = message_extend(fmt0)) == NULL)
		return;
	vhlog(fmt, ap);
	free(fmt);
}

void
vhlog_warnx(const char *fmt, va_list ap)
{
	vhlog(fmt, ap);
}

void
hlog_err(int status, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vhlog_err(status, fmt, ap);
	va_end(ap);
}

void
hlog_errx(int status, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vhlog_errx(status, fmt, ap);
	va_end(ap);
}

void
hlog_warn(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vhlog_warn(fmt, ap);
	va_end(ap);
}

void
hlog_warnx(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vhlog_warnx(fmt, ap);
	va_end(ap);
}

struct hlog_outlet *
hlog_outlet_find_active(struct hlog_outlet *ls0)
{
	struct hlog_outlet *ls;

	HLOG_OUTLET_FOREACH(ls, ls0) {
		switch (ls->ls_state) {
		case HLOG_OUTLET_S_PASS:
			continue;
		case HLOG_OUTLET_S_OFF:
			return NULL;
		case HLOG_OUTLET_S_ON:
		default:
			return ls;
		}
	}
	return NULL;
}

void
hlog_always(struct hlog_outlet *ls _unused, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vhlog(fmt, ap);
	va_end(ap);
}

void
hlog_impl(struct hlog_outlet *ls0, const char *fmt, ...)
{
	struct hlog_outlet *ls;
	va_list ap;

	if ((ls = hlog_outlet_find_active(ls0)) == NULL) {
            ls0->ls_resolved = HLOG_OUTLET_S_OFF;
            return;
        }

        ls0->ls_resolved = HLOG_OUTLET_S_ON;

	va_start(ap, fmt);
	vhlog(fmt, ap);
	va_end(ap);
}

static void
hlog_outlet_reset_all(void)
{
	struct hlog_outlet *ls;

	TAILQ_FOREACH(ls, &hlog_outlets, ls_next)
		ls->ls_resolved = HLOG_OUTLET_S_PASS;
}

struct hlog_outlet *
hlog_outlet_lookup(const char *name)
{
	struct hlog_outlet *ls;

	TAILQ_FOREACH(ls, &hlog_outlets, ls_next) {
		if (strcmp(ls->ls_name, name) == 0)
			return ls;
	}
	return NULL;
}

static struct hlog_outlet *
hlog_outlet_create(const char *name)
{
	struct hlog_outlet *ls;

	if ((ls = calloc(1, sizeof(*ls))) == NULL)
		return NULL;
	else if ((ls->ls_name0 = strdup(name)) == NULL) {
		free(ls);
		return NULL;
	}
	ls->ls_name = ls->ls_name0;
	ls->ls_rendezvous = true;
	return ls;
}

static void
hlog_outlet_destroy(struct hlog_outlet *ls)
{
	/*LINTED*/
	if (ls->ls_name0 != NULL)
		free(ls->ls_name0);
	free(ls);
}

int
hlog_set_state(const char *name, hlog_outlet_state_t state, bool rendezvous)
{
	struct hlog_outlet *ls;
	errno = 0;

	switch (state) {
	case HLOG_OUTLET_S_PASS:
	case HLOG_OUTLET_S_OFF:
	case HLOG_OUTLET_S_ON:
		break;
	default:
		errno = EINVAL;
		return -1;
	}
	if ((ls = hlog_outlet_lookup(name)) == NULL && !rendezvous) {
		errno = ESRCH;
		return -1;
	} else if (ls == NULL) {
		if ((ls = hlog_outlet_create(name)) == NULL)
			return -1;
		TAILQ_INSERT_TAIL(&hlog_outlets, ls, ls_next);
	}
	ls->ls_state = state;
        hlog_outlet_reset_all();
	return 0;
}

void
hlog_outlet_register(struct hlog_outlet *ls_arg)
{
	struct hlog_outlet *ls;
	if ((ls = hlog_outlet_lookup(ls_arg->ls_name)) == NULL ||
	    ls->ls_rendezvous) {
		TAILQ_INSERT_TAIL(&hlog_outlets, ls_arg, ls_next);
		if (ls == NULL)
			return;
		warnx("%s: rendezvous with log-outlet '%s'", __func__,
		    ls->ls_name);
		ls_arg->ls_state = ls->ls_state;
		TAILQ_REMOVE(&hlog_outlets, ls, ls_next);
		hlog_outlet_destroy(ls);
	} else
		warnx("%s: duplicate log-outlet, '%s'", __func__, ls->ls_name);
}
