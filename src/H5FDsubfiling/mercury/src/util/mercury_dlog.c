/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include "mercury_dlog.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <process.h>
#else
#include <unistd.h>
#endif

/****************/
/* Local Macros */
/****************/

/************************************/
/* Local Type and Struct Definition */
/************************************/

/********************/
/* Local Prototypes */
/********************/

/*******************/
/* Local Variables */
/*******************/

/*---------------------------------------------------------------------------*/
struct hg_dlog *
hg_dlog_alloc(char *name, unsigned int lesize, int leloop)
{
    struct hg_dlog_entry *le;
    struct hg_dlog       *d;

    le = malloc(sizeof(*le) * lesize);
    if (!le)
        return NULL;

    d = malloc(sizeof(*d));
    if (!d) {
        free(le);
        return NULL;
    }

    memset(d, 0, sizeof(*d));
    snprintf(d->dlog_magic, sizeof(d->dlog_magic), "%s%s", HG_DLOG_STDMAGIC, name);
    hg_thread_mutex_init(&d->dlock);
    HG_LIST_INIT(&d->cnts32);
    HG_LIST_INIT(&d->cnts64);
    d->le      = le;
    d->lesize  = lesize;
    d->leloop  = leloop;
    d->mallocd = 1;

    return d;
}

/*---------------------------------------------------------------------------*/
void
hg_dlog_free(struct hg_dlog *d)
{
    struct hg_dlog_dcount32 *cp32 = HG_LIST_FIRST(&d->cnts32);
    struct hg_dlog_dcount64 *cp64 = HG_LIST_FIRST(&d->cnts64);

    while (cp32) {
        struct hg_dlog_dcount32 *cp = cp32;
        cp32                        = HG_LIST_NEXT(cp, l);
        free(cp);
    }
    HG_LIST_INIT(&d->cnts32);

    while (cp64) {
        struct hg_dlog_dcount64 *cp = cp64;
        cp64                        = HG_LIST_NEXT(cp, l);
        free(cp);
    }
    HG_LIST_INIT(&d->cnts64);

    if (d->mallocd) {
        free(d->le);
        free(d);
    }
}

/*---------------------------------------------------------------------------*/
void
hg_dlog_mkcount32(struct hg_dlog *d, hg_atomic_int32_t **cptr, const char *name, const char *descr)
{
    struct hg_dlog_dcount32 *dcnt;

    hg_thread_mutex_lock(&d->dlock);
    if (*cptr == NULL) {
        dcnt = malloc(sizeof(*dcnt));
        if (!dcnt) {
            fprintf(stderr, "hd_dlog_mkcount: malloc of %s failed!", name);
            abort();
        }
        dcnt->name  = name;
        dcnt->descr = descr;
        hg_atomic_init32(&dcnt->c, 0);
        HG_LIST_INSERT_HEAD(&d->cnts32, dcnt, l);
        *cptr = &dcnt->c; /* set it in caller's variable */
    }
    hg_thread_mutex_unlock(&d->dlock);
}

/*---------------------------------------------------------------------------*/
void
hg_dlog_mkcount64(struct hg_dlog *d, hg_atomic_int64_t **cptr, const char *name, const char *descr)
{
    struct hg_dlog_dcount64 *dcnt;

    hg_thread_mutex_lock(&d->dlock);
    if (*cptr == NULL) {
        dcnt = malloc(sizeof(*dcnt));
        if (!dcnt) {
            fprintf(stderr, "hd_dlog_mkcount: malloc of %s failed!", name);
            abort();
        }
        dcnt->name  = name;
        dcnt->descr = descr;
        hg_atomic_init64(&dcnt->c, 0);
        HG_LIST_INSERT_HEAD(&d->cnts64, dcnt, l);
        *cptr = &dcnt->c; /* set it in caller's variable */
    }
    hg_thread_mutex_unlock(&d->dlock);
}

/*---------------------------------------------------------------------------*/
void
hg_dlog_setlogstop(struct hg_dlog *d, int stop)
{
    d->lestop = stop; /* no need to lock */
}

/*---------------------------------------------------------------------------*/
void
hg_dlog_resetlog(struct hg_dlog *d)
{
    hg_thread_mutex_lock(&d->dlock);
    d->lefree = 0;
    d->leadds = 0;
    hg_thread_mutex_unlock(&d->dlock);
}

/*---------------------------------------------------------------------------*/
void
hg_dlog_dump(struct hg_dlog *d, int (*log_func)(FILE *, const char *, ...), FILE *stream, int trylock)
{
    unsigned int             left, idx;
    struct hg_dlog_dcount32 *dc32;
    struct hg_dlog_dcount64 *dc64;

    if (trylock) {
        int try_ret = hg_thread_mutex_try_lock(&d->dlock);
        if (try_ret != HG_UTIL_SUCCESS) /* warn them, but keep going */ {
            fprintf(stderr, "hg_dlog_dump: WARN - lock failed\n");
            return;
        }
    }
    else
        hg_thread_mutex_lock(&d->dlock);

    if (d->leadds > 0) {
        log_func(stream,
                 "### ----------------------\n"
                 "### (%s) debug log summary\n"
                 "### ----------------------\n",
                 (d->dlog_magic + strlen(HG_DLOG_STDMAGIC)));
        if (!HG_LIST_IS_EMPTY(&d->cnts32) && !HG_LIST_IS_EMPTY(&d->cnts64)) {
            log_func(stream, "# Counters\n");
            HG_LIST_FOREACH(dc32, &d->cnts32, l)
            {
                log_func(stream, "# %s: %" PRId32 " [%s]\n", dc32->name, hg_atomic_get32(&dc32->c),
                         dc32->descr);
            }
            HG_LIST_FOREACH(dc64, &d->cnts64, l)
            {
                log_func(stream, "# %s: %" PRId64 " [%s]\n", dc64->name, hg_atomic_get64(&dc64->c),
                         dc64->descr);
            }
            log_func(stream, "# -\n");
        }

        log_func(stream, "# Number of log entries: %d\n", d->leadds);

        idx  = (d->lefree < d->leadds) ? d->lesize + d->lefree - d->leadds : d->lefree - d->leadds;
        left = d->leadds;
        while (left--) {
            log_func(stream, "# [%lf] %s:%d\n## %s()\n", hg_time_to_double(d->le[idx].time), d->le[idx].file,
                     d->le[idx].line, d->le[idx].func);
            idx = (idx + 1) % d->lesize;
        }
    }

    hg_thread_mutex_unlock(&d->dlock);
}

/*---------------------------------------------------------------------------*/
void
hg_dlog_dump_counters(struct hg_dlog *d, int (*log_func)(FILE *, const char *, ...), FILE *stream,
                      int trylock)
{
    struct hg_dlog_dcount32 *dc32;
    struct hg_dlog_dcount64 *dc64;

    if (trylock) {
        int try_ret = hg_thread_mutex_try_lock(&d->dlock);
        if (try_ret != HG_UTIL_SUCCESS) /* warn them, but keep going */ {
            fprintf(stderr, "hg_dlog_dump: WARN - lock failed\n");
            return;
        }
    }
    else
        hg_thread_mutex_lock(&d->dlock);

    if (!HG_LIST_IS_EMPTY(&d->cnts32) || !HG_LIST_IS_EMPTY(&d->cnts64)) {
        log_func(stream,
                 "### ----------------------\n"
                 "### (%s) counter log summary\n"
                 "### ----------------------\n",
                 (d->dlog_magic + strlen(HG_DLOG_STDMAGIC)));

        log_func(stream, "# Counters\n");
        HG_LIST_FOREACH(dc32, &d->cnts32, l)
        {
            log_func(stream, "# %s: %" PRId32 " [%s]\n", dc32->name, hg_atomic_get32(&dc32->c), dc32->descr);
        }
        HG_LIST_FOREACH(dc64, &d->cnts64, l)
        {
            log_func(stream, "# %s: %" PRId64 " [%s]\n", dc64->name, hg_atomic_get64(&dc64->c), dc64->descr);
        }
        log_func(stream, "# -\n");
    }

    hg_thread_mutex_unlock(&d->dlock);
}

/*---------------------------------------------------------------------------*/
void
hg_dlog_dump_file(struct hg_dlog *d, const char *base, int addpid, int trylock)
{
    char                     buf[2048];
    int                      pid;
    FILE                    *fp = NULL;
    unsigned int             left, idx;
    struct hg_dlog_dcount32 *dc32;
    struct hg_dlog_dcount64 *dc64;

#ifdef _WIN32
    pid = _getpid();
#else
    pid = getpid();
#endif

    if (addpid)
        snprintf(buf, sizeof(buf), "%s-%d.log", base, pid);
    else
        snprintf(buf, sizeof(buf), "%s.log", base);

    fp = fopen(buf, "w");
    if (!fp) {
        perror("fopen");
        return;
    }

    if (trylock) {
        int try_ret = hg_thread_mutex_try_lock(&d->dlock);
        if (try_ret != HG_UTIL_SUCCESS) /* warn them, but keep going */ {
            fprintf(stderr, "hg_dlog_dump: WARN - lock failed\n");
            fclose(fp);
            return;
        }
    }
    else
        hg_thread_mutex_lock(&d->dlock);

    fprintf(fp, "# START COUNTERS\n");
    HG_LIST_FOREACH(dc32, &d->cnts32, l)
    {
        fprintf(fp, "%s %d %" PRId32 " # %s\n", dc32->name, pid, hg_atomic_get32(&dc32->c), dc32->descr);
    }
    HG_LIST_FOREACH(dc64, &d->cnts64, l)
    {
        fprintf(fp, "%s %d %" PRId64 " # %s\n", dc64->name, pid, hg_atomic_get64(&dc64->c), dc64->descr);
    }
    fprintf(fp, "# END COUNTERS\n\n");

    fprintf(fp, "# NLOGS %d FOR %d\n", d->leadds, pid);

    idx  = (d->lefree < d->leadds) ? d->lesize + d->lefree - d->leadds : d->lefree - d->leadds;
    left = d->leadds;
    while (left--) {
        fprintf(fp, "%lf %d %s %u %s %s %p\n", hg_time_to_double(d->le[idx].time), pid, d->le[idx].file,
                d->le[idx].line, d->le[idx].func, d->le[idx].msg, d->le[idx].data);
        idx = (idx + 1) % d->lesize;
    }

    hg_thread_mutex_unlock(&d->dlock);
    fclose(fp);
}
