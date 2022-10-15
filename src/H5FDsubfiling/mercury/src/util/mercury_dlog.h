/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_DLOG_H
#define MERCURY_DLOG_H

#include "mercury_util_config.h"

#include "mercury_atomic.h"
#include "mercury_list.h"
#include "mercury_thread_mutex.h"
#include "mercury_time.h"

#include <stdio.h>

/*****************/
/* Public Macros */
/*****************/

/*
 * putting a magic number at the front of the dlog allows us to search
 * for a dlog in a coredump file after a crash and examine its contents.
 */
#define HG_DLOG_MAGICLEN 16         /* bytes to reserve for magic# */
#define HG_DLOG_STDMAGIC ">D.LO.G<" /* standard for first 8 bytes */

/*
 * HG_DLOG_INITIALIZER: initializer for a dlog in a global variable.
 * LESIZE is the number of entries in the LE array.  use it like this:
 *
 * #define FOO_NENTS 128
 * struct hg_dlog_entry foo_le[FOO_NENTS];
 * struct hg_dlog foo_dlog = HG_DLOG_INITIALIZER("foo", foo_le, FOO_NENTS, 0);
 */
#define HG_DLOG_INITIALIZER(NAME, LE, LESIZE, LELOOP)                                                        \
    {                                                                                                        \
        HG_DLOG_STDMAGIC NAME, HG_THREAD_MUTEX_INITIALIZER, HG_LIST_HEAD_INITIALIZER(cnts32),                \
            HG_LIST_HEAD_INITIALIZER(cnts64), LE, LESIZE, LELOOP, 0, 0, 0, 0                                 \
    }

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

/*
 * hg_dlog_entry: an entry in the dlog
 */
struct hg_dlog_entry {
    const char  *file; /* file name */
    unsigned int line; /* line number */
    const char  *func; /* function name */
    const char  *msg;  /* entry message (optional) */
    const void  *data; /* user data (optional) */
    hg_time_t    time; /* time added to log */
};

/*
 * hg_dlog_dcount32: 32-bit debug counter in the dlog
 */
struct hg_dlog_dcount32 {
    const char       *name;            /* counter name (short) */
    const char       *descr;           /* description of counter */
    hg_atomic_int32_t c;               /* the counter itself */
    HG_LIST_ENTRY(hg_dlog_dcount32) l; /* linkage */
};

/*
 * hg_dlog_dcount64: 64-bit debug counter in the dlog
 */
struct hg_dlog_dcount64 {
    const char       *name;            /* counter name (short) */
    const char       *descr;           /* description of counter */
    hg_atomic_int64_t c;               /* the counter itself */
    HG_LIST_ENTRY(hg_dlog_dcount64) l; /* linkage */
};

/*
 * hg_dlog: main structure
 */
struct hg_dlog {
    char              dlog_magic[HG_DLOG_MAGICLEN]; /* magic number + name */
    hg_thread_mutex_t dlock;                        /* lock for this data struct */

    /* counter lists */
    HG_LIST_HEAD(hg_dlog_dcount32) cnts32; /* counter list */
    HG_LIST_HEAD(hg_dlog_dcount64) cnts64; /* counter list */

    /* log */
    struct hg_dlog_entry *le;     /* array of log entries */
    unsigned int          lesize; /* size of le[] array */
    int                   leloop; /* circular buffer? */
    unsigned int          lefree; /* next free entry in le[] */
    unsigned int          leadds; /* #adds done if < lesize */
    int                   lestop; /* stop taking new logs */

    int mallocd; /* allocated with malloc? */
};

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * malloc and return a new dlog
 *
 * \param name [IN]             name of dlog (truncated past 8 bytes)
 * \param lesize [IN]           number of entries to allocate for log buffer
 * \param leloop [IN]           set to make log circular (can overwrite old
 *                              entries)
 *
 * \return the new dlog or NULL on malloc error
 */
HG_UTIL_PUBLIC struct hg_dlog *hg_dlog_alloc(char *name, unsigned int lesize, int leloop);

/**
 * free anything we malloc'd on a dlog.  assumes we have the final
 * active reference to dlog  and it won't be used anymore after this
 * call (so no need to lock it).
 *
 * \param d [IN]                the dlog to finalize
 */
HG_UTIL_PUBLIC void hg_dlog_free(struct hg_dlog *d);

/**
 * make a named atomic32 counter in a dlog and return a pointer to
 * it.  we use the dlock to ensure a counter under a given name only
 * gets created once (makes it easy to share a counter across files).
 * aborts if unable to alloc counter.  use it like this:
 *
 * hg_atomic_int32_t *foo_count;
 * static int init = 0;
 * if (init == 0) {
 *   hg_dlog_mkcount32(dlog, &foo_count, "foocount", "counts of foo");
 *   init = 1;
 * }
 *
 * \param d [IN]                dlog to create the counter in
 * \param cptr [IN/OUT]         pointer to use for counter (set to NULL to
 *                              start)
 * \param name [IN]             short one word name for counter
 * \param descr [IN]            short description of counter
 */
HG_UTIL_PUBLIC void hg_dlog_mkcount32(struct hg_dlog *d, hg_atomic_int32_t **cptr, const char *name,
                                      const char *descr);

/**
 * make a named atomic64 counter in a dlog and return a pointer to
 * it.  we use the dlock to ensure a counter under a given name only
 * gets created once (makes it easy to share a counter across files).
 * aborts if unable to alloc counter.  use it like this:
 *
 * hg_atomic_int64_t *foo_count;
 * static int init = 0;
 * if (init == 0) {
 *   hg_dlog_mkcount64(dlog, &foo_count, "foocount", "counts of foo");
 *   init = 1;
 * }
 *
 * \param d [IN]                dlog to create the counter in
 * \param cptr [IN/OUT]         pointer to use for counter (set to NULL to
 *                              start)
 * \param name [IN]             short one word name for counter
 * \param descr [IN]            short description of counter
 */
HG_UTIL_PUBLIC void hg_dlog_mkcount64(struct hg_dlog *d, hg_atomic_int64_t **cptr, const char *name,
                                      const char *descr);

/**
 * attempt to add a log record to a dlog.  the id and msg should point
 * to static strings that are valid throughout the life of the program
 * (not something that is on the stack).
 *
 * \param d [IN]                the dlog to add the log record to
 * \param file [IN]             file entry
 * \param line [IN]             line entry
 * \param func [IN]             func entry
 * \param msg [IN]              log entry message (optional, NULL ok)
 * \param data [IN]             user data pointer for record (optional, NULL ok)
 *
 * \return 1 if added, 0 otherwise
 */
static HG_UTIL_INLINE unsigned int hg_dlog_addlog(struct hg_dlog *d, const char *file, unsigned int line,
                                                  const char *func, const char *msg, const void *data);

/**
 * set the value of stop for a dlog (to enable/disable logging)
 *
 * \param d [IN]                dlog to set stop in
 * \param stop [IN]             value of stop to use (1=stop, 0=go)
 */
HG_UTIL_PUBLIC void hg_dlog_setlogstop(struct hg_dlog *d, int stop);

/**
 * reset the log.  this does not change the counters (since users
 * have direct access to the hg_atomic_int64_t's, we don't need
 * an API to change them here).
 *
 * \param d [IN]                dlog to reset
 */
HG_UTIL_PUBLIC void hg_dlog_resetlog(struct hg_dlog *d);

/**
 * dump dlog info to a stream. set trylock if you want to dump even
 * if it is locked (e.g. you are crashing and you don't care about
 * locking).
 *
 * \param d [IN]                dlog to dump
 * \param log_func [IN]         log function to use (default printf)
 * \param stream [IN]           stream to use
 * \param trylock [IN]          just try to lock (warn if it fails)
 */
HG_UTIL_PUBLIC void hg_dlog_dump(struct hg_dlog *d, int (*log_func)(FILE *, const char *, ...), FILE *stream,
                                 int trylock);

/**
 * dump dlog counters to a stream. set trylock if you want to dump even
 * if it is locked (e.g. you are crashing and you don't care about
 * locking).
 *
 * \param d [IN]                dlog to dump
 * \param log_func [IN]         log function to use (default printf)
 * \param stream [IN]           stream to use
 * \param trylock [IN]          just try to lock (warn if it fails)
 */
HG_UTIL_PUBLIC void hg_dlog_dump_counters(struct hg_dlog *d, int (*log_func)(FILE *, const char *, ...),
                                          FILE *stream, int trylock);

/**
 * dump dlog info to a file.   set trylock if you want to dump even
 * if it is locked (e.g. you are crashing and you don't care about
 * locking).  the output file is "base.log" or base-pid.log" depending
 * on the value of addpid.
 *
 * \param d [IN]                dlog to dump
 * \param base [IN]             output file basename
 * \param addpid [IN]           add pid to output filename
 * \param trylock [IN]          just try to lock (warn if it fails)
 */
HG_UTIL_PUBLIC void hg_dlog_dump_file(struct hg_dlog *d, const char *base, int addpid, int trylock);

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE unsigned int
hg_dlog_addlog(struct hg_dlog *d, const char *file, unsigned int line, const char *func, const char *msg,
               const void *data)
{
    unsigned int rv = 0;
    unsigned int idx;

    hg_thread_mutex_lock(&d->dlock);
    if (d->lestop)
        goto done;
    if (d->leloop == 0 && d->leadds >= d->lesize)
        goto done;
    idx       = d->lefree;
    d->lefree = (d->lefree + 1) % d->lesize;
    if (d->leadds < d->lesize)
        d->leadds++;
    d->le[idx].file = file;
    d->le[idx].line = line;
    d->le[idx].func = func;
    d->le[idx].msg  = msg;
    d->le[idx].data = data;
    hg_time_get_current(&d->le[idx].time);
    rv = 1;

done:
    hg_thread_mutex_unlock(&d->dlock);
    return rv;
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_DLOG_H */
