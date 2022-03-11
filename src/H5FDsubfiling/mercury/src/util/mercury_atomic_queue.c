/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

/* Implementation derived from:
 * https://github.com/freebsd/freebsd/blob/master/sys/sys/buf_ring.h
 *
 * -
 * Copyright (c) 2007-2009 Kip Macy <kmacy@freebsd.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#include "mercury_atomic_queue.h"
#include "mercury_util_error.h"

#include <stdlib.h>

/****************/
/* Local Macros */
/****************/

/* From <sys/param.h> */
#define powerof2(x) ((((x)-1) & (x)) == 0)

/*---------------------------------------------------------------------------*/
struct hg_atomic_queue *
hg_atomic_queue_alloc(unsigned int count)
{
    struct hg_atomic_queue *hg_atomic_queue = NULL;

    HG_UTIL_CHECK_ERROR_NORET(!powerof2(count), done, "atomic queue size must be power of 2");

    hg_atomic_queue = hg_mem_aligned_alloc(HG_MEM_CACHE_LINE_SIZE, sizeof(struct hg_atomic_queue) +
                                                                       count * sizeof(hg_atomic_int64_t));
    HG_UTIL_CHECK_ERROR_NORET(hg_atomic_queue == NULL, done, "Could not allocate atomic queue");

    hg_atomic_queue->prod_size = hg_atomic_queue->cons_size = count;
    hg_atomic_queue->prod_mask = hg_atomic_queue->cons_mask = count - 1;
    hg_atomic_init32(&hg_atomic_queue->prod_head, 0);
    hg_atomic_init32(&hg_atomic_queue->cons_head, 0);
    hg_atomic_init32(&hg_atomic_queue->prod_tail, 0);
    hg_atomic_init32(&hg_atomic_queue->cons_tail, 0);

done:
    return hg_atomic_queue;
}

/*---------------------------------------------------------------------------*/
void
hg_atomic_queue_free(struct hg_atomic_queue *hg_atomic_queue)
{
    hg_mem_aligned_free(hg_atomic_queue);
}
