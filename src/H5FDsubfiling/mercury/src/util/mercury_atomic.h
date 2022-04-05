/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_ATOMIC_H
#define MERCURY_ATOMIC_H

#include "mercury_util_config.h"

#if defined(_WIN32)
#include <windows.h>
typedef struct {
    volatile LONG value;
} hg_atomic_int32_t;
typedef struct {
    volatile LONGLONG value;
} hg_atomic_int64_t;
#define HG_ATOMIC_VAR_INIT(x)                                                                                \
    {                                                                                                        \
        (x)                                                                                                  \
    }
#elif defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
#include <opa_primitives.h>
typedef OPA_int_t hg_atomic_int32_t;
typedef OPA_ptr_t hg_atomic_int64_t; /* OPA has only limited 64-bit support */
#define HG_ATOMIC_VAR_INIT(x) OPA_PTR_T_INITIALIZER(x)
#elif defined(HG_UTIL_HAS_STDATOMIC_H)
#ifndef __cplusplus
#include <stdatomic.h>
typedef atomic_int  hg_atomic_int32_t;
#if (HG_UTIL_ATOMIC_LONG_WIDTH == 8) && !defined(__APPLE__)
typedef atomic_long hg_atomic_int64_t;
#else
typedef atomic_llong hg_atomic_int64_t;
#endif
#else
#include <atomic>
typedef std::atomic_int  hg_atomic_int32_t;
#if (HG_UTIL_ATOMIC_LONG_WIDTH == 8) && !defined(__APPLE__)
typedef std::atomic_long hg_atomic_int64_t;
#else
typedef std::atomic_llong hg_atomic_int64_t;
#endif
using std::atomic_fetch_add_explicit;
using std::atomic_thread_fence;
using std::memory_order_acq_rel;
using std::memory_order_acquire;
using std::memory_order_release;
#endif
#define HG_ATOMIC_VAR_INIT(x) ATOMIC_VAR_INIT(x)
#elif defined(__APPLE__)
#include <libkern/OSAtomic.h>
typedef struct {
    volatile hg_util_int32_t value;
} hg_atomic_int32_t;
typedef struct {
    volatile hg_util_int64_t value;
} hg_atomic_int64_t;
#define HG_ATOMIC_VAR_INIT(x)                                                                                \
    {                                                                                                        \
        (x)                                                                                                  \
    }
#else
#error "Not supported on this platform."
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Init atomic value (32-bit integer).
 *
 * \param ptr [OUT]             pointer to an atomic32 integer
 * \param value [IN]            value
 */
static HG_UTIL_INLINE void hg_atomic_init32(hg_atomic_int32_t *ptr, hg_util_int32_t value);

/**
 * Set atomic value (32-bit integer).
 *
 * \param ptr [OUT]             pointer to an atomic32 integer
 * \param value [IN]            value
 */
static HG_UTIL_INLINE void hg_atomic_set32(hg_atomic_int32_t *ptr, hg_util_int32_t value);

/**
 * Get atomic value (32-bit integer).
 *
 * \param ptr [OUT]             pointer to an atomic32 integer
 *
 * \return Value of the atomic integer
 */
static HG_UTIL_INLINE hg_util_int32_t hg_atomic_get32(hg_atomic_int32_t *ptr);

/**
 * Increment atomic value (32-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic32 integer
 *
 * \return Incremented value
 */
static HG_UTIL_INLINE hg_util_int32_t hg_atomic_incr32(hg_atomic_int32_t *ptr);

/**
 * Decrement atomic value (32-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic32 integer
 *
 * \return Decremented value
 */
static HG_UTIL_INLINE hg_util_int32_t hg_atomic_decr32(hg_atomic_int32_t *ptr);

/**
 * OR atomic value (32-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic32 integer
 * \param value [IN]            value to OR with
 *
 * \return Original value
 */
static HG_UTIL_INLINE hg_util_int32_t hg_atomic_or32(hg_atomic_int32_t *ptr, hg_util_int32_t value);

/**
 * XOR atomic value (32-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic32 integer
 * \param value [IN]            value to XOR with
 *
 * \return Original value
 */
static HG_UTIL_INLINE hg_util_int32_t hg_atomic_xor32(hg_atomic_int32_t *ptr, hg_util_int32_t value);

/**
 * AND atomic value (32-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic32 integer
 * \param value [IN]            value to AND with
 *
 * \return Original value
 */
static HG_UTIL_INLINE hg_util_int32_t hg_atomic_and32(hg_atomic_int32_t *ptr, hg_util_int32_t value);

/**
 * Compare and swap values (32-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic32 integer
 * \param compare_value [IN]    value to compare to
 * \param swap_value [IN]       value to swap with if ptr value is equal to
 *                              compare value
 *
 * \return HG_UTIL_TRUE if swapped or HG_UTIL_FALSE
 */
static HG_UTIL_INLINE hg_util_bool_t hg_atomic_cas32(hg_atomic_int32_t *ptr, hg_util_int32_t compare_value,
                                                     hg_util_int32_t swap_value);

/**
 * Init atomic value (64-bit integer).
 *
 * \param ptr [OUT]             pointer to an atomic32 integer
 * \param value [IN]            value
 */
static HG_UTIL_INLINE void hg_atomic_init64(hg_atomic_int64_t *ptr, hg_util_int64_t value);

/**
 * Set atomic value (64-bit integer).
 *
 * \param ptr [OUT]             pointer to an atomic64 integer
 * \param value [IN]            value
 */
static HG_UTIL_INLINE void hg_atomic_set64(hg_atomic_int64_t *ptr, hg_util_int64_t value);

/**
 * Get atomic value (64-bit integer).
 *
 * \param ptr [OUT]             pointer to an atomic64 integer
 *
 * \return Value of the atomic integer
 */
static HG_UTIL_INLINE hg_util_int64_t hg_atomic_get64(hg_atomic_int64_t *ptr);

/**
 * Increment atomic value (64-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic64 integer
 *
 * \return Incremented value
 */
static HG_UTIL_INLINE hg_util_int64_t hg_atomic_incr64(hg_atomic_int64_t *ptr);

/**
 * Decrement atomic value (64-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic64 integer
 *
 * \return Decremented value
 */
static HG_UTIL_INLINE hg_util_int64_t hg_atomic_decr64(hg_atomic_int64_t *ptr);

/**
 * OR atomic value (64-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic64 integer
 * \param value [IN]            value to OR with
 *
 * \return Original value
 */
static HG_UTIL_INLINE hg_util_int64_t hg_atomic_or64(hg_atomic_int64_t *ptr, hg_util_int64_t value);

/**
 * XOR atomic value (64-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic64 integer
 * \param value [IN]            value to XOR with
 *
 * \return Original value
 */
static HG_UTIL_INLINE hg_util_int64_t hg_atomic_xor64(hg_atomic_int64_t *ptr, hg_util_int64_t value);

/**
 * AND atomic value (64-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic64 integer
 * \param value [IN]            value to AND with
 *
 * \return Original value
 */
static HG_UTIL_INLINE hg_util_int64_t hg_atomic_and64(hg_atomic_int64_t *ptr, hg_util_int64_t value);

/**
 * Compare and swap values (64-bit integer).
 *
 * \param ptr [IN/OUT]          pointer to an atomic64 integer
 * \param compare_value [IN]    value to compare to
 * \param swap_value [IN]       value to swap with if ptr value is equal to
 *                              compare value
 *
 * \return HG_UTIL_TRUE if swapped or HG_UTIL_FALSE
 */
static HG_UTIL_INLINE hg_util_bool_t hg_atomic_cas64(hg_atomic_int64_t *ptr, hg_util_int64_t compare_value,
                                                     hg_util_int64_t swap_value);

/**
 * Memory barrier.
 *
 */
static HG_UTIL_INLINE void hg_atomic_fence(void);

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_atomic_init32(hg_atomic_int32_t *ptr, hg_util_int32_t value)
{
#if defined(HG_UTIL_HAS_STDATOMIC_H) && !defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    atomic_init(ptr, value);
#else
    hg_atomic_set32(ptr, value);
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_atomic_set32(hg_atomic_int32_t *ptr, hg_util_int32_t value)
{
#if defined(_WIN32)
    ptr->value = value;
#elif defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    OPA_store_int(ptr, value);
#elif defined(HG_UTIL_HAS_STDATOMIC_H)
    atomic_store_explicit(ptr, value, memory_order_release);
#elif defined(__APPLE__)
    ptr->value = value;
#else
#error "Not supported on this platform."
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int32_t
hg_atomic_get32(hg_atomic_int32_t *ptr)
{
    hg_util_int32_t ret;

#if defined(_WIN32)
    ret = ptr->value;
#elif defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = OPA_load_int(ptr);
#elif defined(HG_UTIL_HAS_STDATOMIC_H)
    ret = atomic_load_explicit(ptr, memory_order_acquire);
#elif defined(__APPLE__)
    ret        = ptr->value;
#else
#error "Not supported on this platform."
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int32_t
hg_atomic_incr32(hg_atomic_int32_t *ptr)
{
    hg_util_int32_t ret;

#if defined(_WIN32)
    ret = InterlockedIncrementNoFence(&ptr->value);
#elif defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = OPA_fetch_and_incr_int(ptr) + 1;
#elif defined(HG_UTIL_HAS_STDATOMIC_H)
    ret = atomic_fetch_add_explicit(ptr, 1, memory_order_acq_rel) + 1;
#elif defined(__APPLE__)
    ret        = OSAtomicIncrement32(&ptr->value);
#else
#error "Not supported on this platform."
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int32_t
hg_atomic_decr32(hg_atomic_int32_t *ptr)
{
    hg_util_int32_t ret;

#if defined(_WIN32)
    ret = InterlockedDecrementNoFence(&ptr->value);
#elif defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = OPA_fetch_and_decr_int(ptr) - 1;
#elif defined(HG_UTIL_HAS_STDATOMIC_H)
    ret = atomic_fetch_sub_explicit(ptr, 1, memory_order_acq_rel) - 1;
#elif defined(__APPLE__)
    ret        = OSAtomicDecrement32(&ptr->value);
#else
#error "Not supported on this platform."
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int32_t
hg_atomic_or32(hg_atomic_int32_t *ptr, hg_util_int32_t value)
{
    hg_util_int32_t ret;

#if defined(_WIN32)
    ret = InterlockedOrNoFence(&ptr->value, value);
#elif defined(HG_UTIL_HAS_STDATOMIC_H) && !defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = atomic_fetch_or_explicit(ptr, value, memory_order_acq_rel);
#elif defined(__APPLE__)
    ret = OSAtomicOr32Orig((uint32_t)value, (volatile uint32_t *)&ptr->value);
#else
    do {
        ret = hg_atomic_get32(ptr);
    } while (!hg_atomic_cas32(ptr, ret, (ret | value)));
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int32_t
hg_atomic_xor32(hg_atomic_int32_t *ptr, hg_util_int32_t value)
{
    hg_util_int32_t ret;

#if defined(_WIN32)
    ret = InterlockedXorNoFence(&ptr->value, value);
#elif defined(HG_UTIL_HAS_STDATOMIC_H) && !defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = atomic_fetch_xor_explicit(ptr, value, memory_order_acq_rel);
#elif defined(__APPLE__)
    ret = OSAtomicXor32Orig((uint32_t)value, (volatile uint32_t *)&ptr->value);
#else
    do {
        ret = hg_atomic_get32(ptr);
    } while (!hg_atomic_cas32(ptr, ret, (ret ^ value)));
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int32_t
hg_atomic_and32(hg_atomic_int32_t *ptr, hg_util_int32_t value)
{
    hg_util_int32_t ret;

#if defined(_WIN32)
    ret = InterlockedAndNoFence(&ptr->value, value);
#elif defined(HG_UTIL_HAS_STDATOMIC_H) && !defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = atomic_fetch_and_explicit(ptr, value, memory_order_acq_rel);
#elif defined(__APPLE__)
    ret = OSAtomicAnd32Orig((uint32_t)value, (volatile uint32_t *)&ptr->value);
#else
    do {
        ret = hg_atomic_get32(ptr);
    } while (!hg_atomic_cas32(ptr, ret, (ret & value)));
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_bool_t
hg_atomic_cas32(hg_atomic_int32_t *ptr, hg_util_int32_t compare_value, hg_util_int32_t swap_value)
{
    hg_util_bool_t ret;

#if defined(_WIN32)
    ret = (compare_value == InterlockedCompareExchangeNoFence(&ptr->value, swap_value, compare_value));
#elif defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = (hg_util_bool_t)(compare_value == OPA_cas_int(ptr, compare_value, swap_value));
#elif defined(HG_UTIL_HAS_STDATOMIC_H)
    ret = atomic_compare_exchange_strong_explicit(ptr, &compare_value, swap_value, memory_order_acq_rel,
                                                  memory_order_acquire);
#elif defined(__APPLE__)
    ret        = OSAtomicCompareAndSwap32(compare_value, swap_value, &ptr->value);
#else
#error "Not supported on this platform."
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_atomic_init64(hg_atomic_int64_t *ptr, hg_util_int64_t value)
{
#if defined(HG_UTIL_HAS_STDATOMIC_H) && !defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    atomic_init(ptr, value);
#else
    hg_atomic_set64(ptr, value);
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_atomic_set64(hg_atomic_int64_t *ptr, hg_util_int64_t value)
{
#if defined(_WIN32)
    ptr->value = value;
#elif defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    OPA_store_ptr(ptr, (void *)value);
#elif defined(HG_UTIL_HAS_STDATOMIC_H)
    atomic_store_explicit(ptr, value, memory_order_release);
#elif defined(__APPLE__)
    ptr->value = value;
#else
#error "Not supported on this platform."
#endif
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int64_t
hg_atomic_get64(hg_atomic_int64_t *ptr)
{
    hg_util_int64_t ret;

#if defined(_WIN32)
    ret = ptr->value;
#elif defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = (hg_util_int64_t)OPA_load_ptr(ptr);
#elif defined(HG_UTIL_HAS_STDATOMIC_H)
    ret = atomic_load_explicit(ptr, memory_order_acquire);
#elif defined(__APPLE__)
    ptr->value = value;
#else
#error "Not supported on this platform."
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int64_t
hg_atomic_incr64(hg_atomic_int64_t *ptr)
{
    hg_util_int64_t ret;

#if defined(_WIN32)
    ret = InterlockedIncrementNoFence64(&ptr->value);
#elif defined(HG_UTIL_HAS_STDATOMIC_H) && !defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = atomic_fetch_add_explicit(ptr, 1L, memory_order_acq_rel) + 1;
#elif defined(__APPLE__)
    ret = OSAtomicIncrement64(&ptr->value);
#else
    do {
        ret = hg_atomic_get64(ptr);
    } while (!hg_atomic_cas64(ptr, ret, ret + 1));
    ret++;
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int64_t
hg_atomic_decr64(hg_atomic_int64_t *ptr)
{
    hg_util_int64_t ret;

#if defined(_WIN32)
    ret = InterlockedDecrementNoFence64(&ptr->value);
#elif defined(HG_UTIL_HAS_STDATOMIC_H) && !defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = atomic_fetch_sub_explicit(ptr, 1L, memory_order_acq_rel) - 1;
#elif defined(__APPLE__)
    ret = OSAtomicDecrement64(&ptr->value);
#else
    do {
        ret = hg_atomic_get64(ptr);
    } while (!hg_atomic_cas64(ptr, ret, ret - 1));
    ret--;
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int64_t
hg_atomic_or64(hg_atomic_int64_t *ptr, hg_util_int64_t value)
{
    hg_util_int64_t ret;

#if defined(_WIN32)
    ret = InterlockedOr64NoFence(&ptr->value, value);
#elif defined(HG_UTIL_HAS_STDATOMIC_H) && !defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = atomic_fetch_or_explicit(ptr, value, memory_order_acq_rel);
#else
    do {
        ret = hg_atomic_get64(ptr);
    } while (!hg_atomic_cas64(ptr, ret, (ret | value)));
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int64_t
hg_atomic_xor64(hg_atomic_int64_t *ptr, hg_util_int64_t value)
{
    hg_util_int64_t ret;

#if defined(_WIN32)
    ret = InterlockedXor64NoFence(&ptr->value, value);
#elif defined(HG_UTIL_HAS_STDATOMIC_H) && !defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = atomic_fetch_xor_explicit(ptr, value, memory_order_acq_rel);
#else
    do {
        ret = hg_atomic_get64(ptr);
    } while (!hg_atomic_cas64(ptr, ret, (ret ^ value)));
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_int64_t
hg_atomic_and64(hg_atomic_int64_t *ptr, hg_util_int64_t value)
{
    hg_util_int64_t ret;

#if defined(_WIN32)
    ret = InterlockedAnd64NoFence(&ptr->value, value);
#elif defined(HG_UTIL_HAS_STDATOMIC_H) && !defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = atomic_fetch_and_explicit(ptr, value, memory_order_acq_rel);
#else
    do {
        ret = hg_atomic_get64(ptr);
    } while (!hg_atomic_cas64(ptr, ret, (ret & value)));
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE hg_util_bool_t
hg_atomic_cas64(hg_atomic_int64_t *ptr, hg_util_int64_t compare_value, hg_util_int64_t swap_value)
{
    hg_util_bool_t ret;

#if defined(_WIN32)
    ret = (compare_value == InterlockedCompareExchangeNoFence64(&ptr->value, swap_value, compare_value));
#elif defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    ret = (hg_util_bool_t)(compare_value ==
                           (hg_util_int64_t)OPA_cas_ptr(ptr, (void *)compare_value, (void *)swap_value));
#elif defined(HG_UTIL_HAS_STDATOMIC_H)
    ret = atomic_compare_exchange_strong_explicit(ptr, &compare_value, swap_value, memory_order_acq_rel,
                                                  memory_order_acquire);
#elif defined(__APPLE__)
    ret = OSAtomicCompareAndSwap64(compare_value, swap_value, &ptr->value);
#else
#error "Not supported on this platform."
#endif

    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_UTIL_INLINE void
hg_atomic_fence(void)
{
#if defined(_WIN32)
    MemoryBarrier();
#elif defined(HG_UTIL_HAS_OPA_PRIMITIVES_H)
    OPA_read_write_barrier();
#elif defined(HG_UTIL_HAS_STDATOMIC_H)
    atomic_thread_fence(memory_order_acq_rel);
#elif defined(__APPLE__)
    OSMemoryBarrier();
#else
#error "Not supported on this platform."
#endif
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_ATOMIC_H */
