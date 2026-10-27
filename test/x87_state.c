/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose: Verify that library initialization preserves the x87 and SSE state
 *          of the calling thread.
 */

#include "h5test.h"

#if defined(__x86_64__) && defined(__GNUC__)
#include <cpuid.h>
#define X87_STATE_TEST_SUPPORTED
#endif

#ifdef X87_STATE_TEST_SUPPORTED
typedef struct {
    unsigned short fcw;
    unsigned short fsw;
    unsigned int   mxcsr;
    bool           xinuse_reported;
    uint64_t       xinuse;
} test_fpu_state_t;

static test_fpu_state_t
get_fpu_state(void)
{
    test_fpu_state_t state;
    unsigned int     xgetbv_eax, xgetbv_edx;
    unsigned int     eax, ebx, ecx, edx;
    unsigned int     max_leaf = __get_cpuid_max(0, NULL);
    bool             osxsave  = false;
    bool             xinuse_reported = false;

    __asm__ __volatile__("fnstcw %0" : "=m"(state.fcw));
    __asm__ __volatile__("fnstsw %0" : "=m"(state.fsw));
    __asm__ __volatile__("stmxcsr %0" : "=m"(state.mxcsr));

    if (max_leaf >= 1 && __get_cpuid(1, &eax, &ebx, &ecx, &edx))
        osxsave = (ecx & (1U << 27)) != 0;
    if (osxsave && max_leaf >= 0xd && __get_cpuid_count(0xd, 1, &eax, &ebx, &ecx, &edx))
        xinuse_reported = (eax & (1U << 2)) != 0;

    state.xinuse_reported = xinuse_reported;
    state.xinuse          = 0;
    if (xinuse_reported) {
        __asm__ __volatile__("xgetbv" : "=a"(xgetbv_eax), "=d"(xgetbv_edx) : "c"(1U));
        state.xinuse = ((uint64_t)xgetbv_edx << 32) | xgetbv_eax;
    }

    return state;
}
#endif

int
main(void)
{
#ifdef X87_STATE_TEST_SUPPORTED
    test_fpu_state_t before = get_fpu_state();
    test_fpu_state_t after;

    if (H5open() < 0) {
        H5Eprint2(H5E_DEFAULT, stdout);
        return EXIT_FAILURE;
    }
    after = get_fpu_state();

    if (before.fcw != after.fcw) {
        printf("Failed: H5open changed the x87 control word from %04x to %04x\n", before.fcw, after.fcw);
        return EXIT_FAILURE;
    }
    if (before.fsw != after.fsw) {
        printf("Failed: H5open changed the x87 status word from %04x to %04x\n", before.fsw, after.fsw);
        return EXIT_FAILURE;
    }
    if (before.mxcsr != after.mxcsr) {
        printf("Failed: H5open changed MXCSR from %08x to %08x\n", before.mxcsr, after.mxcsr);
        return EXIT_FAILURE;
    }
    if (before.xinuse_reported && before.xinuse != after.xinuse) {
        printf("Failed: H5open changed XINUSE from %016llx to %016llx\n",
               (unsigned long long)before.xinuse, (unsigned long long)after.xinuse);
        return EXIT_FAILURE;
    }

    if (before.xinuse_reported)
        printf("H5open preserved FPU state (XINUSE = %016llx)\n", (unsigned long long)after.xinuse);
    else
        puts("H5open preserved x87 control/status words and MXCSR (XINUSE not reported)");
#else
    puts("SKIP - x87 state inspection is not supported on this platform");
#endif

    return EXIT_SUCCESS;
}