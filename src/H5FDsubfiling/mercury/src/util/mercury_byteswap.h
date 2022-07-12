/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_BYTESWAP_H
#define MERCURY_BYTESWAP_H

#include "mercury_util_config.h"

#if defined(_WIN32)
#    include <stdlib.h>
#    define bswap_16(x) _byteswap_ushort(x)
#    define bswap_32(x) _byteswap_ulong(x)
#    define bswap_64(x) _byteswap_uint64(x)
#elif defined(__APPLE__)
#    include <libkern/OSByteOrder.h>
#    define bswap_16(x) OSSwapInt16(x)
#    define bswap_32(x) OSSwapInt32(x)
#    define bswap_64(x) OSSwapInt64(x)
#else
#    include <byteswap.h>
#endif

#endif /* MERCURY_BYTESWAP_H */
