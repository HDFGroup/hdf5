/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_INET_H
#define MERCURY_INET_H

#include "mercury_util_config.h"

#if defined(_WIN32)
#    include <winsock2.h>
#else
#    include "mercury_byteswap.h"
#    include <arpa/inet.h>
#    if defined(__APPLE__)
#        include <machine/endian.h>
#    else
#        include <endian.h>
#    endif
#    if !defined(BYTE_ORDER)
#        if defined(__BYTE_ORDER) && defined(__LITTLE_ENDIAN) &&               \
            defined(__BIG_ENDIAN)
#            define BYTE_ORDER    __BYTE_ORDER
#            define LITTLE_ENDIAN __LITTLE_ENDIAN
#            define BIG_ENDIAN    __BIG_ENDIAN
#        else
#            error "cannot determine endianness!"
#        endif
#    endif
#    if BYTE_ORDER == LITTLE_ENDIAN
#        ifndef htonll
#            define htonll(x) bswap_64(x)
#        endif
#        ifndef ntohll
#            define ntohll(x) bswap_64(x)
#        endif
#    else
#        ifndef htonll
#            define htonll(x) (x)
#        endif
#        ifndef ntohll
#            define ntohll(x) (x)
#        endif
#    endif
#endif

#endif /* MERCURY_INET_H */
