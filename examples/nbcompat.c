#include "nbcompat.h"

size_t
strlcpy(char *dst, const char *src, size_t size)
{
    char *      d;
    const char *s;

    for (d = dst, s = src; (s - src) < size; d++, s++) {
        *d = *s;
        if (*s == '\0')
            return s - src;
    }

    dst[size - 1] = '\0';
    return size;
}
