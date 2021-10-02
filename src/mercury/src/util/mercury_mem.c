/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#include "mercury_mem.h"

#include "mercury_util_error.h"

#ifdef _WIN32
#include <windows.h>
#else
#include <errno.h>
#include <fcntl.h> /* For O_* constants */
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h> /* For mode constants */
#include <sys/types.h>
#include <unistd.h>
#endif
#include <stdlib.h>

/*---------------------------------------------------------------------------*/
long
hg_mem_get_page_size(void)
{
    static long page_size = 0;

    if (page_size == 0) {
#ifdef _WIN32
        SYSTEM_INFO system_info;
        GetSystemInfo(&system_info);
        page_size = system_info.dwPageSize;
#else
        page_size = sysconf(_SC_PAGE_SIZE);
#endif
    }

    return page_size;
}

/*---------------------------------------------------------------------------*/
void *
hg_mem_aligned_alloc(size_t alignment, size_t size)
{
    void *mem_ptr = NULL;

#ifdef _WIN32
    mem_ptr = _aligned_malloc(size, alignment);
#else
#ifdef _ISOC11_SOURCE
    mem_ptr = aligned_alloc(alignment, size);
#else
    int rc = posix_memalign(&mem_ptr, alignment, size);
    if (rc != 0)
        return NULL;
#endif
#endif

    return mem_ptr;
}

/*---------------------------------------------------------------------------*/
void
hg_mem_aligned_free(void *mem_ptr)
{
#ifdef _WIN32
    _aligned_free(mem_ptr);
#else
    free(mem_ptr);
#endif
}

/*---------------------------------------------------------------------------*/
void *
hg_mem_header_alloc(size_t header_size, size_t alignment, size_t size)
{
    const size_t pad =
        (alignment == 0 || header_size % alignment == 0) ? 0 : alignment - header_size % alignment;

    return (char *)malloc(header_size + pad + size) + header_size + pad;
}

/*---------------------------------------------------------------------------*/
void
hg_mem_header_free(size_t header_size, size_t alignment, void *mem_ptr)
{
    const size_t pad =
        (alignment == 0 || header_size % alignment == 0) ? 0 : alignment - header_size % alignment;

    free((char *)mem_ptr - header_size - pad);
}

/*---------------------------------------------------------------------------*/
void *
hg_mem_shm_map(const char *name, size_t size, hg_util_bool_t create)
{
    void *mem_ptr = NULL;
#ifdef _WIN32
    HANDLE        fd     = INVALID_HANDLE_VALUE;
    LARGE_INTEGER large  = {.QuadPart = size};
    DWORD         access = FILE_MAP_READ | FILE_MAP_WRITE;
    BOOL          rc;

    if (create) {
        fd = CreateFileMappingA(INVALID_HANDLE_VALUE, 0, PAGE_READWRITE, large.HighPart, large.LowPart, name);
        HG_UTIL_CHECK_ERROR_NORET(!fd, error, "CreateFileMappingA() failed");
    }
    else {
        fd = OpenFileMappingA(access, FALSE, name);
        HG_UTIL_CHECK_ERROR_NORET(!fd, error, "OpenFileMappingA() failed");
    }

    mem_ptr = MapViewOfFile(fd, access, 0, 0, size);
    HG_UTIL_CHECK_ERROR_NORET(!mem_ptr, error, "MapViewOfFile() failed");

    /* The handle can be closed without affecting the memory mapping */
    rc = CloseHandle(fd);
    HG_UTIL_CHECK_ERROR_NORET(!rc, error, "CloseHandle() failed");
#else
    int fd = 0;
    int flags = O_RDWR | (create ? O_CREAT : 0);
    struct stat shm_stat;
    int rc;

    fd = shm_open(name, flags, S_IRUSR | S_IWUSR);
    HG_UTIL_CHECK_ERROR_NORET(fd < 0, error, "shm_open() failed (%s)", strerror(errno));

    rc = fstat(fd, &shm_stat);
    HG_UTIL_CHECK_ERROR_NORET(rc != 0, error, "fstat() failed (%s)", strerror(errno));

    if (shm_stat.st_size == 0) {
        rc = ftruncate(fd, (off_t)size);
        HG_UTIL_CHECK_ERROR_NORET(rc != 0, error, "ftruncate() failed (%s)", strerror(errno));
    }
    else
        HG_UTIL_CHECK_ERROR_NORET(shm_stat.st_size < (off_t)size, error, "shm file size too small");

    mem_ptr = mmap(NULL, size, PROT_WRITE | PROT_READ, MAP_SHARED, fd, 0);
    HG_UTIL_CHECK_ERROR_NORET(mem_ptr == MAP_FAILED, error, "mmap() failed (%s)", strerror(errno));

    /* The file descriptor can be closed without affecting the memory mapping */
    rc = close(fd);
    HG_UTIL_CHECK_ERROR_NORET(rc != 0, error, "close() failed (%s)", strerror(errno));
#endif

    return mem_ptr;

error:
#ifdef _WIN32
    if (fd)
        CloseHandle(fd);
#else
    if (fd > 0)
        close(fd);
#endif

    return NULL;
}

/*---------------------------------------------------------------------------*/
int
hg_mem_shm_unmap(const char *name, void *mem_ptr, size_t size)
{
    int ret = HG_UTIL_SUCCESS;

#ifdef _WIN32
    if (mem_ptr) {
        BOOL rc = UnmapViewOfFile(mem_ptr);
        HG_UTIL_CHECK_ERROR(!rc, done, ret, HG_UTIL_FAIL, "UnmapViewOfFile() failed");
    }
#else
    if (mem_ptr && mem_ptr != MAP_FAILED) {
        int rc = munmap(mem_ptr, size);
        HG_UTIL_CHECK_ERROR(rc != 0, done, ret, HG_UTIL_FAIL, "munmap() failed (%s)", strerror(errno));
    }

    if (name) {
        int rc = shm_unlink(name);
        HG_UTIL_CHECK_ERROR(rc != 0, done, ret, HG_UTIL_FAIL, "shm_unlink() failed (%s)", strerror(errno));
    }
#endif

done:
    return ret;
}
