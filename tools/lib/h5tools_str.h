/*
 * Copyright (c) 2001 National Center for Supercomputing Applications
 *                    All rights reserved.
 *
 * Programmer:  Bill Wendling <wendling@ncsa.uiuc.edu>
 *              Monday, 19. February 2001
 */
#ifndef H5TOOLS_STR_H__
#define H5TOOLS_STR_H__

typedef struct h5tools_str_t {
    char	*s;		/*allocate string		*/
    size_t	len;		/*length of actual value	*/
    size_t	nalloc;		/*allocated size of string	*/
} h5tools_str_t;

extern void     h5tools_str_close(h5tools_str_t *str);
extern size_t   h5tools_str_len(h5tools_str_t *str);
extern char    *h5tools_str_append(h5tools_str_t *str, const char *fmt, ...);
extern char    *h5tools_str_reset(h5tools_str_t *str);
extern char    *h5tools_str_trunc(h5tools_str_t *str, size_t size);
extern char    *h5tools_str_fmt(h5tools_str_t *str, size_t start, const char *fmt);
extern char    *h5tools_str_prefix(h5tools_str_t *str, const h5dump_t *info,
                                   hsize_t elmtno, int ndims, hsize_t min_idx[],
                                   hsize_t max_idx[]);
extern int      h5tools_str_dump_region(h5tools_str_t *, hid_t, const h5dump_t *);
extern void     h5tools_print_char(h5tools_str_t *, const h5dump_t *, unsigned char);
extern char    *h5tools_str_sprint(h5tools_str_t *str, const h5dump_t *info,
                                   hid_t container, hid_t type, void *vp,
                                   h5tools_context_t *ctx);

#endif  /* H5TOOLS_STR_H__ */
