/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* $Id$ */

#define H5P_PACKAGE     /* prevent warning from including H5Ppkg.h */

#include "H5private.h"          /* Generic functions                        */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5FLprivate.h"	/* Free lists                               */
#include "H5Iprivate.h"		/* IDs                                      */
#include "H5MMprivate.h"        /* Memory management                        */
#include "H5Oprivate.h"         /* Object headers                           */
#include "H5Ppkg.h"             /* Property lists                           */

#if defined (WIN32) && !defined (__MWERKS__) 
#include <sys/types.h>
#include <sys/timeb.h>
#endif

/* Pablo mask */
#define PABLO_MASK  H5O_plist_mask

/* local prototypes */
static void     *H5O_plist_decode(H5F_t *f, const uint8_t *p, H5O_shared_t *sh);
static herr_t    H5O_plist_encode(H5F_t *f, uint8_t *p, const void *_mesg);
static size_t    H5O_plist_size(H5F_t *f, const void *_mesg);
static herr_t    H5O_plist_free(void *_mesg);
static herr_t    H5O_plist_debug(H5F_t *f, const void *_mesg,
                                 FILE *stream, int indent, int fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_PLIST[1] = {{
    H5O_PLIST_ID,               /* message id number                    */
    "plist",                    /* message name for debugging           */
    sizeof(H5P_genplist_t),     /* native message size                  */
    H5O_plist_decode,           /* decode message                       */
    H5O_plist_encode,           /* encode message                       */
    NULL,                       /* copy the native value                */
    H5O_plist_size,             /* size of symbol table entry           */
    NULL,                       /* default reset method                 */
    H5O_plist_free,             /* free method                          */
    NULL,                       /* get share method                     */
    NULL,                       /* set share method                     */
    H5O_plist_debug,            /* debug the message                    */
}};

#define H5O_PLIST_VERSION       1

/* Is the interface initialized? */
static int interface_initialize_g = 0;
#define INTERFACE_INIT          NULL

/* Declare external the free list for hsize_t arrays */
H5FL_ARR_EXTERN(hsize_t);

#define UINT_ENCODE(dst, src)   \
        if (sizeof(src) == 2) {                 \
            UINT16ENCODE(dst, src);             \
        } else if (sizeof(src) == 4) {          \
            UINT32ENCODE(dst, src);             \
        } else {                                \
            /* sizeof(src) == 8 */              \
            UINT64ENCODE(dst, src);             \
        }

#define UINT_DECODE(src, dst)   \
        if (sizeof(dst) == 2) {                 \
            UINT16DECODE(src, dst);             \
        } else if (sizeof(dst) == 4) {          \
            UINT32DECODE(src, dst);             \
        } else {                                \
            /* sizeof(dst) == 8 */              \
            UINT64DECODE(src, dst);             \
        }

#define INT_ENCODE(dst, src)    \
        if (sizeof(src) == 2) {                 \
            INT16ENCODE(dst, src);              \
        } else if (sizeof(src) == 4) {          \
            INT32ENCODE(dst, src);              \
        } else {                                \
            /* sizeof(src) == 8 */              \
            INT64ENCODE(dst, src);              \
        }

#define INT_DECODE(src, dst)    \
        if (sizeof(dst) == 2) {                 \
            INT16DECODE(src, dst);              \
        } else if (sizeof(dst) == 4) {          \
            INT32DECODE(src, dst);              \
        } else {                                \
            /* sizeof(dst) == 8 */              \
            INT64DECODE(src, dst);              \
        }

/*
 * Function:    H5O_plist_decode
 * Purpose:     Decode a property list and return a pointer to a memory
 *              struct with the decoded information.
 *
 *              This function decodes the "raw" form of a serialized
 *              property list in memory native format. The struct is
 *              allocated within this function using malloc() and is
 *              returned to the caller.
 *
 *          H5F_t *f            IN: pointer to the HDF5 file struct
 *          uint8 *p            OUT: the raw information buffer
 *          H5O_shared_t *sh    IN: not used; must be NULL
 *
 * Return:      Success:    Pointer to the new message in native order
 *              Failure:    NULL
 * Programmer:  Bill Wendling, 24, September 2002
 * Modifications:
 */
static void *
H5O_plist_decode(H5F_t UNUSED *f, const uint8_t *p, H5O_shared_t UNUSED *sh)
{
    H5P_genplist_t *new_plist = NULL;
    H5P_genclass_t *pclass; /* property list class to modify        */
    hid_t new_plist_id;     /* property list ID of new list created */
    int version;            /* message version number               */
    unsigned int i, nprops, hashsize;
    void *ret_value;
    
    FUNC_ENTER_NOAPI(H5O_plist_decode, NULL);

    /* check args */
    assert(f);
    assert(p);
    assert(!sh);

    /* Version number */
    version = *p++;

    if (version != H5O_PLIST_VERSION)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL,
                    "bad version number for property list message");

    /* Reserved (for what?) */
    ++p;

    /*
     * Decode the sizes of the parts of the property list. The sizes
     * stored in the file are exact but the parts are aligned on 8-byte
     * boundaries.
     */

    /*
     * Retrieve the name of the property class with its parent(s). It's a
     * regular NULL terminated string.
     */
    pclass = H5P_open_class_path(p);
    p += HDstrlen(p) + 1; /* + 1 for the NULL */

    UINT_DECODE(p, nprops);
    UINT_DECODE(p, hashsize);

    /* Allocate new property list */
    if ((new_plist = H5MM_calloc(sizeof(H5P_genplist_t) +
                                 ((hashsize - 1) * sizeof(H5P_genprop_t *)))) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Initialize new property list */
    new_plist->pclass = pclass;
    new_plist->nprops = 0;      /* Initially the plist has the same
                                   number of properties as the class */
    new_plist->class_init = 0;  /* Initially, wait until the class
                                   callback finishes to set */

    /* Create new property list */
    for (i = 0; i < nprops; ++i) {
        H5P_genprop_t *tprop;
        unsigned str_len;

        /*
         * Allocate and initialize the property structure which is going
         * to hold the information we're reading in.
         */
        if (NULL == (tprop = H5MM_malloc(sizeof(H5P_genprop_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

        tprop->create = NULL;
        tprop->def_value = NULL;
        tprop->set = NULL;
        tprop->get = NULL;
        tprop->del = NULL;
        tprop->copy = NULL;
        tprop->close = NULL;
        tprop->next = NULL;

        /* Grab the XORed value of the name and get the length of the name */
        UINT_DECODE(p, tprop->xor_val);
        UINT_DECODE(p, str_len);

        if (str_len) {
            /* If there is a name, allocate space for it and copy */
            if (NULL == (tprop->name = H5MM_malloc(str_len + 1))) {
                H5MM_xfree(tprop);
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
            }

            HDmemcpy(tprop->name, p, str_len + 1);
            p += str_len + 1;
        } else {
            tprop->name = NULL;
            ++p;
        }

        /* Grab the size of the "value" data */
        UINT_DECODE(p, tprop->size);

        /* Allocate and memcpy the value part of the property. */
        if ((tprop->value = H5MM_malloc(tprop->size)) == NULL) {
            H5MM_xfree(tprop->name);
            H5MM_xfree(tprop);
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
        }

        HDmemcpy(tprop->value, p, tprop->size);
        p += tprop->size;

        /* Insert the initialized property into the property list */
        if (H5P_add_prop(new_plist->props, new_plist->pclass->hashsize, tprop) < 0) {
            H5MM_xfree(tprop->value);
            H5MM_xfree(tprop->name);
            H5MM_xfree(tprop);
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, NULL, "Can't insert property into list");
        }

        /* Increment the number of properties in list */
        ++new_plist->nprops;
    }

    /* Increment the number of property lists derived from class */
    if (H5P_access_class(new_plist->pclass, H5P_MOD_INC_LST) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, NULL, "Can't increment class ref count");

    /* Get an atom for the property list */
    if ((new_plist_id = H5I_register(H5I_GENPROP_LST, new_plist)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, NULL, "unable to atomize property list");

    /*
     * Save the property list ID in the property list struct, for use in
     * the property class's 'close' callback
     */
    new_plist->plist_id = new_plist_id;

    /* Set the class initialization flag */
    new_plist->class_init = 1;

    /* Set return value */
    ret_value = new_plist;  /* success */
    
done:
    FUNC_LEAVE(ret_value);
}

/*
 * Function:    H5O_plist_encode
 * Purpose:     Encode a property list and return a pointer to a memory
 *              struct with the encoded information.
 *
 *              This function encodes the "raw" form of a property list
 *              into a struct in memory native format.
 *
 *          H5F_t *f            IN: pointer to the HDF5 file struct
 *          uint8 *p            OUT: the raw information buffer
 *          const void *mesg    IN: pointer to the metadata to encode
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 * Programmer:  Bill Wendling, 24, September 2002
 * Modifications:
 */
static herr_t
H5O_plist_encode(H5F_t UNUSED *f, uint8_t *p, const void *mesg)
{
    const H5P_genplist_t *plist = (const H5P_genplist_t *)mesg;
    herr_t ret_value = SUCCEED;
    unsigned int i;
    char *class_path;
    uint8_t *old = p;

    FUNC_ENTER_NOAPI(H5O_plist_encode, FAIL);

    /* check args */
    assert(f);
    assert(p);
    assert(plist);

    /* Version */
    *p++ = H5O_PLIST_VERSION;

    /* Reserved */
    *p++ = '\0';

    /*
     * Encode the sizes of the parts of the property list. The sizes
     * stored in the file are exact but the parts are aligned on 8-byte
     * boundaries.
     */

    /*
     * The class name encoded will look like:
     *
     *      BaseClass/ParentClass/.../DerivedClass
     */
    class_path = H5P_get_class_path(plist->pclass);

    if (class_path) {
        size_t s = HDstrlen(class_path) + 1;

        HDmemcpy(p, class_path, s);
        p += s;
    } else {
        *p++ = '\0';
    }

    HDfree(class_path);
    UINT_ENCODE(p, plist->nprops);
    UINT_ENCODE(p, plist->pclass->hashsize);

    for (i = 0; i < plist->pclass->hashsize; ++i) {
        H5P_genprop_t *tprop = plist->props[i];

        /* Walk through the list of properties at each hash location */
        while (tprop) {
            const char *n = tprop->name;

            /*
             * Copy the meat of the generic property:
             *
             *      1. The XORed version of the name
             *      2. The name of the property
             *      3. The size of the property value
             *      4. The property value
             */
            UINT_ENCODE(p, tprop->xor_val);

            if (n && *n) {
                size_t s = HDstrlen(n);

                UINT_ENCODE(p, s);
                HDmemcpy(p, n, s + 1);
                p += s + 1;
            } else {
                /* if there isn't a name, put a NULL there */
                UINT_ENCODE(p, 0u);
                *p++ = '\0';
            }

            UINT_ENCODE(p, tprop->size);
            HDmemcpy(p, tprop->value, tprop->size);
            p += tprop->size;

            /* Go to next registered property in class */
            tprop = tprop->next;
        }
    }

    fprintf(stderr, "number of bytes == %d\n", p - old);

done:
    FUNC_LEAVE(ret_value);
}

/*
 * Function:    H5O_plist_size
 * Purpose:     Return the raw message size in bytes.
 *
 *              This function returns the size of the raw elements on
 *              success. (Not counting the message type or size fields,
 *              only the data portion of the message). It doesn't take
 *              into account alignment.
 *
 *          H5F_t *f            IN: pointer to the HDF5 file struct
 *          const void *mesg    IN: pointer to the metadata structure
 *
 * Return:      Success:    Size of message
 *              Failure:    0
 * Programmer:  Bill Wendling, 24, September 2002
 * Modifications:
 */
static size_t
H5O_plist_size(H5F_t UNUSED *f, const void *mesg)
{
    const H5P_genplist_t *plist = (const H5P_genplist_t *)mesg;
    const H5P_genclass_t *pclass;
    size_t ret_value;
    char *class_path = NULL;
    unsigned i;

    FUNC_ENTER_NOAPI(H5O_plist_size, 0);

    /* check args */
    assert(plist);

    ret_value = 2 +                             /*version info          */
                1;                              /*reserved              */

    /*
     * Loop through the class and its parent(s) to gather the complete
     * length of the name. The class name encoded will look like:
     *
     *      DerivedClass/ParentClass/.../BaseClass
     */
    pclass = plist->pclass;

    while (pclass) {
        if (pclass->name)
            ret_value += HDstrlen(pclass->name);/*length of class name  */

        if ((pclass = pclass->parent) != NULL)
            ++ret_value;                        /*separating "/"        */
    }

    ++ret_value;                                /*terminating NULL      */

    class_path = H5P_get_class_path(plist->pclass);

    if (class_path)
        ret_value += HDstrlen(class_path) + 1;  /*class path            */
    else
        ++ret_value;

    HDfree(class_path);
    ret_value += sizeof(plist->nprops) +           /*num properties     */
                 sizeof(plist->pclass->hashsize);  /*hash size          */

    for (i = 0; i < plist->pclass->hashsize; ++i) {
        H5P_genprop_t *tprop = plist->props[i];

        /* Walk through the list of properties at each hash location */
        while (tprop) {
            const char *n = tprop->name;

            ret_value += sizeof(tprop->xor_val) + /*xored value         */
                         sizeof(size_t);          /*length of the name  */

            if (n && *n)
                ret_value += HDstrlen(n) + 1;   /*the name              */
            else
                ++ret_value;                    /*the name: NULL        */

            ret_value += sizeof(tprop->size) +  /*size of data size     */
                         tprop->size;           /*the data              */

            /* Go to next registered property in class */
            tprop = tprop->next;
        }
    }

done:
    FUNC_LEAVE(ret_value);
}

/*
 * Function:    H5O_plist_free
 * Purpose:     Free's the property list.
 *
 *          const void *mesg    IN: pointer to the property list to free
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 * Programmer:  Bill Wendling, 24, September 2002
 * Modifications:
 */
static herr_t
H5O_plist_free(void *mesg)
{
    H5P_genplist_t *plist = mesg;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_plist_free, FAIL);
    assert(mesg);

    ret_value = H5P_close(plist);

done:
    FUNC_LEAVE(ret_value);
}

/*
 * Function:    H5O_plist_debug
 * Purpose:     Prints debugging information for the property list message.
 *
 *          H5F_t *f            IN: pointer to the HDF5 file struct
 *          const void *mesg    IN: Pointer to the source property list struct
 *          FILE *stream        IN: Pointer to the stream for output data
 *          int indent          IN: Amount to indent information by
 *          int fwidth          IN: Field width (?)
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 * Programmer:  Bill Wendling, 24, September 2002
 * Modifications:
 */
static herr_t
H5O_plist_debug(H5F_t UNUSED *f, const void *mesg, FILE *stream,
                int indent, int fwidth)
{
    const H5P_genplist_t *plist = (const H5P_genplist_t *)mesg;
    herr_t ret_value = SUCCEED;
    unsigned int i;

    FUNC_ENTER_NOAPI(H5O_plist_debug, FAIL);

    /* check args */
    assert(f);
    assert(plist);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s\n", indent, "", fwidth, "Property List:");
    indent += 2;
    HDfprintf(stream, "%*sNumber of properties: %d\n", indent, "", plist->nprops);
    HDfprintf(stream, "%*sProperties {\n", indent, "");
    indent += 2;

    for (i = 0; i < plist->pclass->hashsize; ++i) {
        H5P_genprop_t *tprop = plist->props[i];

        HDfprintf(stream, "%*sProperty {\n", indent, "");
        indent += 2;

        /* Walk through the list of properties at each hash location */
        while (tprop) {
            register unsigned int j;

            /*
             * Copy the meat of the generic property:
             *
             *      1. The name of the property
             *      2. The XORed version of the name
             *      3. The size of the property value
             *      4. The property value
             */
            HDfprintf(stream, "%*sName: ", indent, "");

            if (tprop->name)
                HDfprintf(stream, "%s\n", tprop->name);
            else
                HDfprintf(stream, "(null)\n");

            HDfprintf(stream, "%*sXOR Value: %d\n", indent, "", tprop->xor_val);
            HDfprintf(stream, "%*sValue Size: %d\n", indent, "", tprop->size);
            HDfprintf(stream, "%*sValue: ", indent, "");

            for (j = 0; j < tprop->size; ++j)
                HDfprintf(stream, "%02x ", ((char *)tprop->value)[j]);

            HDfprintf(stream, "\n");

            /* Go to next registered property in class */
            tprop = tprop->next;
        }

        indent -= 2;
        HDfprintf(stream, "%*s}\n", indent, "");
    }

    indent -= 2;
    HDfprintf(stream, "%*s}\n", indent, "");

    indent -= 2;
    HDfprintf(stream, "%*s}\n", indent, "");

done:
    FUNC_LEAVE(ret_value);
}
