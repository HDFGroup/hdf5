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

/* This file defines resources shared between multiple gentest source files */

#ifndef H5_GENTEST_H
#define H5_GENTEST_H

#define MY_LINKCLASS 187

/* A UD link traversal function.  Shouldn't actually be called. */
static hid_t
UD_traverse(H5_ATTR_UNUSED const char *link_name, H5_ATTR_UNUSED hid_t cur_group,
            H5_ATTR_UNUSED const void *udata, H5_ATTR_UNUSED size_t udata_size, H5_ATTR_UNUSED hid_t lapl_id,
            H5_ATTR_UNUSED hid_t dxpl_id)
{
    return -1;
}

static const H5L_class_t UD_link_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    (H5L_type_t)MY_LINKCLASS, /* Link type id number            */
    "UD link class",          /* name for debugging             */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_traverse,              /* The actual traversal function  */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};

#endif /* H5_GENTEST_H */