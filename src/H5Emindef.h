/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Generated automatically by bin/make_err -- do not edit */
/* Add new errors to H5err.txt file */

#ifndef H5Emindef_H
#define H5Emindef_H

/***********************************/
/* Minor error message definitions */
/***********************************/

/* ARGS: Argument errors */
/* H5E_BADRANGE */
{false, "Out of range", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_BADTYPE */
    {false, "Inappropriate type", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_BADVALUE */
    {false, "Bad value", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_UNINITIALIZED */
    {false, "Information is uinitialized", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_UNSUPPORTED */
    {false, "Feature is unsupported", H5E_MINOR, &H5E_err_cls_s},

    /* ASYNC: Asynchronous operation errors */
    /* H5E_CANTCANCEL */
    {false, "Can't cancel operation", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTWAIT */
    {false, "Can't wait on operation", H5E_MINOR, &H5E_err_cls_s},

    /* BTREE: B-tree related errors */
    /* H5E_CANTDECODE */
    {false, "Unable to decode value", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTENCODE */
    {false, "Unable to encode value", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTFIND */
    {false, "Unable to check for record", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTINSERT */
    {false, "Unable to insert object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTLIST */
    {false, "Unable to list node", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTMODIFY */
    {false, "Unable to modify record", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTREDISTRIBUTE */
    {false, "Unable to redistribute records", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTREMOVE */
    {false, "Unable to remove object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTSPLIT */
    {false, "Unable to split node", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTSWAP */
    {false, "Unable to swap records", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_EXISTS */
    {false, "Object already exists", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_NOTFOUND */
    {false, "Object not found", H5E_MINOR, &H5E_err_cls_s},

    /* CACHE: Cache related errors */
    /* H5E_CANTCLEAN */
    {false, "Unable to mark metadata as clean", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTCORK */
    {false, "Unable to cork an object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTDEPEND */
    {false, "Unable to create a flush dependency", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTDIRTY */
    {false, "Unable to mark metadata as dirty", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTEXPUNGE */
    {false, "Unable to expunge a metadata cache entry", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTFLUSH */
    {false, "Unable to flush data from cache", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTINS */
    {false, "Unable to insert metadata into cache", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTLOAD */
    {false, "Unable to load metadata into cache", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTMARKCLEAN */
    {false, "Unable to mark a pinned entry as clean", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTMARKDIRTY */
    {false, "Unable to mark a pinned entry as dirty", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTMARKSERIALIZED */
    {false, "Unable to mark an entry as serialized", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTMARKUNSERIALIZED */
    {false, "Unable to mark an entry as unserialized", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTNOTIFY */
    {false, "Unable to notify object about action", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTPIN */
    {false, "Unable to pin cache entry", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTPROTECT */
    {false, "Unable to protect metadata", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTRESIZE */
    {false, "Unable to resize a metadata cache entry", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTSERIALIZE */
    {false, "Unable to serialize data from cache", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTTAG */
    {false, "Unable to tag metadata in the cache", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTUNCORK */
    {false, "Unable to uncork an object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTUNDEPEND */
    {false, "Unable to destroy a flush dependency", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTUNPIN */
    {false, "Unable to un-pin cache entry", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTUNPROTECT */
    {false, "Unable to unprotect metadata", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTUNSERIALIZE */
    {false, "Unable to mark metadata as unserialized", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_LOGGING */
    {false, "Failure in the cache logging framework", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_NOTCACHED */
    {false, "Metadata not currently cached", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_PROTECT */
    {false, "Protected metadata error", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_SYSTEM */
    {false, "Internal error detected", H5E_MINOR, &H5E_err_cls_s},

    /* DSPACE: Dataspace errors */
    /* H5E_BADSELECT */
    {false, "Invalid selection", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTAPPEND */
    {false, "Can't append object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTCLIP */
    {false, "Can't clip hyperslab region", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTCOMPARE */
    {false, "Can't compare objects", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTCOUNT */
    {false, "Can't count elements", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTNEXT */
    {false, "Can't move to next iterator location", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTSELECT */
    {false, "Can't select hyperslab", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_INCONSISTENTSTATE */
    {false, "Internal states are inconsistent", H5E_MINOR, &H5E_err_cls_s},

    /* FILE: Generic low-level file I/O errors */
    /* H5E_CLOSEERROR */
    {false, "Close failed", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_FCNTL */
    {false, "File control (fcntl) failed", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_OVERFLOW */
    {false, "Address overflowed", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_READERROR */
    {false, "Read failed", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_SEEKERROR */
    {false, "Seek failed", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_WRITEERROR */
    {false, "Write failed", H5E_MINOR, &H5E_err_cls_s},

    /* FILEACC: File accessibility errors */
    /* H5E_BADFILE */
    {false, "Bad file ID accessed", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTCLOSEFILE */
    {false, "Unable to close file", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTCREATE */
    {false, "Unable to create file", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTDELETEFILE */
    {false, "Unable to delete file", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTLOCKFILE */
    {false, "Unable to lock file", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTOPENFILE */
    {false, "Unable to open file", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTUNLOCKFILE */
    {false, "Unable to unlock file", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_FILEEXISTS */
    {false, "File already exists", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_FILEOPEN */
    {false, "File already open", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_MOUNT */
    {false, "File mount error", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_NOTHDF5 */
    {false, "Not an HDF5 file", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_TRUNCATED */
    {false, "File has been truncated", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_UNMOUNT */
    {false, "File unmount error", H5E_MINOR, &H5E_err_cls_s},

    /* FSPACE: Free space errors */
    /* H5E_CANTMERGE */
    {false, "Can't merge objects", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTREVIVE */
    {false, "Can't revive object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTSHRINK */
    {false, "Can't shrink container", H5E_MINOR, &H5E_err_cls_s},

    /* FUNC: Function entry/exit interface errors */
    /* H5E_ALREADYINIT */
    {false, "Object already initialized", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTINIT */
    {false, "Unable to initialize object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTRELEASE */
    {false, "Unable to release object", H5E_MINOR, &H5E_err_cls_s},

    /* GROUP: Group related errors */
    /* H5E_CANTCLOSEOBJ */
    {false, "Can't close object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTOPENOBJ */
    {false, "Can't open object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_COMPLEN */
    {false, "Name component is too long", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_PATH */
    {false, "Problem with path to object", H5E_MINOR, &H5E_err_cls_s},

    /* HEAP: Heap errors */
    /* H5E_CANTATTACH */
    {false, "Can't attach object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTCOMPUTE */
    {false, "Can't compute value", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTEXTEND */
    {false, "Can't extend heap's space", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTOPERATE */
    {false, "Can't operate on object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTRESTORE */
    {false, "Can't restore condition", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTUPDATE */
    {false, "Can't update object", H5E_MINOR, &H5E_err_cls_s},

    /* ID: Object ID related errors */
    /* H5E_BADGROUP */
    {false, "Unable to find ID group information", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_BADID */
    {false, "Unable to find ID information (already closed?)", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTDEC */
    {false, "Unable to decrement reference count", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTINC */
    {false, "Unable to increment reference count", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTREGISTER */
    {false, "Unable to register new ID", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_NOIDS */
    {false, "Out of IDs for group", H5E_MINOR, &H5E_err_cls_s},

    /* LINK: Link related errors */
    /* H5E_CANTMOVE */
    {false, "Can't move object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTSORT */
    {false, "Can't sort objects", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_NLINKS */
    {false, "Too many soft links in path", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_NOTREGISTERED */
    {false, "Link class not registered", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_TRAVERSE */
    {false, "Link traversal failure", H5E_MINOR, &H5E_err_cls_s},

    /* MAP: Map related errors */
    /* H5E_CANTPUT */
    {false, "Can't put value", H5E_MINOR, &H5E_err_cls_s},

    /* MPI: Parallel MPI errors */
    /* H5E_CANTGATHER */
    {false, "Can't gather data", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTRECV */
    {false, "Can't receive data", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_MPI */
    {false, "Some MPI function failed", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_MPIERRSTR */
    {false, "MPI Error String", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_NO_INDEPENDENT */
    {false, "Can't perform independent IO", H5E_MINOR, &H5E_err_cls_s},

    /* NONE: No error */
    /* H5E_NONE_MINOR */
    {false, "No error", H5E_MINOR, &H5E_err_cls_s},

    /* OHDR: Object header related errors */
    /* H5E_ALIGNMENT */
    {false, "Alignment error", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_BADITER */
    {false, "Iteration failed", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_BADMESG */
    {false, "Unrecognized message", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTDELETE */
    {false, "Can't delete message", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTPACK */
    {false, "Can't pack messages", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTRENAME */
    {false, "Unable to rename object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTRESET */
    {false, "Can't reset object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_LINKCOUNT */
    {false, "Bad object header link count", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_VERSION */
    {false, "Wrong version number", H5E_MINOR, &H5E_err_cls_s},

    /* PIPELINE: I/O pipeline errors */
    /* H5E_CALLBACK */
    {false, "Callback failed", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANAPPLY */
    {false, "Error from filter 'can apply' callback", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTFILTER */
    {false, "Filter operation failed", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_NOENCODER */
    {false, "Filter present but encoding disabled", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_NOFILTER */
    {false, "Requested filter is not available", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_SETLOCAL */
    {false, "Error from filter 'set local' callback", H5E_MINOR, &H5E_err_cls_s},

    /* PLIST: Property list errors */
    /* H5E_CANTGET */
    {false, "Can't get value", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTSET */
    {false, "Can't set value", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_DUPCLASS */
    {false, "Duplicate class name in parent class", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_SETDISALLOWED */
    {false, "Disallowed operation", H5E_MINOR, &H5E_err_cls_s},

    /* PLUGIN: Plugin errors */
    /* H5E_OPENERROR */
    {false, "Can't open directory or file", H5E_MINOR, &H5E_err_cls_s},

    /* RESOURCE: Resource errors */
    /* H5E_ALREADYEXISTS */
    {false, "Object already exists", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTALLOC */
    {false, "Can't allocate space", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTCOPY */
    {false, "Unable to copy object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTFREE */
    {false, "Unable to free object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTGC */
    {false, "Unable to garbage collect", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTGETSIZE */
    {false, "Unable to compute size", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTLOCK */
    {false, "Unable to lock object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_CANTUNLOCK */
    {false, "Unable to unlock object", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_NOSPACE */
    {false, "No space available for allocation", H5E_MINOR, &H5E_err_cls_s},
    /* H5E_OBJOPEN */
    {false, "Object is already open", H5E_MINOR, &H5E_err_cls_s},

    /* SYSTEM: System level errors */
    /* H5E_SYSERRSTR */
    {false, "System error message", H5E_MINOR, &H5E_err_cls_s},

    /* TYPECONV: Datatype conversion errors */
    /* H5E_BADSIZE */
    {false, "Bad size for object", H5E_MINOR, &H5E_err_cls_s},
/* H5E_CANTCONVERT */
{
    false, "Can't convert datatypes", H5E_MINOR, &H5E_err_cls_s
}

#endif /* H5Emindef_H */
