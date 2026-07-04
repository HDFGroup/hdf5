/************************************************************

  This example shows a way to recursively traverse the file
  using H5Literate.  The method shown here guarantees that
  the recursion will not enter an infinite loop, but does
  not prevent objects from being visited more than once.
  The program prints the directory structure of the file
  specified in FILE.  The default file used by this example
  implements the structure described in the User's Guide,
  chapter 4, figure 26.

Note: This example includes older cases from previous versions
  of HDF5 for historical reference and to illustrate how to
  migrate older code to newer functions. However, readers are
  encouraged to avoid using deprecated functions and earlier
  schemas from those versions.
 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <string.h>

#define FILENAME "h5ex_g_traverse.h5"

/*
 * Define operator data structure type for H5Literate callback.
 * During recursive iteration, these structures will form a
 * linked list that can be searched for duplicate groups,
 * preventing infinite recursion.
 */
struct opdata {
    unsigned       recurs; /* Recursion level.  0=root */
    struct opdata *prev;   /* Pointer to previous opdata */
#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    H5O_token_t token; /* Group token */
#else
    haddr_t addr; /* Group address */
#endif
};

/*
 * Operator function to be called by H5Literate.
 */
herr_t op_func(hid_t loc_id, const char *name, const H5L_info_t *info, void *operator_data);

#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
/* Function to check for duplicate groups in a path. */
int group_check(hid_t loc_id, struct opdata *od, H5O_token_t target_token);

#else
/* Function to check for duplicate groups in a path. */
int group_check(struct opdata *od, haddr_t target_addr);

#endif

int
main(void)
{
    hid_t         file; /* Handle */
    herr_t        status;
    H5O_info_t    infobuf;
    struct opdata od;

    /*
     * Open file and initialize the operator data structure.
     */
    file = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    status = H5Oget_info(file, &infobuf, H5O_INFO_ALL);
    memcpy(&od.token, &infobuf.token, sizeof(H5O_token_t));
#else
    status  = H5Oget_info(file, &infobuf);
    od.addr = infobuf.addr;
#endif
    od.recurs = 0;
    od.prev   = NULL;

    /*
     * Print the root group and formatting, begin iteration.
     */
    printf("/ {\n");
    status = H5Literate(file, H5_INDEX_NAME, H5_ITER_NATIVE, NULL, op_func, (void *)&od);
    printf("}\n");

    /*
     * Close and release resources.
     */
    status = H5Fclose(file);

    return 0;
}

/************************************************************

  Operator function.  This function prints the name and type
  of the object passed to it.  If the object is a group, it
  is first checked against other groups in its path using
  the group_check function, then if it is not a duplicate,
  H5Literate is called for that group.  This guarantees that
  the program will not enter infinite recursion due to a
  circular path in the file.

 ************************************************************/
herr_t
op_func(hid_t loc_id, const char *name, const H5L_info_t *info, void *operator_data)
{
    herr_t         status;
    herr_t         return_val = 0;
    struct opdata *od         = (struct opdata *)operator_data; /* Type conversion */
    unsigned       spaces     = 2 * (od->recurs + 1); /* Number of whitespaces to prepend to output */

    /*
     * Get type of the object and display its name and type.
     * The name of the object is passed to this function by
     * the Library.
     */
    H5O_info_t infobuf;
#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    status = H5Oget_info_by_name(loc_id, name, &infobuf, H5O_INFO_ALL, H5P_DEFAULT);
#else
    status = H5Oget_info_by_name(loc_id, name, &infobuf, H5P_DEFAULT);
#endif
    printf("%*s", spaces, ""); /* Format output */
    switch (infobuf.type) {
        case H5O_TYPE_GROUP:
            printf("Group: %s {\n", name);

            /*
             * Check group address/token against linked list of operator
             * data structures.  We will always run the check, as the
             * reference count cannot be relied upon if there are
             * symbolic links, and H5Oget_info_by_name always follows
             * symbolic links.  Alternatively we could use H5Lget_info
             * and never recurse on groups discovered by symbolic
             * links, however it could still fail if an object's
             * reference count was manually manipulated with
             * H5Odecr_refcount.
             */
#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
            if (group_check(loc_id, od, infobuf.token)) {
#else
            if (group_check(od, infobuf.addr)) {
#endif
                printf("%*s  Warning: Loop detected!\n", spaces, "");
            }
            else {
                if (od->recurs + 1 > 7)
                    return -1;

                /*
                 * Initialize new operator data structure and
                 * begin recursive iteration on the discovered
                 * group.  The new opdata structure is given a
                 * pointer to the current one.
                 */
                struct opdata nextod;
                nextod.recurs = od->recurs + 1;
                nextod.prev   = od;
#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
                memcpy(&nextod.token, &infobuf.token, sizeof(H5O_token_t));
#else
                nextod.addr = infobuf.addr;
#endif
                return_val = H5Literate_by_name(loc_id, name, H5_INDEX_NAME, H5_ITER_NATIVE, NULL, op_func,
                                                (void *)&nextod, H5P_DEFAULT);
            }
            printf("%*s}\n", spaces, "");
            break;
        case H5O_TYPE_DATASET:
            printf("Dataset: %s\n", name);
            break;
        case H5O_TYPE_NAMED_DATATYPE:
            printf("Datatype: %s\n", name);
            break;
        default:
            printf("Unknown: %s\n", name);
    }

    return return_val;
}

/************************************************************

  This function recursively searches the linked list of
  opdata structures for one whose address/token matches
  target_addr/token.  Returns 1 if a match is found, and 0
  otherwise.

 ************************************************************/
#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
int
group_check(hid_t loc_id, struct opdata *od, H5O_token_t target_token)
{
    int token_cmp;

    if (H5Otoken_cmp(loc_id, &od->token, &target_token, &token_cmp) < 0) {
        return 0;
    }
    if (!token_cmp) {
        return 1; /* Tokens match */
    }
    else if (!od->recurs)
        return 0; /* Root group reached with no matches */
    else {        /* Recursively examine the next node */
        return group_check(loc_id, od->prev, target_token);
    }
}
#else
int
group_check(struct opdata *od, haddr_t target_addr)
{
    if (od->addr == target_addr)
        return 1; /* Addresses match */
    else if (!od->recurs)
        return 0; /* Root group reached with no matches */
    else {        /* Recursively examine the next node */
        return group_check(od->prev, target_addr);
    }
}
#endif
