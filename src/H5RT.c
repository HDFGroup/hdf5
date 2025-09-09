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

/****************/
/* Module Setup */
/****************/
// TODO - proper module setup
#define H5RT_MODULE

#include "H5RTpkg.h"

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic functions */
#include "H5Eprivate.h"  /* Error handling */
#include "H5FLprivate.h" /* Free lists */

H5FL_DEFINE_STATIC(H5RT_t);
H5FL_DEFINE_STATIC(H5RT_node_t);

herr_t H5RT__fill(H5RT_node_t *node, int rank, H5RT_leaf_t *leaves, size_t count, bool root, int last_sort_dim);
static void H5RT__search_recurse(H5RT_node_t *node, int rank, hsize_t min[], hsize_t max[], H5RT_leaf_t **head, H5RT_leaf_t **tail);
static void H5RT__free_recurse(H5RT_node_t *node);
static bool intersect(hsize_t min1[], hsize_t max1[], hsize_t min2[], hsize_t max2[]);

// TODO - temp version for build
static bool intersect(hsize_t min1[], hsize_t max1[], hsize_t min2[], hsize_t max2[])
{
    bool ret_value = true;

    for (int i = 0; i < H5S_MAX_RANK; i++)
        if (min1[i] > max2[i] || min2[i] > max1[i])
            ret_value = false;

    return ret_value;
} /* end intersect() */

herr_t
H5RT__fill(H5RT_node_t *node, int rank, H5RT_leaf_t *leaves, size_t count, bool root, int last_sort_dim)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)
    /* Calculate the overall min and max for this node, unless this is the root node.  Could instead do this for children in the loop. */

    if (count <= H5RT_MAX_NODE_SIZE) {
        // node->children.leaves = leaves;
        node->nchildren = (int)count;
        node->children_are_leaves = true;
    }
    else {
        /* Pick a dimension to split along. This may simply be the biggest dimension or could check for something like amount of overlap */

        /* Sort leaves by the mid point along this dimension */

        /* Split into approximately N blocks, where N is the target node size, but no more than H5RT_MAX_NODE_SIZE */

        /* Iterate over blocks, allocating the H5RT_node_t for each (in node->children.nodes[i]), and recursively calling this function for each block with parameter node = &node->children.nodes[i], leaves = a pointer to the first leaf in this block, and count = the number of leaves in this block */
        // TODO
        size_t nblocks = 1;
        size_t block_size = 1;
        size_t leaves_left = 1;
        int sort_dimension = -1;
        // TODO - minimum of block size/leaves left
        size_t next_count = (block_size < leaves_left) ? block_size : leaves_left;

        for (size_t i = 0; i < nblocks; i++){
            if (NULL == (node->children.nodes[i] = H5FL_MALLOC(H5RT_node_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "failed to allocate memory for R-tree node");
            if (H5RT__fill(node->children.nodes[i], rank, leaves + (i * block_size), next_count, false, sort_dimension) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "failed to fill R-tree");
        }
        node->nchildren = nblocks;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5RT__fill() */

/* Creates a new R-tree of rank rank, filling it with count leaves.  Takes ownership of the leaves array. */
H5RT_t *
H5RT_create(int rank, H5RT_leaf_t *leaves, size_t count)
{
    H5RT_t *rtree = NULL;
    H5RT_t *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    if (NULL == (rtree = H5FL_MALLOC(H5RT_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "failed ot allocate memory for R-tree");

    rtree->leaves = leaves;

    // TODO - proper sort dim
    if (H5RT__fill(&rtree->root, rank, leaves, count, true, 0) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, NULL, "failed to fill R-tree");

    ret_value = rtree;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5RT_create() */

static void
H5RT__search_recurse(H5RT_node_t *node, int rank, hsize_t min[], hsize_t max[], H5RT_leaf_t **head, H5RT_leaf_t **tail)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check all children for intersection */
    for (int i = 0; i < node->nchildren; i++)
        if (node->children_are_leaves) {
            if (intersect(min, max, node->children.leaves[i]->min, node->children.leaves[i]->max)) {
                /* We found an intersectinig leaf, add it to the linked list of leaves */
                if (*tail) {
                    assert(*head);
                    (*tail)->next = node->children.leaves[i];
                }
                else {
                    assert(!*head);
                    *head = node->children.leaves[i];
                }
                *tail = node->children.leaves[i];
            }
        }
        else if (intersect(min, max, node->children.nodes[i]->min, node->children.nodes[i]->max)) {
            /* We found an intersecting internal node, recurse into it */
            // TODO - Just for build
            H5RT_leaf_t **fake_head = NULL;
            H5RT__search_recurse(node->children.nodes[i], rank, min, max, fake_head, tail);
        }

    FUNC_LEAVE_NOAPI_VOID
} /* end H5RT__search_recurse() */

/* Returns a linked list of leaves whose bounding boxes intersect with min and max */
H5RT_leaf_t *
H5RT_search(H5RT_t *rtree, hsize_t min[], hsize_t max[])
{
    H5RT_leaf_t *head = NULL;
    H5RT_leaf_t *tail = NULL;
    H5RT_leaf_t *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOERR

    /* Perform the actual search */
    H5RT__search_recurse(&rtree->root, rtree->rank, min, max, &head, &tail);

    /* Terminate the linked list (since we don't clean up the "next" pointers in general */
    if (tail)
        tail->next = NULL;

    /* Return the linked list */
    ret_value = head;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5RT_search() */

static void
H5RT__free_recurse(H5RT_node_t *node)
{
    FUNC_ENTER_NOAPI_NOERR

    if (!node->children_are_leaves)
        for (int i = 0; i < node->nchildren; i++) {
            H5RT__free_recurse(node->children.nodes[i]);
            H5FL_FREE(H5RT_node_t, node->children.nodes[i]);
        }

    FUNC_LEAVE_NOAPI_VOID
}

/* Deletes and frees all memory used by the R-tree, including the leaves array */
void
H5RT_free(H5RT_t *rtree)
{
    FUNC_ENTER_NOAPI_NOERR

    H5RT__free_recurse(&rtree->root);
    free(rtree->leaves);
    H5FL_FREE(H5RT_t, rtree);

    FUNC_LEAVE_NOAPI_VOID
}
