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

herr_t H5RT__bulk_load(H5RT_node_t *node, int rank, H5RT_leaf_t *leaves, size_t count, bool root, int prev_sort_dim);
static void H5RT__search_recurse(H5RT_node_t *node, int rank, hsize_t min[], hsize_t max[], H5RT_leaf_t **head, H5RT_leaf_t **tail);
static void H5RT__free_recurse(H5RT_node_t *node);
static bool intersect(int rank, hsize_t min1[], hsize_t max1[], hsize_t min2[], hsize_t max2[]);

static int H5RT__leaf_compare(const void* leaf1, const void* leaf2, void *dim);

/* Check if two hyper-rectangles specified by (min1, max1) and (min2, max2) intersect */
static bool intersect(int rank, hsize_t min1[], hsize_t max1[], hsize_t min2[], hsize_t max2[])
{
    for (int i = 0; i < rank; i++)
        if (min1[i] > max2[i] || min2[i] > max1[i])
            return false; /* No overlap in i-th dimension */

    return true;
} /* end intersect() */

/* Returns
 * -1 if leaf1 < leaf2
 * 0 if leaf1 == leaf2
 * 1 if leaf1 > leaf2
 */
static int H5RT__leaf_compare(const void* leaf1, const void* leaf2, void *dim) {
    assert(leaf1);
    assert(leaf2);
    assert(dim);

    const H5RT_leaf_t* l1 = (const H5RT_leaf_t*)leaf1;
    const H5RT_leaf_t* l2 = (const H5RT_leaf_t*)leaf2;
    int sort_dim = *(int*)dim;
    int ret_value = 0;

    /* Compare based on the midpoint of the specified dimension */
    if (l1->mid[sort_dim] < l2->mid[sort_dim]) {
        ret_value = -1;
    } else if (l1->mid[sort_dim] > l2->mid[sort_dim]) {
        ret_value = 1;
    } else {
        ret_value = 0;
    }

    return ret_value;
}

/* Load the provided leaves into the r-tree in an efficient manner.
 *
 * Parameters:
 *   node = the node to fill
 *   rank = the rank of the hyper-rectangles
 *   leaves = a pointer to the first leaf in this block
 *   count = the number of leaves in this block
 *   root = whether this is the root node
 *   prev_sort_dim = the dimension that was last sorted on (or -1 if none)
 * This is an implementation of the sort-tile-recursive (STR) algorithm.
 * See "STR: A Simple and Efficient Algorithm for R-Tree Packing"
 * https://archive.org/details/nasa_techdoc_19970016975/page/n9 */
herr_t
H5RT__bulk_load(H5RT_node_t *node, int rank, H5RT_leaf_t *leaves, size_t count, bool root, int prev_sort_dim)
{
    herr_t ret_value = SUCCEED;
    int sort_dim = -1;
    size_t leaves_left = count; /* Leaves left to partition */
    size_t child_leaf_count = 0;
    H5RT_leaf_t *child_leaf_start = NULL;

    FUNC_ENTER_NOAPI(FAIL)

    if (!node)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid node");
    if (rank < 1 || rank > H5S_MAX_RANK)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid rank");
    if (!leaves)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid leaves");
    if (count == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "must have at least one leaf");
    if (prev_sort_dim < -1)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid previous sort dimension");

    /* If the algorithm is working correctly, at least one should hold:
     * - there are few enough leaves left to fit into one node
     * - there remains at least one more dim to sort along
     */
    if (count > H5RT_MAX_NODE_SIZE && prev_sort_dim == rank - 1)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid state: too many leaves left to fit in one node, but no more dimensions to sort along");

    /* Compute the max/min bounds of the provided node */
    /* Initial values */
    for (size_t i = 0; i < H5S_MAX_RANK; i++) {
        node->min[i] = SIZE_MAX;
        node->max[i] = 0;
    }
    /* Compute max/min from leaves */
    for (size_t i = 0; i < count; i++) {
        for (int d = 0; d < rank; d++) {
            if (leaves[i].min[d] < node->min[d])
                node->min[d] = leaves[i].min[d];
            if (leaves[i].max[d] > node->max[d])
                node->max[d] = leaves[i].max[d];
        }
    }

    if (count <= H5RT_MAX_NODE_SIZE) {
        /* All leaves will fit into this node */
        node->nchildren = (int)count;
        node->children_are_leaves = true;
        node->children.leaves = leaves;
    } else {
        /* Should not have already sorted the last dim */
        assert(prev_sort_dim < rank - 1);

        /* Sort the hyper-rectangles in this region by the first unsorted coordinate of their midpoints */
        sort_dim = prev_sort_dim + 1;
        qsort_r(leaves, count, sizeof(H5RT_leaf_t), H5RT__leaf_compare, &sort_dim);

        /* After leaves are sorted in the current dimension, partition the hyper-rectangles into slabs */
        /* Slab = run of consecutive hyper-rectangles in the sorted list */

        double num_leaf_pages = ceil((double) count / (double) H5RT_MAX_NODE_SIZE);
        /* For the iteration that sorts along the k-th total dimension, the rank in the exponent here
         * should be k */
        double remaining_ranks = (double) rank - (double) (prev_sort_dim + 1);
        assert(remaining_ranks >= 1.0);
        int num_slabs = (int) ceil(pow(num_leaf_pages, 1.0 / remaining_ranks));

        assert(num_slabs <= H5RT_MAX_NODE_SIZE);

        node->nchildren = num_slabs;
        node->children_are_leaves = false;

        /* Recurse down to the next dimension to process each slab/region */
        for (int i = 0; i < num_slabs; i++) {
            /* Allocate child node */
            if (NULL == (node->children.nodes[i] = H5FL_MALLOC(H5RT_node_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "failed to allocate memory for R-tree node");

            /* max/min bounds of child will be computed in the recursion */
            /* Recurse to process this slab */

            /* Most nodes will be filled with H5RT_MAX_NODE_SIZE children */
            /* The last node may have fewer */
            child_leaf_count = (H5RT_MAX_NODE_SIZE < leaves_left ) ? H5RT_MAX_NODE_SIZE : leaves_left;

            /* Advance to the start of the region for the next recursion to partition */
            // TODO: Ideally there should be a check against walking off edge of array here
            child_leaf_start = leaves + (i * H5RT_MAX_NODE_SIZE);
            assert(child_leaf_start);
        
            if (H5RT__bulk_load(node->children.nodes[i], rank, child_leaf_start, child_leaf_count, false, sort_dim) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "failed to fill R-tree");
            
            leaves_left -= H5RT_MAX_NODE_SIZE;
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5RT__bulk_load() */

/* Creates a new R-tree of rank rank, filling it with count leaves. Takes ownership of the leaves array. */
H5RT_t *
H5RT_create(int rank, H5RT_leaf_t *leaves, size_t count)
{
    H5RT_t *rtree = NULL;
    H5RT_t *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    if (rank < 1 || rank > H5S_MAX_RANK)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid rank");

    if (count == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "r-tree must have at least one leaf");

    if (NULL == (rtree = H5FL_MALLOC(H5RT_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "failed to allocate memory for R-tree");

    rtree->rank = rank;

    /* Take ownership of leaves array */
    rtree->leaves = leaves;

    /* Populate the r-tree with nodes containing the provided leaves */
    if (H5RT__bulk_load(&rtree->root, rank, leaves, count, true, -1) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, NULL, "failed to fill R-tree");

    ret_value = rtree;

done:
    if (!ret_value && rtree)
        H5RT_free(rtree);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5RT_create() */

/*
 *  Parameters:
 *     node (in): Node from which to begin the search.
 *     rank (in): rank of r-tree.
 *     min (in): Minimum bounds of spatial search, should have 'rank' dims.
 *     max (in): Maximum bounds of spatial search, should have 'rank' dims.
 *     head (out): Head of the linked list of results. Should be NULL on initial call.
 *     tail (out): Tail of the linked list of results. Should be NULL on initial call.
 */
static void
H5RT__search_recurse(H5RT_node_t *node, int rank, hsize_t min[], hsize_t max[], H5RT_leaf_t **head, H5RT_leaf_t **tail)
{
    hsize_t *curr_min = NULL;
    hsize_t *curr_max = NULL;

    H5RT_leaf_t *curr_leaf = NULL;
    H5RT_node_t *curr_node = NULL;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    assert(node);
    assert(head);
    assert(tail);

    /* Check all children for intersection */
    for (int i = 0; i < node->nchildren; i++)
        if (node->children_are_leaves) {
            curr_leaf = node->children.leaves + i;
            curr_min = curr_leaf->min;
            curr_max = curr_leaf->max;

            if (intersect(rank, min, max, curr_min, curr_max)) {
                /* We found an intersecting leaf, add it to the linked list of leaves */
                if (*tail) {
                    assert(*head);
                    (*tail)->next = curr_leaf;
                }
                else {
                    /* This is the first leaf to be returned - mark it as head */
                    assert(*head == NULL);
                    *head = curr_leaf;
                }
                /* Newly added leaf is the new tail of result list */
                *tail = curr_leaf;
            }
        }
        else {
            /* This is an internal node in the r-tree */
            curr_node = node->children.nodes[i];
            curr_min = curr_node->min;
            curr_max = curr_node->max;

            /* Only recurse into child node if its bounding box overlaps with the search region */
            if (intersect(rank, min, max, curr_min, curr_max)) {
                /* We found an intersecting internal node, recurse into it */
                H5RT__search_recurse(curr_node, rank, min, max, head, tail);
            }  
        }

    FUNC_LEAVE_NOAPI_VOID
} /* end H5RT__search_recurse() */

/* Returns a linked list of leaves whose bounding boxes intersect with min and max
 * TBD: The information used to assemble the return structure is 
 * stored in the leaves themselves, so subsequent/concurrent searches
 * will make previous search results invalid */
H5RT_leaf_t *
H5RT_search(H5RT_t *rtree, hsize_t min[], hsize_t max[])
{
    H5RT_leaf_t *head = NULL;
    H5RT_leaf_t *tail = NULL;
    H5RT_leaf_t *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    assert((hsize_t*)min);
    assert((hsize_t*)max);

    if (!rtree)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid r-tree");

    /* Perform the actual search */
    H5RT__search_recurse(&rtree->root, rtree->rank, min, max, &head, &tail);

    /* Terminate the linked list (since we don't clean up the "next" pointers in general */
    if (tail)
        tail->next = NULL;

    /* Return the linked list */
    ret_value = head;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5RT_search() */

static void
H5RT__free_recurse(H5RT_node_t *node)
{
    FUNC_ENTER_NOAPI_NOERR

    /* Only recurse if the children are more internal nodes */
    if (!node->children_are_leaves)
        for (int i = 0; i < node->nchildren; i++) {
            H5RT__free_recurse(node->children.nodes[i]);
            H5FL_FREE(H5RT_node_t, node->children.nodes[i]);
        }

    FUNC_LEAVE_NOAPI_VOID
}

/* Deletes and frees all memory used by the R-tree, including the leaves array */
herr_t
H5RT_free(H5RT_t *rtree)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL);

    if (!rtree)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid r-tree");

    H5RT__free_recurse(&rtree->root);
    free(rtree->leaves);
    H5FL_FREE(H5RT_t, rtree);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
