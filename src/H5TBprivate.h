/*-------------------------------------------------------------------------
 * Copyright (C) 2000	National Center for Supercomputing Applications.
 *			All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5TBprivate.h
 *			Apr 22 2000
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Private non-prototype header.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5TBprivate_H
#define _H5TBprivate_H

/* Public headers needed by this file */
#ifdef LATER
#include <H5TBpublic.h>		/*Public API prototypes */
#endif /* LATER */

/* Typedef for key comparison function */
typedef int (*H5TB_cmp_t)(void *k1, void *k2, int cmparg);

/* Shortcut macros for links */
# define   PARENT  0
# define   LEFT    1
# define   RIGHT   2

# define  Parent    link[PARENT]
# define  Lchild    link[LEFT]
# define  Rchild    link[RIGHT]

/* Tree-balancing flags */
# define  TBBT_HEAVY(s) s       /* If the `s' sub-tree is deeper than the other */
# define  TBBT_DOUBLE   4       /* If "heavy" sub-tree is two levels deeper */
# define  TBBT_INTERN   8       /* If node is internal (has two children) */
# define  TBBT_UNBAL    ( TBBT_HEAVY(LEFT) | TBBT_HEAVY(RIGHT) )
# define  TBBT_FLAGS    ( TBBT_UNBAL | TBBT_INTERN | TBBT_DOUBLE )
# define  TBBT_CHILD(s) ( TBBT_INTERN | TBBT_HEAVY(s) )

/* Internal macros */
# define  LeftCnt(node) ( (node)->lcnt )    /* Left descendants */
# define  RightCnt(node) ( (node)->rcnt )   /* Right descendants */
# define  Cnt(node,s)   ( LEFT==(s) ? LeftCnt(node) : RightCnt(node) )
# define  HasChild(n,s) ( Cnt(n,s)>0 )
# define  Heavy(n,s)    ( (s) & (LeftCnt(n)>RightCnt(n) ? LEFT : \
                 LeftCnt(n)==RightCnt(n) ? 0 : RIGHT))
# define  Intern(n)     ( LeftCnt(n) && RightCnt(n) )
# define  UnBal(n)      ( LeftCnt(n)>RightCnt(n) ? LEFT : \
                 LeftCnt(n)==RightCnt(n) ? 0 : RIGHT)
# define  Double(n)     ( TBBT_DOUBLE & (n)->flags )
# define  Other(side)   ( LEFT + RIGHT - (side) )
# define  Delta(n,s)    (  ( Heavy(n,s) ? 1 : -1 )                          \
                            *  ( Double(n) ? 2 : UnBal(n) ? 1 : 0 )  )
# define  SetFlags(n,s,b,i)   (  ( -2<(b) && (b)<2 ? 0 : TBBT_DOUBLE )   \
    |  ( 0>(b) ? TBBT_HEAVY(s) : (b)>0 ? TBBT_HEAVY(Other(s)) : 0 )        \
    |  ( (i) ? TBBT_INTERN : 0 )  )

/* Internal types for flags & leaf counts */
typedef unsigned long tbbt_flag;
typedef unsigned long tbbt_leaf;

/* Threaded node structure */
typedef struct tbbt_node
{
    void *       data;          /* Pointer to user data to be associated with node */
    void *       key;           /* Field to sort nodes on */

    struct tbbt_node *link[3];  /* Pointers to parent, left child, and right child */
    tbbt_flag flags;        /* Combination of the bit fields */
    tbbt_leaf lcnt;         /* count of left children */
    tbbt_leaf rcnt;         /* count of right children */
} TBBT_NODE;

/* Threaded tree structure */
typedef struct tbbt_tree
{
    TBBT_NODE  *root;       /* Pointer to actual root of tbbt tree */
    unsigned long count;    /* The number of nodes in the tree currently */
    unsigned fast_compare;  /* use a faster in-line compare (with casts) instead of function call */
    H5TB_cmp_t  compar;     /* Key comparison function */
    int cmparg;
} TBBT_TREE;

/* Define the "fast compare" values */
#define TBBT_FAST_HADDR_COMPARE    1
#define TBBT_FAST_INTN_COMPARE     2

#if defined c_plusplus || defined __cplusplus
extern      "C"
{
#endif                          /* c_plusplus || __cplusplus */

TBBT_TREE  *H5TB_dmake (H5TB_cmp_t cmp, int arg, unsigned fast_compare);
TBBT_NODE  *H5TB_dfind (TBBT_TREE * tree, void * key, TBBT_NODE ** pp);
TBBT_NODE  *H5TB_find(TBBT_NODE * root, void * key, H5TB_cmp_t cmp,
                 int arg, TBBT_NODE ** pp);
TBBT_NODE  *H5TB_dless (TBBT_TREE * tree, void * key, TBBT_NODE ** pp);
TBBT_NODE  *H5TB_less (TBBT_NODE * root, void * key, H5TB_cmp_t cmp,
                 int arg, TBBT_NODE ** pp);
TBBT_NODE  *H5TB_index (TBBT_NODE * root, unsigned indx);
TBBT_NODE  *H5TB_dins (TBBT_TREE * tree, void * item, void * key);
TBBT_NODE  *H5TB_ins (TBBT_NODE ** root, void * item, void * key, H5TB_cmp_t cmp, int arg);
void *H5TB_rem(TBBT_NODE ** root, TBBT_NODE * node, void * *kp);
TBBT_NODE  *H5TB_first (TBBT_NODE * root);
TBBT_NODE  *H5TB_last (TBBT_NODE * root);
TBBT_NODE  *H5TB_next (TBBT_NODE * node);
TBBT_NODE  *H5TB_prev (TBBT_NODE * node);
TBBT_TREE  *H5TB_dfree (TBBT_TREE * tree, void(*fd) (void *), void(*fk) (void *));
void       *H5TB_free (TBBT_NODE ** root, void(*fd) (void *), void(*fk) (void *));
long        H5TB_count (TBBT_TREE * tree);

#ifdef H5TB_DEBUG
herr_t      H5TB_dump(TBBT_TREE *ptree, void (*key_dump)(void *,void *), intn method);
#endif /* H5TB_DEBUG */

#if defined c_plusplus || defined __cplusplus
}
#endif                          /* c_plusplus || __cplusplus */

#endif  /* _H5TBprivate_H */

