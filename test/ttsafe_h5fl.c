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

/********************************************************************
 *
 * Test the threadsafe correctness of the H5FL routines
 *
 ********************************************************************/

#include "ttsafe.h"

#ifdef H5_HAVE_THREADS

/* Library headers needed */
#include "H5FLprivate.h" /* Free Lists                               */

/* Macros */
#define NUM_THREADS 16

/* Test config */
#define MAX_TOKENS           1024 /* Max # of tokens (buffers) active */
#define NUM_TEST_OPS         4096 /* Number of operations in a test vector */
#define NUM_VECTORS          16   /* Number of vectors for each thread */
#define NUM_ITERS_PER_THREAD 64   /* Number of times to run vectors in each thread */

/* Types of various sizes, for regular free lists */
typedef struct {
    unsigned char buf[16];
} h5fl_reg_test_type_1;

typedef struct {
    unsigned char buf[64];
} h5fl_reg_test_type_2;

typedef struct {
    unsigned char buf[256];
} h5fl_reg_test_type_3;

typedef struct {
    unsigned char buf[1];
} h5fl_reg_test_type_4;

typedef struct {
    unsigned char buf[2];
} h5fl_reg_test_type_5;

typedef struct {
    unsigned char buf[3];
} h5fl_reg_test_type_6;

typedef struct {
    unsigned char buf[5];
} h5fl_reg_test_type_7;

typedef struct {
    unsigned char buf[8];
} h5fl_reg_test_type_8;

typedef struct {
    unsigned char buf[13];
} h5fl_reg_test_type_9;

typedef struct {
    unsigned char buf[21];
} h5fl_reg_test_type_10;

typedef struct {
    unsigned char buf[34];
} h5fl_reg_test_type_11;

typedef struct {
    unsigned char buf[55];
} h5fl_reg_test_type_12;

/* 'regular' free lists of the various types */
H5FL_DEFINE_STATIC(h5fl_reg_test_type_1);
H5FL_DEFINE_STATIC(h5fl_reg_test_type_2);
H5FL_DEFINE_STATIC(h5fl_reg_test_type_3);
H5FL_DEFINE_STATIC(h5fl_reg_test_type_4);
H5FL_DEFINE_STATIC(h5fl_reg_test_type_5);
H5FL_DEFINE_STATIC(h5fl_reg_test_type_6);
H5FL_DEFINE_STATIC(h5fl_reg_test_type_7);
H5FL_DEFINE_STATIC(h5fl_reg_test_type_8);
H5FL_DEFINE_STATIC(h5fl_reg_test_type_9);
H5FL_DEFINE_STATIC(h5fl_reg_test_type_10);
H5FL_DEFINE_STATIC(h5fl_reg_test_type_11);
H5FL_DEFINE_STATIC(h5fl_reg_test_type_12);

/* 'block' free lists of the various types */
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_1);
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_2);
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_3);
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_4);
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_5);
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_6);
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_7);
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_8);
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_9);
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_10);
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_11);
H5FL_BLK_DEFINE_STATIC(h5fl_blk_test_type_12);

typedef struct {
    H5FL_reg_head_t *free_list;
    size_t           elmt_size;
    unsigned char   *fill1;
    unsigned char   *fill2;
    unsigned char   *fill3;
    void            *zero;
} h5fl_reg_type_info;

typedef struct {
    H5FL_fac_head_t *free_list;
    size_t           elmt_size;
    unsigned char   *fill1;
    unsigned char   *fill2;
    unsigned char   *fill3;
    void            *zero;
} h5fl_fac_type_info;

typedef struct {
    H5FL_blk_head_t *free_list;
    size_t initial_size;
} h5fl_blk_type_info;

/* Array of all the 'regular' free lists & info */
static h5fl_reg_type_info h5fl_reg_test_types[] = {
    {&H5FL_REG_NAME(h5fl_reg_test_type_1), sizeof(h5fl_reg_test_type_1), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(h5fl_reg_test_type_2), sizeof(h5fl_reg_test_type_2), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(h5fl_reg_test_type_3), sizeof(h5fl_reg_test_type_3), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(h5fl_reg_test_type_4), sizeof(h5fl_reg_test_type_4), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(h5fl_reg_test_type_5), sizeof(h5fl_reg_test_type_5), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(h5fl_reg_test_type_6), sizeof(h5fl_reg_test_type_6), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(h5fl_reg_test_type_7), sizeof(h5fl_reg_test_type_7), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(h5fl_reg_test_type_8), sizeof(h5fl_reg_test_type_8), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(h5fl_reg_test_type_9), sizeof(h5fl_reg_test_type_9), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(h5fl_reg_test_type_10), sizeof(h5fl_reg_test_type_10), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(h5fl_reg_test_type_11), sizeof(h5fl_reg_test_type_11), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(h5fl_reg_test_type_12), sizeof(h5fl_reg_test_type_12), NULL, NULL, NULL, NULL},
};

/* Array of all the 'factory' free lists & info */
static h5fl_fac_type_info h5fl_fac_test_types[] = {
    {NULL, 16, NULL, NULL, NULL, NULL},  {NULL, 64, NULL, NULL, NULL, NULL},
    {NULL, 256, NULL, NULL, NULL, NULL}, {NULL, 1, NULL, NULL, NULL, NULL},
    {NULL, 2, NULL, NULL, NULL, NULL},   {NULL, 3, NULL, NULL, NULL, NULL},
    {NULL, 5, NULL, NULL, NULL, NULL},   {NULL, 8, NULL, NULL, NULL, NULL},
    {NULL, 13, NULL, NULL, NULL, NULL},  {NULL, 21, NULL, NULL, NULL, NULL},
    {NULL, 34, NULL, NULL, NULL, NULL},  {NULL, 55, NULL, NULL, NULL, NULL},
};

/* Array of all the 'block' free lists & info */
static h5fl_blk_type_info h5fl_blk_test_types[] = {
    {&H5FL_BLK_NAME(h5fl_blk_test_type_1), 16},
    {&H5FL_BLK_NAME(h5fl_blk_test_type_2), 64},
    {&H5FL_BLK_NAME(h5fl_blk_test_type_3), 256},
    {&H5FL_BLK_NAME(h5fl_blk_test_type_4), 1},
    {&H5FL_BLK_NAME(h5fl_blk_test_type_5), 2},
    {&H5FL_BLK_NAME(h5fl_blk_test_type_6), 3},
    {&H5FL_BLK_NAME(h5fl_blk_test_type_7), 5},
    {&H5FL_BLK_NAME(h5fl_blk_test_type_8), 8},
    {&H5FL_BLK_NAME(h5fl_blk_test_type_9), 13},
    {&H5FL_BLK_NAME(h5fl_blk_test_type_10), 21},
    {&H5FL_BLK_NAME(h5fl_blk_test_type_11), 34},
    {&H5FL_BLK_NAME(h5fl_blk_test_type_12), 55},
};

typedef enum {
    H5FL_REG_OP_MALLOC,
    H5FL_REG_OP_CALLOC,
    H5FL_REG_OP_ZERO,
    H5FL_REG_OP_FILL1,
    H5FL_REG_OP_FILL2,
    H5FL_REG_OP_FILL3,
    H5FL_REG_OP_FREE,
} h5fl_reg_test_op_code;

typedef enum {
    H5FL_FAC_OP_MALLOC,
    H5FL_FAC_OP_CALLOC,
    H5FL_FAC_OP_ZERO,
    H5FL_FAC_OP_FILL1,
    H5FL_FAC_OP_FILL2,
    H5FL_FAC_OP_FILL3,
    H5FL_FAC_OP_FREE,
} h5fl_fac_test_op_code;

typedef enum {
    H5FL_BLK_OP_MALLOC,
    H5FL_BLK_OP_CALLOC,
    H5FL_BLK_OP_REALLOC,
    H5FL_BLK_OP_ZERO,
    H5FL_BLK_OP_FILL1,
    H5FL_BLK_OP_FILL2,
    H5FL_BLK_OP_FILL3,
    H5FL_BLK_OP_FREE,
} h5fl_blk_test_op_code;

typedef enum {
    H5FL_REG_ST_UNINIT,
    H5FL_REG_ST_ZERO,
    H5FL_REG_ST_FILL1,
    H5FL_REG_ST_FILL2,
    H5FL_REG_ST_FILL3
} h5fl_reg_token_state;

typedef enum {
    H5FL_FAC_ST_UNINIT,
    H5FL_FAC_ST_ZERO,
    H5FL_FAC_ST_FILL1,
    H5FL_FAC_ST_FILL2,
    H5FL_FAC_ST_FILL3
} h5fl_fac_token_state;

typedef enum {
    H5FL_BLK_ST_UNINIT,
    H5FL_BLK_ST_ZERO,
    H5FL_BLK_ST_FILL1,
    H5FL_BLK_ST_FILL2,
    H5FL_BLK_ST_FILL3
} h5fl_blk_token_state;

typedef struct {
    void                *val;
    unsigned             type_idx;
    h5fl_reg_token_state state;
} h5fl_reg_test_token;

typedef struct {
    void                *val;
    unsigned             type_idx;
    h5fl_fac_token_state state;
} h5fl_fac_test_token;

typedef struct {
    unsigned char                *val;
    unsigned             type_idx;
    size_t         curr_size;
    int            size_shift;
    h5fl_blk_token_state state;
} h5fl_blk_test_token;

typedef union {
    unsigned             type_idx;
    h5fl_reg_test_token *token;
} h5fl_reg_test_op_param;

typedef union {
    unsigned             type_idx;
    h5fl_fac_test_token *token;
} h5fl_fac_test_op_param;

typedef union {
    unsigned             type_idx;
    h5fl_blk_test_token *token;
    int             size_shift;
} h5fl_blk_test_op_param;

typedef struct {
    h5fl_reg_test_op_code  op_code;
    h5fl_reg_test_token   *token;
    h5fl_reg_test_op_param param;
} h5fl_reg_test_op;

typedef struct {
    h5fl_fac_test_op_code  op_code;
    h5fl_fac_test_token   *token;
    h5fl_fac_test_op_param param;
} h5fl_fac_test_op;

typedef struct {
    h5fl_blk_test_op_code  op_code;
    h5fl_blk_test_token   *token;
    h5fl_blk_test_op_param param;
} h5fl_blk_test_op;

typedef struct {
    unsigned          vec_size;
    h5fl_reg_test_op *op_vector;
} h5fl_reg_test_vector;

typedef struct {
    unsigned          vec_size;
    h5fl_fac_test_op *op_vector;
} h5fl_fac_test_vector;

typedef struct {
    unsigned          vec_size;
    h5fl_blk_test_op *op_vector;
} h5fl_blk_test_vector;

typedef struct {
    unsigned              odds;
    h5fl_reg_test_op_code op_code;
} h5fl_reg_test_op_odds;

typedef struct {
    unsigned              odds;
    h5fl_fac_test_op_code op_code;
} h5fl_fac_test_op_odds;

typedef struct {
    unsigned              odds;
    h5fl_blk_test_op_code op_code;
} h5fl_blk_test_op_odds;

/* Operation odds when token array is not full */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_reg_test_op_odds h5fl_reg_all_ops_odds[] = {
    {221, H5FL_REG_OP_MALLOC}, /* 22.1%  = H5FL_REG_OP_MALLOC */
    {221, H5FL_REG_OP_CALLOC}, /* 22.1%  = H5FL_REG_OP_CALLOC */
    {64, H5FL_REG_OP_ZERO},    /* 6.4% = H5FL_REG_OP_ZERO */
    {64, H5FL_REG_OP_FILL1},   /* 6.4% = H5FL_REG_OP_FILL1 */
    {64, H5FL_REG_OP_FILL2},   /* 6.4% = H5FL_REG_OP_FILL2 */
    {64, H5FL_REG_OP_FILL3},   /* 6.4% = H5FL_REG_OP_FILL3 */
    {302, H5FL_REG_OP_FREE},   /* 30.2%   = H5FL_REG_OP_FREE */
};

/* Operation odds when token array is not full */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_fac_test_op_odds h5fl_fac_all_ops_odds[] = {
    {221, H5FL_FAC_OP_MALLOC}, /* 22.1%  = H5FL_FAC_OP_MALLOC */
    {221, H5FL_FAC_OP_CALLOC}, /* 22.1%  = H5FL_FAC_OP_CALLOC */
    {64, H5FL_FAC_OP_ZERO},    /* 6.4% = H5FL_FAC_OP_ZERO */
    {64, H5FL_FAC_OP_FILL1},   /* 6.4% = H5FL_FAC_OP_FILL1 */
    {64, H5FL_FAC_OP_FILL2},   /* 6.4% = H5FL_FAC_OP_FILL2 */
    {64, H5FL_FAC_OP_FILL3},   /* 6.4% = H5FL_FAC_OP_FILL3 */
    {302, H5FL_FAC_OP_FREE},   /* 30.2%   = H5FL_FAC_OP_FREE */
};

/* Operation odds when token array is not full */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_blk_test_op_odds h5fl_blk_all_ops_odds[] = {
    {171, H5FL_BLK_OP_MALLOC}, /* 17.1%  = H5FL_BLK_OP_MALLOC */
    {171, H5FL_BLK_OP_CALLOC}, /* 17.1%  = H5FL_BLK_OP_CALLOC */
    {200, H5FL_BLK_OP_REALLOC}, /* 20.0%  = H5FL_BLK_OP_REALLOC */
    {64, H5FL_BLK_OP_ZERO},    /* 6.4% = H5FL_BLK_OP_ZERO */
    {64, H5FL_BLK_OP_FILL1},   /* 6.4% = H5FL_BLK_OP_FILL1 */
    {64, H5FL_BLK_OP_FILL2},   /* 6.4% = H5FL_BLK_OP_FILL2 */
    {64, H5FL_BLK_OP_FILL3},   /* 6.4% = H5FL_BLK_OP_FILL3 */
    {202, H5FL_BLK_OP_FREE},   /* 20.2%   = H5FL_BLK_OP_FREE */
};

/* Operation odds when token array is full */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_reg_test_op_odds h5fl_reg_full_ops_odds[] = {
    {0, H5FL_REG_OP_MALLOC},  /* 0%  = H5FL_REG_OP_MALLOC */
    {0, H5FL_REG_OP_CALLOC},  /* 0%  = H5FL_REG_OP_CALLOC */
    {104, H5FL_REG_OP_ZERO},  /* 10.4% = H5FL_REG_OP_ZERO */
    {104, H5FL_REG_OP_FILL1}, /* 10.4% = H5FL_REG_OP_FILL1 */
    {104, H5FL_REG_OP_FILL2}, /* 10.4% = H5FL_REG_OP_FILL2 */
    {104, H5FL_REG_OP_FILL3}, /* 10.4% = H5FL_REG_OP_FILL3 */
    {584, H5FL_REG_OP_FREE},  /* 58.4% = H5FL_REG_OP_FREE */
};

/* Operation odds when token array is full */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_fac_test_op_odds h5fl_fac_full_ops_odds[] = {
    {0, H5FL_FAC_OP_MALLOC},  /* 0%  = H5FL_FAC_OP_MALLOC */
    {0, H5FL_FAC_OP_CALLOC},  /* 0%  = H5FL_FAC_OP_CALLOC */
    {104, H5FL_FAC_OP_ZERO},  /* 10.4% = H5FL_FAC_OP_ZERO */
    {104, H5FL_FAC_OP_FILL1}, /* 10.4% = H5FL_FAC_OP_FILL1 */
    {104, H5FL_FAC_OP_FILL2}, /* 10.4% = H5FL_FAC_OP_FILL2 */
    {104, H5FL_FAC_OP_FILL3}, /* 10.4% = H5FL_FAC_OP_FILL3 */
    {584, H5FL_FAC_OP_FREE},  /* 58.4% = H5FL_FAC_OP_FREE */
};

/* Operation odds when token array is full */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_blk_test_op_odds h5fl_blk_full_ops_odds[] = {
    {0, H5FL_BLK_OP_MALLOC},  /* 0%  = H5FL_BLK_OP_MALLOC */
    {0, H5FL_BLK_OP_CALLOC},  /* 0%  = H5FL_BLK_OP_CALLOC */
    {200, H5FL_BLK_OP_REALLOC},  /* 20.0%  = H5FL_BLK_OP_REALLOC */
    {84, H5FL_BLK_OP_ZERO},  /* 8.4% = H5FL_BLK_OP_ZERO */
    {84, H5FL_BLK_OP_FILL1}, /* 8.4% = H5FL_BLK_OP_FILL1 */
    {84, H5FL_BLK_OP_FILL2}, /* 8.4% = H5FL_BLK_OP_FILL2 */
    {84, H5FL_BLK_OP_FILL3}, /* 8.4% = H5FL_BLK_OP_FILL3 */
    {464, H5FL_BLK_OP_FREE},  /* 46.4% = H5FL_BLK_OP_FREE */
};

/* Operation odds when vector is nearly full */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_reg_test_op_odds h5fl_reg_vec_almost_full_ops_odds[] = {
    {0, H5FL_REG_OP_MALLOC},  /* 0%  = H5FL_REG_OP_MALLOC */
    {0, H5FL_REG_OP_CALLOC},  /* 0%  = H5FL_REG_OP_CALLOC */
    {250, H5FL_REG_OP_ZERO},  /* 25% = H5FL_REG_OP_ZERO */
    {250, H5FL_REG_OP_FILL1}, /* 25% = H5FL_REG_OP_FILL1 */
    {250, H5FL_REG_OP_FILL2}, /* 25% = H5FL_REG_OP_FILL2 */
    {250, H5FL_REG_OP_FILL3}, /* 25% = H5FL_REG_OP_FILL3 */
    {0, H5FL_REG_OP_FREE},    /* 0% = H5FL_REG_OP_FREE */
};

/* Operation odds when vector is nearly full */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_fac_test_op_odds h5fl_fac_vec_almost_full_ops_odds[] = {
    {0, H5FL_FAC_OP_MALLOC},  /* 0%  = H5FL_FAC_OP_MALLOC */
    {0, H5FL_FAC_OP_CALLOC},  /* 0%  = H5FL_FAC_OP_CALLOC */
    {250, H5FL_FAC_OP_ZERO},  /* 25% = H5FL_FAC_OP_ZERO */
    {250, H5FL_FAC_OP_FILL1}, /* 25% = H5FL_FAC_OP_FILL1 */
    {250, H5FL_FAC_OP_FILL2}, /* 25% = H5FL_FAC_OP_FILL2 */
    {250, H5FL_FAC_OP_FILL3}, /* 25% = H5FL_FAC_OP_FILL3 */
    {0, H5FL_FAC_OP_FREE},    /* 0% = H5FL_FAC_OP_FREE */
};

/* Operation odds when vector is nearly full */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_blk_test_op_odds h5fl_blk_vec_almost_full_ops_odds[] = {
    {0, H5FL_BLK_OP_MALLOC},  /* 0%  = H5FL_BLK_OP_MALLOC */
    {0, H5FL_BLK_OP_CALLOC},  /* 0%  = H5FL_BLK_OP_CALLOC */
    {400, H5FL_BLK_OP_REALLOC},  /* 40%  = H5FL_BLK_OP_REALLOC */
    {150, H5FL_BLK_OP_ZERO},  /* 15% = H5FL_BLK_OP_ZERO */
    {150, H5FL_BLK_OP_FILL1}, /* 15% = H5FL_BLK_OP_FILL1 */
    {150, H5FL_BLK_OP_FILL2}, /* 15% = H5FL_BLK_OP_FILL2 */
    {150, H5FL_BLK_OP_FILL3}, /* 15% = H5FL_BLK_OP_FILL3 */
    {0, H5FL_BLK_OP_FREE},    /* 0% = H5FL_BLK_OP_FREE */
};

/* Operation odds when token array is empty */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_reg_test_op_odds h5fl_reg_empty_ops_odds[] = {
    {500, H5FL_REG_OP_MALLOC}, /* 50%  = H5FL_REG_OP_MALLOC */
    {500, H5FL_REG_OP_CALLOC}, /* 50%  = H5FL_REG_OP_CALLOC */
    {0, H5FL_REG_OP_ZERO},     /* 0% = H5FL_REG_OP_ZERO */
    {0, H5FL_REG_OP_FILL1},    /* 0% = H5FL_REG_OP_FILL1 */
    {0, H5FL_REG_OP_FILL2},    /* 0% = H5FL_REG_OP_FILL2 */
    {0, H5FL_REG_OP_FILL3},    /* 0% = H5FL_REG_OP_FILL3 */
    {0, H5FL_REG_OP_FREE},     /* 0% = H5FL_REG_OP_FREE */
};

/* Operation odds when token array is empty */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_fac_test_op_odds h5fl_fac_empty_ops_odds[] = {
    {500, H5FL_FAC_OP_MALLOC}, /* 50%  = H5FL_FAC_OP_MALLOC */
    {500, H5FL_FAC_OP_CALLOC}, /* 50%  = H5FL_FAC_OP_CALLOC */
    {0, H5FL_FAC_OP_ZERO},     /* 0% = H5FL_FAC_OP_ZERO */
    {0, H5FL_FAC_OP_FILL1},    /* 0% = H5FL_FAC_OP_FILL1 */
    {0, H5FL_FAC_OP_FILL2},    /* 0% = H5FL_FAC_OP_FILL2 */
    {0, H5FL_FAC_OP_FILL3},    /* 0% = H5FL_FAC_OP_FILL3 */
    {0, H5FL_FAC_OP_FREE},     /* 0% = H5FL_FAC_OP_FREE */
};

/* Operation odds when token array is empty */
/* (Must sum to 1000 (i.e. 100%) */
static const h5fl_blk_test_op_odds h5fl_blk_empty_ops_odds[] = {
    {500, H5FL_BLK_OP_MALLOC}, /* 50%  = H5FL_BLK_OP_MALLOC */
    {500, H5FL_BLK_OP_CALLOC}, /* 50%  = H5FL_BLK_OP_CALLOC */
    {0, H5FL_BLK_OP_REALLOC}, /* 0%  = H5FL_BLK_OP_REALLOC */
    {0, H5FL_BLK_OP_ZERO},     /* 0% = H5FL_BLK_OP_ZERO */
    {0, H5FL_BLK_OP_FILL1},    /* 0% = H5FL_BLK_OP_FILL1 */
    {0, H5FL_BLK_OP_FILL2},    /* 0% = H5FL_BLK_OP_FILL2 */
    {0, H5FL_BLK_OP_FILL3},    /* 0% = H5FL_BLK_OP_FILL3 */
    {0, H5FL_BLK_OP_FREE},     /* 0% = H5FL_BLK_OP_FREE */
};

static unsigned
get_new_h5fl_reg_token(h5fl_reg_test_token *tokens, unsigned *next_token)
{
    unsigned curr_pos  = *next_token;
    unsigned start_pos = curr_pos;

    do {
        /* Check for empty position */
        if (NULL == tokens[curr_pos].val) {
            *next_token = (curr_pos + 1) % MAX_TOKENS;
            return curr_pos;
        }

        curr_pos = (curr_pos + 1) % MAX_TOKENS;
    } while (curr_pos != start_pos);

    assert(curr_pos == start_pos && "Can't find empty position for new token");
    abort();
}

static unsigned
get_new_h5fl_fac_token(h5fl_fac_test_token *tokens, unsigned *next_token)
{
    unsigned curr_pos  = *next_token;
    unsigned start_pos = curr_pos;

    do {
        /* Check for empty position */
        if (NULL == tokens[curr_pos].val) {
            *next_token = (curr_pos + 1) % MAX_TOKENS;
            return curr_pos;
        }

        curr_pos = (curr_pos + 1) % MAX_TOKENS;
    } while (curr_pos != start_pos);

    assert(curr_pos == start_pos && "Can't find empty position for new token");
    abort();
}

static unsigned
get_new_h5fl_blk_token(h5fl_blk_test_token *tokens, unsigned *next_token)
{
    unsigned curr_pos  = *next_token;
    unsigned start_pos = curr_pos;

    do {
        /* Check for empty position */
        if (NULL == tokens[curr_pos].val) {
            *next_token = (curr_pos + 1) % MAX_TOKENS;
            return curr_pos;
        }

        curr_pos = (curr_pos + 1) % MAX_TOKENS;
    } while (curr_pos != start_pos);

    assert(curr_pos == start_pos && "Can't find empty position for new token");
    abort();
}

static h5fl_reg_test_op_code
get_new_h5fl_reg_op(const h5fl_reg_test_op_odds *op_odds)
{
    unsigned idx;
    unsigned rng;

    idx = 0;
    rng = (unsigned)h5_local_rand() % 1000;
    while (0 == op_odds[idx].odds || rng > op_odds[idx].odds) {
        rng -= op_odds[idx].odds;
        idx++;
    }

    return op_odds[idx].op_code;
}

static h5fl_fac_test_op_code
get_new_h5fl_fac_op(const h5fl_fac_test_op_odds *op_odds)
{
    unsigned idx;
    unsigned rng;

    idx = 0;
    rng = (unsigned)h5_local_rand() % 1000;
    while (0 == op_odds[idx].odds || rng > op_odds[idx].odds) {
        rng -= op_odds[idx].odds;
        idx++;
    }

    return op_odds[idx].op_code;
}

static h5fl_blk_test_op_code
get_new_h5fl_blk_op(const h5fl_blk_test_op_odds *op_odds)
{
    unsigned idx;
    unsigned rng;

    idx = 0;
    rng = (unsigned)h5_local_rand() % 1000;
    while (0 == op_odds[idx].odds || rng > op_odds[idx].odds) {
        rng -= op_odds[idx].odds;
        idx++;
    }

    return op_odds[idx].op_code;
}

static unsigned
get_active_h5fl_reg_token(h5fl_reg_test_token *tokens, unsigned num_possible_tokens)
{
    unsigned curr_pos;
    unsigned start_pos;

    start_pos = curr_pos = (unsigned)h5_local_rand() % num_possible_tokens;
    do {
        /* Check for active position */
        if (NULL != tokens[curr_pos].val)
            return curr_pos;

        curr_pos = (curr_pos + 1) % num_possible_tokens;
    } while (curr_pos != start_pos);

    assert(curr_pos == start_pos && "Can't find active token");
    abort();
}

static unsigned
get_active_h5fl_fac_token(h5fl_fac_test_token *tokens, unsigned num_possible_tokens)
{
    unsigned curr_pos;
    unsigned start_pos;

    start_pos = curr_pos = (unsigned)h5_local_rand() % num_possible_tokens;
    do {
        /* Check for active position */
        if (NULL != tokens[curr_pos].val)
            return curr_pos;

        curr_pos = (curr_pos + 1) % num_possible_tokens;
    } while (curr_pos != start_pos);

    assert(curr_pos == start_pos && "Can't find active token");
    abort();
}

static unsigned
get_active_h5fl_blk_token(h5fl_blk_test_token *tokens, unsigned num_possible_tokens)
{
    unsigned curr_pos;
    unsigned start_pos;

    start_pos = curr_pos = (unsigned)h5_local_rand() % num_possible_tokens;
    do {
        /* Check for active position */
        if (NULL != tokens[curr_pos].val)
            return curr_pos;

        curr_pos = (curr_pos + 1) % num_possible_tokens;
    } while (curr_pos != start_pos);

    assert(curr_pos == start_pos && "Can't find active token");
    abort();
}

#if 0
static void
print_h5fl_reg_vector(h5fl_reg_test_vector *vector, h5fl_reg_test_token *tokens)
{
    unsigned num_active_tokens = 0; /* # of active tokens at any position in the test vector execution */

    /* Print test vector */
    for (unsigned u = 0; u < vector->vec_size; u++) {
        switch (vector->op_vector[u].op_code) {
            case H5FL_REG_OP_MALLOC:
                fprintf(stderr, "%04u (%u): H5FL_REG_OP_MALLOC - token: %p, type_idx = %u\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].param.type_idx);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state = H5FL_REG_ST_UNINIT;

                /* Increment # of active tokens */
                num_active_tokens++;
                break;

            case H5FL_REG_OP_CALLOC:
                fprintf(stderr, "%04u (%u): H5FL_REG_OP_CALLOC - token: %p, type_idx = %u\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].param.type_idx);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state = H5FL_REG_ST_ZERO;

                /* Increment # of active tokens */
                num_active_tokens++;
                break;

            case H5FL_REG_OP_ZERO:
                fprintf(stderr, "%04u (%u): H5FL_REG_OP_ZERO   - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = H5FL_REG_ST_ZERO;
                break;

            case H5FL_REG_OP_FILL1:
                fprintf(stderr, "%04u (%u): H5FL_REG_OP_FILL1  - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = H5FL_REG_ST_FILL1;
                break;

            case H5FL_REG_OP_FILL2:
                fprintf(stderr, "%04u (%u): H5FL_REG_OP_FILL2  - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = H5FL_REG_ST_FILL2;
                break;

            case H5FL_REG_OP_FILL3:
                fprintf(stderr, "%04u (%u): H5FL_REG_OP_FILL3  - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = H5FL_REG_ST_FILL3;
                break;

            case H5FL_REG_OP_FREE:
                fprintf(stderr, "%04u (%u): H5FL_REG_OP_FREE   - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);

                /* Decrement # of active tokens */
                num_active_tokens--;
                break;

            default:
                assert (0 && "Invalid op code");
                abort();
        }
    }
}

static void
print_h5fl_fac_vector(h5fl_fac_test_vector *vector, h5fl_fac_test_token *tokens)
{
    unsigned num_active_tokens = 0; /* # of active tokens at any position in the test vector execution */

    /* Print test vector */
    for (unsigned u = 0; u < vector->vec_size; u++) {
        switch (vector->op_vector[u].op_code) {
            case H5FL_FAC_OP_MALLOC:
                fprintf(stderr, "%04u (%u): H5FL_FAC_OP_MALLOC - token: %p, type_idx = %u\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].param.type_idx);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state = H5FL_FAC_ST_UNINIT;

                /* Increment # of active tokens */
                num_active_tokens++;
                break;

            case H5FL_FAC_OP_CALLOC:
                fprintf(stderr, "%04u (%u): H5FL_FAC_OP_CALLOC - token: %p, type_idx = %u\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].param.type_idx);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state = H5FL_FAC_ST_ZERO;

                /* Increment # of active tokens */
                num_active_tokens++;
                break;

            case H5FL_FAC_OP_ZERO:
                fprintf(stderr, "%04u (%u): H5FL_FAC_OP_ZERO   - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = H5FL_FAC_ST_ZERO;
                break;

            case H5FL_FAC_OP_FILL1:
                fprintf(stderr, "%04u (%u): H5FL_FAC_OP_FILL1  - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = H5FL_FAC_ST_FILL1;
                break;

            case H5FL_FAC_OP_FILL2:
                fprintf(stderr, "%04u (%u): H5FL_FAC_OP_FILL2  - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = H5FL_FAC_ST_FILL2;
                break;

            case H5FL_FAC_OP_FILL3:
                fprintf(stderr, "%04u (%u): H5FL_FAC_OP_FILL3  - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = H5FL_FAC_ST_FILL3;
                break;

            case H5FL_FAC_OP_FREE:
                fprintf(stderr, "%04u (%u): H5FL_FAC_OP_FREE   - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);

                /* Decrement # of active tokens */
                num_active_tokens--;
                break;

            default:
                assert (0 && "Invalid op code");
                abort();
        }
    }
}
#endif

static void
init_h5fl_reg_vector(unsigned vec_size, h5fl_reg_test_vector *vector, unsigned num_tokens,
                     h5fl_reg_test_token *tokens)
{
    unsigned num_active_tokens = 0; /* # of active tokens at any position in the test vector execution */
    unsigned curr_alloc_token;      /* Current position for allocating tokens */
    unsigned pos;                   /* Current position in the test vector */
    bool     tokens_wrapped = false;

    /* Allocate the test vector */
    vector->vec_size  = vec_size;
    vector->op_vector = calloc(vec_size, sizeof(h5fl_reg_test_op));
    CHECK_PTR(vector->op_vector, "calloc");

    /* Fiil the test vector, leaving room to free active tokens */
    pos              = 0;
    curr_alloc_token = 0;
    while (pos < (vec_size - num_active_tokens)) {
        h5fl_reg_test_op_code op_code;

        /* Check for active tokens */
        /* (Also must have enough room for both alloc & free operations) */
        if (0 == num_active_tokens && pos < (vec_size - 2))
            op_code = get_new_h5fl_reg_op(h5fl_reg_empty_ops_odds);
        else {
            /* Don't create new tokens when there won't be enough room in the
             * vector for both the alloc & free operations.
             */
            if (pos > ((vec_size - num_active_tokens) - 2))
                op_code = get_new_h5fl_reg_op(h5fl_reg_vec_almost_full_ops_odds);
            /* Don't create new tokens when the token array is full */
            else if (num_tokens == num_active_tokens)
                op_code = get_new_h5fl_reg_op(h5fl_reg_full_ops_odds);
            else
                op_code = get_new_h5fl_reg_op(h5fl_reg_all_ops_odds);
        }

        /* Set op code */
        vector->op_vector[pos].op_code = op_code;

        /* Set up specific parameters for each op code */
        switch (op_code) {
            case H5FL_REG_OP_MALLOC:
            case H5FL_REG_OP_CALLOC: {
                unsigned prev_alloc_token = curr_alloc_token;
                unsigned type_idx;
                unsigned new_token;

                /* RNG type to allocate */
                type_idx  = (unsigned)h5_local_rand() % (unsigned)NELMTS(h5fl_reg_test_types);
                new_token = get_new_h5fl_reg_token(tokens, &curr_alloc_token);
                vector->op_vector[pos].token          = &tokens[new_token];
                vector->op_vector[pos].param.type_idx = type_idx;

                /* Mark token as used */
                tokens[new_token].val = (void *)(~(uintptr_t)NULL);

                /* Increment # of active tokens */
                num_active_tokens++;

                /* Check for tokens wrapping */
                if (curr_alloc_token < prev_alloc_token)
                    tokens_wrapped = true;
            } break;

            case H5FL_REG_OP_ZERO:
            case H5FL_REG_OP_FILL1:
            case H5FL_REG_OP_FILL2:
            case H5FL_REG_OP_FILL3:
            case H5FL_REG_OP_FREE: {
                unsigned token_idx;

                token_idx = get_active_h5fl_reg_token(tokens, tokens_wrapped ? num_tokens : curr_alloc_token);
                vector->op_vector[pos].token = &tokens[token_idx];

                if (H5FL_REG_OP_FREE == op_code) {
                    /* Mark token as free */
                    tokens[token_idx].val = NULL;

                    /* Decrement # of active tokens */
                    num_active_tokens--;
                }
            } break;

            default:
                assert(0 && "Invalid op code");
                abort();
        }

        pos++;
    }

    /* Fill remainder of test vector with free operations */
    while (pos < vec_size) {
        unsigned token_idx;

        /* Set op code */
        vector->op_vector[pos].op_code = H5FL_REG_OP_FREE;

        token_idx = get_active_h5fl_reg_token(tokens, tokens_wrapped ? num_tokens : curr_alloc_token);
        vector->op_vector[pos].token = &tokens[token_idx];

        /* Mark token as free */
        tokens[token_idx].val = NULL;

        /* Decrement # of active tokens */
        num_active_tokens--;

        pos++;
    }

    assert(0 == num_active_tokens);
}

static void
init_h5fl_fac_vector(unsigned vec_size, h5fl_fac_test_vector *vector, unsigned num_tokens,
                     h5fl_fac_test_token *tokens)
{
    unsigned num_active_tokens = 0; /* # of active tokens at any position in the test vector execution */
    unsigned curr_alloc_token;      /* Current position for allocating tokens */
    unsigned pos;                   /* Current position in the test vector */
    bool     tokens_wrapped = false;

    /* Allocate the test vector */
    vector->vec_size  = vec_size;
    vector->op_vector = calloc(vec_size, sizeof(h5fl_fac_test_op));
    CHECK_PTR(vector->op_vector, "calloc");

    /* Fiil the test vector, leaving room to free active tokens */
    pos              = 0;
    curr_alloc_token = 0;
    while (pos < (vec_size - num_active_tokens)) {
        h5fl_fac_test_op_code op_code;

        /* Check for active tokens */
        /* (Also must have enough room for both alloc & free operations) */
        if (0 == num_active_tokens && pos < (vec_size - 2))
            op_code = get_new_h5fl_fac_op(h5fl_fac_empty_ops_odds);
        else {
            /* Don't create new tokens when there won't be enough room in the
             * vector for both the alloc & free operations.
             */
            if (pos > ((vec_size - num_active_tokens) - 2))
                op_code = get_new_h5fl_fac_op(h5fl_fac_vec_almost_full_ops_odds);
            /* Don't create new tokens when the token array is full */
            else if (num_tokens == num_active_tokens)
                op_code = get_new_h5fl_fac_op(h5fl_fac_full_ops_odds);
            else
                op_code = get_new_h5fl_fac_op(h5fl_fac_all_ops_odds);
        }

        /* Set op code */
        vector->op_vector[pos].op_code = op_code;

        /* Set up specific parameters for each op code */
        switch (op_code) {
            case H5FL_FAC_OP_MALLOC:
            case H5FL_FAC_OP_CALLOC: {
                unsigned prev_alloc_token = curr_alloc_token;
                unsigned type_idx;
                unsigned new_token;

                /* RNG type to allocate */
                type_idx  = (unsigned)h5_local_rand() % (unsigned)NELMTS(h5fl_fac_test_types);
                new_token = get_new_h5fl_fac_token(tokens, &curr_alloc_token);
                vector->op_vector[pos].token          = &tokens[new_token];
                vector->op_vector[pos].param.type_idx = type_idx;

                /* Mark token as used */
                tokens[new_token].val = (void *)(~(uintptr_t)NULL);

                /* Increment # of active tokens */
                num_active_tokens++;

                /* Check for tokens wrapping */
                if (curr_alloc_token < prev_alloc_token)
                    tokens_wrapped = true;
            } break;

            case H5FL_FAC_OP_ZERO:
            case H5FL_FAC_OP_FILL1:
            case H5FL_FAC_OP_FILL2:
            case H5FL_FAC_OP_FILL3:
            case H5FL_FAC_OP_FREE: {
                unsigned token_idx;

                token_idx = get_active_h5fl_fac_token(tokens, tokens_wrapped ? num_tokens : curr_alloc_token);
                vector->op_vector[pos].token = &tokens[token_idx];

                if (H5FL_FAC_OP_FREE == op_code) {
                    /* Mark token as free */
                    tokens[token_idx].val = NULL;

                    /* Decrement # of active tokens */
                    num_active_tokens--;
                }
            } break;

            default:
                assert(0 && "Invalid op code");
                abort();
        }

        pos++;
    }

    /* Fill remainder of test vector with free operations */
    while (pos < vec_size) {
        unsigned token_idx;

        /* Set op code */
        vector->op_vector[pos].op_code = H5FL_FAC_OP_FREE;

        token_idx = get_active_h5fl_fac_token(tokens, tokens_wrapped ? num_tokens : curr_alloc_token);
        vector->op_vector[pos].token = &tokens[token_idx];

        /* Mark token as free */
        tokens[token_idx].val = NULL;

        /* Decrement # of active tokens */
        num_active_tokens--;

        pos++;
    }

    assert(0 == num_active_tokens);
}

static void
init_h5fl_blk_vector(unsigned vec_size, h5fl_blk_test_vector *vector, unsigned num_tokens,
                     h5fl_blk_test_token *tokens)
{
    unsigned num_active_tokens = 0; /* # of active tokens at any position in the test vector execution */
    unsigned curr_alloc_token;      /* Current position for allocating tokens */
    unsigned pos;                   /* Current position in the test vector */
    bool     tokens_wrapped = false;

    /* Allocate the test vector */
    vector->vec_size  = vec_size;
    vector->op_vector = calloc(vec_size, sizeof(h5fl_blk_test_op));
    CHECK_PTR(vector->op_vector, "calloc");

    /* Fiil the test vector, leaving room to free active tokens */
    pos              = 0;
    curr_alloc_token = 0;
    while (pos < (vec_size - num_active_tokens)) {
        h5fl_blk_test_op_code op_code;

        /* Check for active tokens */
        /* (Also must have enough room for both alloc & free operations) */
        if (0 == num_active_tokens && pos < (vec_size - 2))
            op_code = get_new_h5fl_blk_op(h5fl_blk_empty_ops_odds);
        else {
            /* Don't create new tokens when there won't be enough room in the
             * vector for both the alloc & free operations.
             */
            if (pos > ((vec_size - num_active_tokens) - 2))
                op_code = get_new_h5fl_blk_op(h5fl_blk_vec_almost_full_ops_odds);
            /* Don't create new tokens when the token array is full */
            else if (num_tokens == num_active_tokens)
                op_code = get_new_h5fl_blk_op(h5fl_blk_full_ops_odds);
            else
                op_code = get_new_h5fl_blk_op(h5fl_blk_all_ops_odds);
        }

        /* Set op code */
        vector->op_vector[pos].op_code = op_code;

        /* Set up specific parameters for each op code */
        switch (op_code) {
            case H5FL_BLK_OP_MALLOC:
            case H5FL_BLK_OP_CALLOC: {
                unsigned prev_alloc_token = curr_alloc_token;
                unsigned type_idx;
                unsigned new_token;

                /* RNG type to allocate */

                type_idx  = (unsigned)h5_local_rand() % (unsigned)NELMTS(h5fl_blk_test_types);
                new_token = get_new_h5fl_blk_token(tokens, &curr_alloc_token);
                vector->op_vector[pos].token          = &tokens[new_token];
                vector->op_vector[pos].param.type_idx = type_idx;

                /* Mark token as used */
                tokens[new_token].val = (void *)(~(uintptr_t)NULL);

                /* Increment # of active tokens */
                num_active_tokens++;

                /* Check for tokens wrapping */
                if (curr_alloc_token < prev_alloc_token)
                    tokens_wrapped = true;
            } break;

            case H5FL_BLK_OP_REALLOC: {
                unsigned token_idx;

                token_idx = get_active_h5fl_blk_token(tokens, tokens_wrapped ? num_tokens : curr_alloc_token);
                vector->op_vector[pos].token = &tokens[token_idx];
                vector->op_vector[pos].param.size_shift = ((unsigned)h5_local_rand() & 0x10) ? 1 : -1;
            } break;

            case H5FL_BLK_OP_ZERO:
            case H5FL_BLK_OP_FILL1:
            case H5FL_BLK_OP_FILL2:
            case H5FL_BLK_OP_FILL3:
            case H5FL_BLK_OP_FREE: {
                unsigned token_idx;

                token_idx = get_active_h5fl_blk_token(tokens, tokens_wrapped ? num_tokens : curr_alloc_token);
                vector->op_vector[pos].token = &tokens[token_idx];

                if (H5FL_BLK_OP_FREE == op_code) {
                    /* Mark token as free */
                    tokens[token_idx].val = NULL;

                    /* Decrement # of active tokens */
                    num_active_tokens--;
                }
            } break;

            default:
                assert(0 && "Invalid op code");
                abort();
        }

        pos++;
    }

    /* Fill remainder of test vector with free operations */
    while (pos < vec_size) {
        unsigned token_idx;

        /* Set op code */
        vector->op_vector[pos].op_code = H5FL_BLK_OP_FREE;

        token_idx = get_active_h5fl_blk_token(tokens, tokens_wrapped ? num_tokens : curr_alloc_token);
        vector->op_vector[pos].token = &tokens[token_idx];

        /* Mark token as free */
        tokens[token_idx].val = NULL;

        /* Decrement # of active tokens */
        num_active_tokens--;

        pos++;
    }

    assert(0 == num_active_tokens);
}

static inline unsigned
validate_h5fl_reg_token(const h5fl_reg_test_token *token)
{
    int v;

    switch (token->state) {
        case H5FL_REG_ST_UNINIT:
            break;

        case H5FL_REG_ST_ZERO:
            v = memcmp(token->val, h5fl_reg_test_types[token->type_idx].zero,
                       h5fl_reg_test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "H5FL_REG_ST_ZERO");
            if (0 != v)
                return (1);
            break;

        case H5FL_REG_ST_FILL1:
            v = memcmp(token->val, h5fl_reg_test_types[token->type_idx].fill1,
                       h5fl_reg_test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "H5FL_REG_ST_FILL1");
            if (0 != v)
                return (1);
            break;

        case H5FL_REG_ST_FILL2:
            v = memcmp(token->val, h5fl_reg_test_types[token->type_idx].fill2,
                       h5fl_reg_test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "H5FL_REG_ST_FILL2");
            if (0 != v)
                return (1);
            break;

        case H5FL_REG_ST_FILL3:
            v = memcmp(token->val, h5fl_reg_test_types[token->type_idx].fill3,
                       h5fl_reg_test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "H5FL_REG_ST_FILL3");
            if (0 != v)
                return (1);
            break;

        default:
            assert(0 && "Invalid state for token");
            abort();
    }

    return (0);
}

static inline unsigned
validate_h5fl_fac_token(const h5fl_fac_test_token *token)
{
    int v;

    switch (token->state) {
        case H5FL_FAC_ST_UNINIT:
            break;

        case H5FL_FAC_ST_ZERO:
            v = memcmp(token->val, h5fl_fac_test_types[token->type_idx].zero,
                       h5fl_fac_test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "H5FL_FAC_ST_ZERO");
            if (0 != v)
                return (1);
            break;

        case H5FL_FAC_ST_FILL1:
            v = memcmp(token->val, h5fl_fac_test_types[token->type_idx].fill1,
                       h5fl_fac_test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "H5FL_FAC_ST_FILL1");
            if (0 != v)
                return (1);
            break;

        case H5FL_FAC_ST_FILL2:
            v = memcmp(token->val, h5fl_fac_test_types[token->type_idx].fill2,
                       h5fl_fac_test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "H5FL_FAC_ST_FILL2");
            if (0 != v)
                return (1);
            break;

        case H5FL_FAC_ST_FILL3:
            v = memcmp(token->val, h5fl_fac_test_types[token->type_idx].fill3,
                       h5fl_fac_test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "H5FL_FAC_ST_FILL3");
            if (0 != v)
                return (1);
            break;

        default:
            assert(0 && "Invalid state for token");
            abort();
    }

    return (0);
}

static inline unsigned
validate_h5fl_blk_token(const h5fl_blk_test_token *token)
{
    unsigned u;

    switch (token->state) {
        case H5FL_BLK_ST_UNINIT:
            break;

        case H5FL_BLK_ST_ZERO:
            for (u = 0; u < token->curr_size; u++) {
                VERIFY(token->val[u], 0, "H5FL_BLK_ST_ZERO");
                if (0 != token->val[u])
                    return (1);
            }
            break;

        case H5FL_BLK_ST_FILL1:
            for (u = 0; u < token->curr_size; u++) {
                VERIFY(token->val[u], 1, "H5FL_BLK_ST_FILL1");
                if (1 != token->val[u])
                    return (1);
            }
            break;

        case H5FL_BLK_ST_FILL2:
            for (u = 0; u < token->curr_size; u++) {
                VERIFY(token->val[u], 2, "H5FL_BLK_ST_FILL2");
                if (2 != token->val[u])
                    return (1);
            }
            break;

        case H5FL_BLK_ST_FILL3:
            for (u = 0; u < token->curr_size; u++) {
                VERIFY(token->val[u], 3, "H5FL_BLK_ST_FILL3");
                if (3 != token->val[u])
                    return (1);
            }
            break;

        default:
            assert(0 && "Invalid state for token");
            abort();
    }

    return (0);
}

static unsigned
run_h5fl_reg_vector(h5fl_reg_test_vector *vector)
{
    /* Execute test vector */
    for (unsigned u = 0; u < vector->vec_size; u++) {
        switch (vector->op_vector[u].op_code) {
            case H5FL_REG_OP_MALLOC:
                vector->op_vector[u].token->val =
                    H5FL_reg_malloc(h5fl_reg_test_types[vector->op_vector[u].param.type_idx].free_list);
                CHECK_PTR(vector->op_vector[u].token->val, "H5FL_reg_malloc");
                if (NULL == vector->op_vector[u].token->val)
                    return (1);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state    = H5FL_REG_ST_UNINIT;
                break;

            case H5FL_REG_OP_CALLOC:
                vector->op_vector[u].token->val =
                    H5FL_reg_calloc(h5fl_reg_test_types[vector->op_vector[u].param.type_idx].free_list);
                CHECK_PTR(vector->op_vector[u].token->val, "H5FL_reg_calloc");
                if (NULL == vector->op_vector[u].token->val)
                    return (1);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state    = H5FL_REG_ST_ZERO;
                break;

            case H5FL_REG_OP_ZERO:
                if (H5FL_REG_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_reg_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_REG_ST_ZERO != vector->op_vector[u].token->state) {
                    memset(vector->op_vector[u].token->val, 0,
                           h5fl_reg_test_types[vector->op_vector[u].token->type_idx].elmt_size);
                    vector->op_vector[u].token->state = H5FL_REG_ST_ZERO;
                }
                break;

            case H5FL_REG_OP_FILL1:
                if (H5FL_REG_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_reg_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_REG_ST_FILL1 != vector->op_vector[u].token->state) {
                    memcpy(vector->op_vector[u].token->val,
                           h5fl_reg_test_types[vector->op_vector[u].token->type_idx].fill1,
                           h5fl_reg_test_types[vector->op_vector[u].token->type_idx].elmt_size);
                    vector->op_vector[u].token->state = H5FL_REG_ST_FILL1;
                }
                break;

            case H5FL_REG_OP_FILL2:
                if (H5FL_REG_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_reg_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_REG_ST_FILL2 != vector->op_vector[u].token->state) {
                    memcpy(vector->op_vector[u].token->val,
                           h5fl_reg_test_types[vector->op_vector[u].token->type_idx].fill2,
                           h5fl_reg_test_types[vector->op_vector[u].token->type_idx].elmt_size);
                    vector->op_vector[u].token->state = H5FL_REG_ST_FILL2;
                }
                break;

            case H5FL_REG_OP_FILL3:
                if (H5FL_REG_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_reg_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_REG_ST_FILL3 != vector->op_vector[u].token->state) {
                    memcpy(vector->op_vector[u].token->val,
                           h5fl_reg_test_types[vector->op_vector[u].token->type_idx].fill3,
                           h5fl_reg_test_types[vector->op_vector[u].token->type_idx].elmt_size);
                    vector->op_vector[u].token->state = H5FL_REG_ST_FILL3;
                }
                break;

            case H5FL_REG_OP_FREE:
                if (H5FL_REG_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_reg_token(vector->op_vector[u].token))
                        return (1);
                H5FL_reg_free(h5fl_reg_test_types[vector->op_vector[u].token->type_idx].free_list,
                              vector->op_vector[u].token->val);
                vector->op_vector[u].token->val = NULL;
                break;

            default:
                assert(0 && "Invalid op code");
                abort();
        }
    }

    return (0);
}

static unsigned
run_h5fl_fac_vector(h5fl_fac_test_vector *vector)
{
    /* Execute test vector */
    for (unsigned u = 0; u < vector->vec_size; u++) {
        switch (vector->op_vector[u].op_code) {
            case H5FL_FAC_OP_MALLOC:
                vector->op_vector[u].token->val =
                    H5FL_fac_malloc(h5fl_fac_test_types[vector->op_vector[u].param.type_idx].free_list);
                CHECK_PTR(vector->op_vector[u].token->val, "H5FL_fac_malloc");
                if (NULL == vector->op_vector[u].token->val)
                    return (1);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state    = H5FL_FAC_ST_UNINIT;
                break;

            case H5FL_FAC_OP_CALLOC:
                vector->op_vector[u].token->val =
                    H5FL_fac_calloc(h5fl_fac_test_types[vector->op_vector[u].param.type_idx].free_list);
                CHECK_PTR(vector->op_vector[u].token->val, "H5FL_fac_calloc");
                if (NULL == vector->op_vector[u].token->val)
                    return (1);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state    = H5FL_FAC_ST_ZERO;
                break;

            case H5FL_FAC_OP_ZERO:
                if (H5FL_FAC_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_fac_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_FAC_ST_ZERO != vector->op_vector[u].token->state) {
                    memset(vector->op_vector[u].token->val, 0,
                           h5fl_fac_test_types[vector->op_vector[u].token->type_idx].elmt_size);
                    vector->op_vector[u].token->state = H5FL_FAC_ST_ZERO;
                }
                break;

            case H5FL_FAC_OP_FILL1:
                if (H5FL_FAC_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_fac_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_FAC_ST_FILL1 != vector->op_vector[u].token->state) {
                    memcpy(vector->op_vector[u].token->val,
                           h5fl_fac_test_types[vector->op_vector[u].token->type_idx].fill1,
                           h5fl_fac_test_types[vector->op_vector[u].token->type_idx].elmt_size);
                    vector->op_vector[u].token->state = H5FL_FAC_ST_FILL1;
                }
                break;

            case H5FL_FAC_OP_FILL2:
                if (H5FL_FAC_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_fac_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_FAC_ST_FILL2 != vector->op_vector[u].token->state) {
                    memcpy(vector->op_vector[u].token->val,
                           h5fl_fac_test_types[vector->op_vector[u].token->type_idx].fill2,
                           h5fl_fac_test_types[vector->op_vector[u].token->type_idx].elmt_size);
                    vector->op_vector[u].token->state = H5FL_FAC_ST_FILL2;
                }
                break;

            case H5FL_FAC_OP_FILL3:
                if (H5FL_FAC_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_fac_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_FAC_ST_FILL3 != vector->op_vector[u].token->state) {
                    memcpy(vector->op_vector[u].token->val,
                           h5fl_fac_test_types[vector->op_vector[u].token->type_idx].fill3,
                           h5fl_fac_test_types[vector->op_vector[u].token->type_idx].elmt_size);
                    vector->op_vector[u].token->state = H5FL_FAC_ST_FILL3;
                }
                break;

            case H5FL_FAC_OP_FREE:
                if (H5FL_FAC_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_fac_token(vector->op_vector[u].token))
                        return (1);
                H5FL_fac_free(h5fl_fac_test_types[vector->op_vector[u].token->type_idx].free_list,
                              vector->op_vector[u].token->val);
                vector->op_vector[u].token->val = NULL;
                break;

            default:
                assert(0 && "Invalid op code");
                abort();
        }
    }

    return (0);
}

static void
fill_h5fl_blk_vector(h5fl_blk_test_token *token)
{
    switch (token->state) {
        case H5FL_BLK_ST_UNINIT:
            break;

        case H5FL_BLK_ST_ZERO:
            memset(token->val, 0, token->curr_size);
            break;

        case H5FL_BLK_ST_FILL1:
            memset(token->val, 1, token->curr_size);
            break;

        case H5FL_BLK_ST_FILL2:
            memset(token->val, 2, token->curr_size);
            break;

        case H5FL_BLK_ST_FILL3:
            memset(token->val, 3, token->curr_size);
            break;

        default:
            assert(0 && "Invalid state for token");
            abort();
    }
}

static unsigned
run_h5fl_blk_vector(h5fl_blk_test_vector *vector)
{
    /* Execute test vector */
    for (unsigned u = 0; u < vector->vec_size; u++) {
        switch (vector->op_vector[u].op_code) {
            case H5FL_BLK_OP_MALLOC:
                vector->op_vector[u].token->val =
                    H5FL_blk_malloc(h5fl_blk_test_types[vector->op_vector[u].param.type_idx].free_list, h5fl_blk_test_types[vector->op_vector[u].param.type_idx].initial_size);
                CHECK_PTR(vector->op_vector[u].token->val, "H5FL_blk_malloc");
                if (NULL == vector->op_vector[u].token->val)
                    return (1);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->curr_size = h5fl_blk_test_types[vector->op_vector[u].param.type_idx].initial_size;
                vector->op_vector[u].token->size_shift    = 0;
                vector->op_vector[u].token->state    = H5FL_BLK_ST_UNINIT;
                break;

            case H5FL_BLK_OP_CALLOC:
                vector->op_vector[u].token->val =
                    H5FL_blk_calloc(h5fl_blk_test_types[vector->op_vector[u].param.type_idx].free_list, h5fl_blk_test_types[vector->op_vector[u].param.type_idx].initial_size);
                CHECK_PTR(vector->op_vector[u].token->val, "H5FL_blk_calloc");
                if (NULL == vector->op_vector[u].token->val)
                    return (1);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->curr_size = h5fl_blk_test_types[vector->op_vector[u].param.type_idx].initial_size;
                vector->op_vector[u].token->size_shift    = 0;
                vector->op_vector[u].token->state    = H5FL_BLK_ST_ZERO;
                break;

            case H5FL_BLK_OP_REALLOC: {
                size_t new_size;
                size_t prev_size;

                /* Choose new size for token's buffer */
                vector->op_vector[u].token->size_shift += vector->op_vector[u].param.size_shift;
                if (vector->op_vector[u].token->size_shift > 0)
                    new_size = h5fl_blk_test_types[vector->op_vector[u].token->type_idx].initial_size << vector->op_vector[u].token->size_shift;
                else if (vector->op_vector[u].token->size_shift < 0) {
                    new_size = h5fl_blk_test_types[vector->op_vector[u].token->type_idx].initial_size >> (-vector->op_vector[u].token->size_shift);
                    if (0 == new_size)
                        new_size = 1;
                }
                else
                    new_size = h5fl_blk_test_types[vector->op_vector[u].token->type_idx].initial_size;

                /* Validate current buffer */
                if (H5FL_BLK_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_blk_token(vector->op_vector[u].token))
                        return (1);

                /* Reallocate buffer */
                vector->op_vector[u].token->val =
                    H5FL_blk_realloc(h5fl_blk_test_types[vector->op_vector[u].token->type_idx].free_list, vector->op_vector[u].token->val, new_size);
                CHECK_PTR(vector->op_vector[u].token->val, "H5FL_blk_realloc");
                if (NULL == vector->op_vector[u].token->val)
                    return (1);

                /* Update size & value for buffer */
                prev_size = vector->op_vector[u].token->curr_size;
                vector->op_vector[u].token->curr_size = new_size;
                if (new_size > prev_size)
                    fill_h5fl_blk_vector(vector->op_vector[u].token);
                }
                break;

            case H5FL_BLK_OP_ZERO:
                if (H5FL_BLK_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_blk_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_BLK_ST_ZERO != vector->op_vector[u].token->state) {
                    vector->op_vector[u].token->state = H5FL_BLK_ST_ZERO;
                    memset(vector->op_vector[u].token->val, 0, vector->op_vector[u].token->curr_size);
                }
                break;

            case H5FL_BLK_OP_FILL1:
                if (H5FL_BLK_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_blk_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_BLK_ST_FILL1 != vector->op_vector[u].token->state) {
                    vector->op_vector[u].token->state = H5FL_BLK_ST_FILL1;
                    memset(vector->op_vector[u].token->val, 1, vector->op_vector[u].token->curr_size);
                }
                break;

            case H5FL_BLK_OP_FILL2:
                if (H5FL_BLK_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_blk_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_BLK_ST_FILL2 != vector->op_vector[u].token->state) {
                    vector->op_vector[u].token->state = H5FL_BLK_ST_FILL2;
                    memset(vector->op_vector[u].token->val, 2, vector->op_vector[u].token->curr_size);
                }
                break;

            case H5FL_BLK_OP_FILL3:
                if (H5FL_BLK_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_blk_token(vector->op_vector[u].token))
                        return (1);
                if (H5FL_BLK_ST_FILL3 != vector->op_vector[u].token->state) {
                    vector->op_vector[u].token->state = H5FL_BLK_ST_FILL3;
                    memset(vector->op_vector[u].token->val, 3, vector->op_vector[u].token->curr_size);
                }
                break;

            case H5FL_BLK_OP_FREE:
                if (H5FL_BLK_ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_h5fl_blk_token(vector->op_vector[u].token))
                        return (1);
                H5FL_blk_free(h5fl_blk_test_types[vector->op_vector[u].token->type_idx].free_list,
                              vector->op_vector[u].token->val);
                vector->op_vector[u].token->val = NULL;
                break;

            default:
                assert(0 && "Invalid op code");
                abort();
        }
    }

    return (0);
}

static H5TS_THREAD_RETURN_TYPE
thread_h5fl_reg(void *_vectors)
{
    h5fl_reg_test_vector   *vectors   = (h5fl_reg_test_vector *)_vectors;
    unsigned                errors    = 0;
    H5TS_THREAD_RETURN_TYPE ret_value = (H5TS_THREAD_RETURN_TYPE)0;

    /* Randomly run a number of vectors */
    for (unsigned u = 0; u < NUM_ITERS_PER_THREAD; u++) {
        unsigned rng = (unsigned)h5_local_rand() % NUM_VECTORS;

        /* Run the test vector */
        errors += run_h5fl_reg_vector(&vectors[rng]);
    }

    if (errors > 0)
        ret_value = (H5TS_THREAD_RETURN_TYPE)1;

    return ret_value;
}

/* 'regular' H5FL test vectors */
static h5fl_reg_test_vector *h5fl_reg_vectors[NUM_THREADS];

static void
test_h5fl_reg(void)
{
    h5fl_reg_test_token *tokens[NUM_THREADS]; /* Test tokens */
    H5TS_thread_t        threads[NUM_THREADS];
    herr_t               result;

    /* Output message about test being performed */
    MESSAGE(7, ("Testing 'regular' H5FL operations\n"));

    /* Initialize the zero values for each type */
    for (unsigned u = 0; u < (unsigned)NELMTS(h5fl_reg_test_types); u++) {
        h5fl_reg_test_types[u].zero = calloc(1, h5fl_reg_test_types[u].elmt_size);
        CHECK_PTR(h5fl_reg_test_types[u].zero, "calloc");
    }

    /* Initialize the fill values for each type to RNG values */
    for (unsigned u = 0; u < (unsigned)NELMTS(h5fl_reg_test_types); u++) {
        h5fl_reg_test_types[u].fill1 = malloc(h5fl_reg_test_types[u].elmt_size);
        CHECK_PTR(h5fl_reg_test_types[u].fill1, "malloc");
        for (unsigned v = 0; v < h5fl_reg_test_types[u].elmt_size; v++)
            h5fl_reg_test_types[u].fill1[v] = (unsigned char)h5_local_rand();

        h5fl_reg_test_types[u].fill2 = malloc(h5fl_reg_test_types[u].elmt_size);
        CHECK_PTR(h5fl_reg_test_types[u].fill2, "malloc");
        for (unsigned v = 0; v < h5fl_reg_test_types[u].elmt_size; v++)
            h5fl_reg_test_types[u].fill2[v] = (unsigned char)h5_local_rand();

        h5fl_reg_test_types[u].fill3 = malloc(h5fl_reg_test_types[u].elmt_size);
        CHECK_PTR(h5fl_reg_test_types[u].fill3, "malloc");
        for (unsigned v = 0; v < h5fl_reg_test_types[u].elmt_size; v++)
            h5fl_reg_test_types[u].fill3[v] = (unsigned char)h5_local_rand();
    }

    /* Initialize the test vectors */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        /* Allocate the test tokens */
        tokens[u] = calloc(MAX_TOKENS, sizeof(h5fl_reg_test_token));
        CHECK_PTR(tokens[u], "calloc");

        /* Initialize the test vectors */
        h5fl_reg_vectors[u] = calloc(NUM_VECTORS, sizeof(h5fl_reg_test_vector));
        CHECK_PTR(h5fl_reg_vectors[u], "calloc");

        for (unsigned v = 0; v < NUM_VECTORS; v++)
            init_h5fl_reg_vector(NUM_TEST_OPS, &h5fl_reg_vectors[u][v], MAX_TOKENS, tokens[u]);
    }

    /* Create threads and have them execute the vector */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        result = H5TS_thread_create(&threads[u], thread_h5fl_reg, h5fl_reg_vectors[u]);
        CHECK_I(result, "H5TS_thread_create");
    }

    /* Wait for all threads */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        H5TS_THREAD_RETURN_TYPE thread_ret = (H5TS_THREAD_RETURN_TYPE)0;

        /* Join thread */
        result = H5TS_thread_join(threads[u], &thread_ret);
        CHECK_I(result, "H5TS_thread_join");

        /* Verify no errors from thread */
        VERIFY(thread_ret, (H5TS_THREAD_RETURN_TYPE)0, "error in thread");
    }

    /* Free test vectors & tokens */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        free(tokens[u]);
        for (unsigned v = 0; v < NUM_VECTORS; v++)
            free(h5fl_reg_vectors[u][v].op_vector);
        free(h5fl_reg_vectors[u]);
    }

    /* Free the zero fill values for each type */
    for (unsigned u = 0; u < (unsigned)NELMTS(h5fl_reg_test_types); u++) {
        free(h5fl_reg_test_types[u].zero);
        free(h5fl_reg_test_types[u].fill1);
        free(h5fl_reg_test_types[u].fill2);
        free(h5fl_reg_test_types[u].fill3);
    }
}

static H5TS_THREAD_RETURN_TYPE
thread_h5fl_fac(void *_vectors)
{
    h5fl_fac_test_vector   *vectors   = (h5fl_fac_test_vector *)_vectors;
    unsigned                errors    = 0;
    H5TS_THREAD_RETURN_TYPE ret_value = (H5TS_THREAD_RETURN_TYPE)0;

    /* Randomly run a number of vectors */
    for (unsigned u = 0; u < NUM_ITERS_PER_THREAD; u++) {
        unsigned rng = (unsigned)h5_local_rand() % NUM_VECTORS;

        /* Run the test vector */
        errors += run_h5fl_fac_vector(&vectors[rng]);
    }

    if (errors > 0)
        ret_value = (H5TS_THREAD_RETURN_TYPE)1;

    return ret_value;
}

/* 'factory' H5FL test vectors */
static h5fl_fac_test_vector *h5fl_fac_vectors[NUM_THREADS];

static void
test_h5fl_fac(void)
{
    h5fl_fac_test_token *tokens[NUM_THREADS]; /* Test tokens */
    H5TS_thread_t        threads[NUM_THREADS];
    herr_t               result;

    /* Output message about test being performed */
    MESSAGE(7, ("Testing 'factory' H5FL operations\n"));

    /* Initialize the free list factory for each block size */
    for (unsigned u = 0; u < (unsigned)NELMTS(h5fl_fac_test_types); u++) {
        h5fl_fac_test_types[u].free_list = H5FL_fac_init(h5fl_fac_test_types[u].elmt_size);
        CHECK_PTR(h5fl_fac_test_types[u].free_list, "H5FL_fac_init");
    }

    /* Initialize the zero values for each type */
    for (unsigned u = 0; u < (unsigned)NELMTS(h5fl_fac_test_types); u++) {
        h5fl_fac_test_types[u].zero = calloc(1, h5fl_fac_test_types[u].elmt_size);
        CHECK_PTR(h5fl_fac_test_types[u].zero, "calloc");
    }

    /* Initialize the fill values for each type to RNG values */
    for (unsigned u = 0; u < (unsigned)NELMTS(h5fl_fac_test_types); u++) {
        h5fl_fac_test_types[u].fill1 = malloc(h5fl_fac_test_types[u].elmt_size);
        CHECK_PTR(h5fl_fac_test_types[u].fill1, "malloc");
        for (unsigned v = 0; v < h5fl_fac_test_types[u].elmt_size; v++)
            h5fl_fac_test_types[u].fill1[v] = (unsigned char)h5_local_rand();

        h5fl_fac_test_types[u].fill2 = malloc(h5fl_fac_test_types[u].elmt_size);
        CHECK_PTR(h5fl_fac_test_types[u].fill2, "malloc");
        for (unsigned v = 0; v < h5fl_fac_test_types[u].elmt_size; v++)
            h5fl_fac_test_types[u].fill2[v] = (unsigned char)h5_local_rand();

        h5fl_fac_test_types[u].fill3 = malloc(h5fl_fac_test_types[u].elmt_size);
        CHECK_PTR(h5fl_fac_test_types[u].fill3, "malloc");
        for (unsigned v = 0; v < h5fl_fac_test_types[u].elmt_size; v++)
            h5fl_fac_test_types[u].fill3[v] = (unsigned char)h5_local_rand();
    }

    /* Initialize the test vectors */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        /* Allocate the test tokens */
        tokens[u] = calloc(MAX_TOKENS, sizeof(h5fl_fac_test_token));
        CHECK_PTR(tokens[u], "calloc");

        /* Initialize the test vectors */
        h5fl_fac_vectors[u] = calloc(NUM_VECTORS, sizeof(h5fl_fac_test_vector));
        CHECK_PTR(h5fl_fac_vectors[u], "calloc");

        for (unsigned v = 0; v < NUM_VECTORS; v++)
            init_h5fl_fac_vector(NUM_TEST_OPS, &h5fl_fac_vectors[u][v], MAX_TOKENS, tokens[u]);
    }

    /* Create threads and have them execute the vector */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        result = H5TS_thread_create(&threads[u], thread_h5fl_fac, h5fl_fac_vectors[u]);
        CHECK_I(result, "H5TS_thread_create");
    }

    /* Wait for all threads */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        H5TS_THREAD_RETURN_TYPE thread_ret = (H5TS_THREAD_RETURN_TYPE)0;

        /* Join thread */
        result = H5TS_thread_join(threads[u], &thread_ret);
        CHECK_I(result, "H5TS_thread_join");

        /* Verify no errors from thread */
        VERIFY(thread_ret, (H5TS_THREAD_RETURN_TYPE)0, "error in thread");
    }

    /* Free test vectors & tokens */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        free(tokens[u]);
        for (unsigned v = 0; v < NUM_VECTORS; v++)
            free(h5fl_fac_vectors[u][v].op_vector);
        free(h5fl_fac_vectors[u]);
    }

    /* Release the free list factory for each block size */
    for (unsigned u = 0; u < (unsigned)NELMTS(h5fl_fac_test_types); u++) {
        result = H5FL_fac_term(h5fl_fac_test_types[u].free_list);
        CHECK_I(result, "H5FL_fac_term");
    }

    /* Free the zero fill values for each type */
    for (unsigned u = 0; u < (unsigned)NELMTS(h5fl_fac_test_types); u++) {
        free(h5fl_fac_test_types[u].zero);
        free(h5fl_fac_test_types[u].fill1);
        free(h5fl_fac_test_types[u].fill2);
        free(h5fl_fac_test_types[u].fill3);
    }
}

static H5TS_THREAD_RETURN_TYPE
thread_h5fl_blk(void *_vectors)
{
    h5fl_blk_test_vector   *vectors   = (h5fl_blk_test_vector *)_vectors;
    unsigned                errors    = 0;
    H5TS_THREAD_RETURN_TYPE ret_value = (H5TS_THREAD_RETURN_TYPE)0;

    /* Randomly run a number of vectors */
    for (unsigned u = 0; u < NUM_ITERS_PER_THREAD; u++) {
        unsigned rng = (unsigned)h5_local_rand() % NUM_VECTORS;

        /* Run the test vector */
        errors += run_h5fl_blk_vector(&vectors[rng]);
    }

    if (errors > 0)
        ret_value = (H5TS_THREAD_RETURN_TYPE)1;

    return ret_value;
}

/* 'block' H5FL test vectors */
static h5fl_blk_test_vector *h5fl_blk_vectors[NUM_THREADS];

static void
test_h5fl_blk(void)
{
    h5fl_blk_test_token *tokens[NUM_THREADS]; /* Test tokens */
    H5TS_thread_t        threads[NUM_THREADS];
    herr_t               result;

    /* Output message about test being performed */
    MESSAGE(7, ("Testing 'block' H5FL operations\n"));

    /* Initialize the test vectors */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        /* Allocate the test tokens */
        tokens[u] = calloc(MAX_TOKENS, sizeof(h5fl_blk_test_token));
        CHECK_PTR(tokens[u], "calloc");

        /* Initialize the test vectors */
        h5fl_blk_vectors[u] = calloc(NUM_VECTORS, sizeof(h5fl_blk_test_vector));
        CHECK_PTR(h5fl_blk_vectors[u], "calloc");

        for (unsigned v = 0; v < NUM_VECTORS; v++)
            init_h5fl_blk_vector(NUM_TEST_OPS, &h5fl_blk_vectors[u][v], MAX_TOKENS, tokens[u]);
    }

    /* Create threads and have them execute the vector */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        result = H5TS_thread_create(&threads[u], thread_h5fl_blk, h5fl_blk_vectors[u]);
        CHECK_I(result, "H5TS_thread_create");
    }

    /* Wait for all threads */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        H5TS_THREAD_RETURN_TYPE thread_ret = (H5TS_THREAD_RETURN_TYPE)0;

        /* Join thread */
        result = H5TS_thread_join(threads[u], &thread_ret);
        CHECK_I(result, "H5TS_thread_join");

        /* Verify no errors from thread */
        VERIFY(thread_ret, (H5TS_THREAD_RETURN_TYPE)0, "error in thread");
    }

    /* Free test vectors & tokens */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        free(tokens[u]);
        for (unsigned v = 0; v < NUM_VECTORS; v++)
            free(h5fl_blk_vectors[u][v].op_vector);
        free(h5fl_blk_vectors[u]);
    }
}

/*
 **********************************************************************
 * Test H5FL package
 **********************************************************************
 */
void
tts_h5fl(const void H5_ATTR_UNUSED *params)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing threadsafe H5FL operations\n"));

    /* Set up local RNG */
    h5_setup_local_rand("tts_h5fl", 0);

    /* Run tests */
    test_h5fl_reg();
    test_h5fl_fac();
    test_h5fl_blk();
} /* end tts_h5fl() */

#endif /*H5_HAVE_THREADS*/
