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
} test_type_1;

typedef struct {
    unsigned char buf[64];
} test_type_2;

typedef struct {
    unsigned char buf[256];
} test_type_3;

typedef struct {
    unsigned char buf[1];
} test_type_4;

typedef struct {
    unsigned char buf[2];
} test_type_5;

typedef struct {
    unsigned char buf[3];
} test_type_6;

typedef struct {
    unsigned char buf[5];
} test_type_7;

typedef struct {
    unsigned char buf[8];
} test_type_8;

typedef struct {
    unsigned char buf[13];
} test_type_9;

typedef struct {
    unsigned char buf[21];
} test_type_10;

typedef struct {
    unsigned char buf[34];
} test_type_11;

typedef struct {
    unsigned char buf[55];
} test_type_12;

/* Free lists of the various types */
H5FL_DEFINE_STATIC(test_type_1);
H5FL_DEFINE_STATIC(test_type_2);
H5FL_DEFINE_STATIC(test_type_3);
H5FL_DEFINE_STATIC(test_type_4);
H5FL_DEFINE_STATIC(test_type_5);
H5FL_DEFINE_STATIC(test_type_6);
H5FL_DEFINE_STATIC(test_type_7);
H5FL_DEFINE_STATIC(test_type_8);
H5FL_DEFINE_STATIC(test_type_9);
H5FL_DEFINE_STATIC(test_type_10);
H5FL_DEFINE_STATIC(test_type_11);
H5FL_DEFINE_STATIC(test_type_12);

typedef struct {
    H5FL_reg_head_t *free_list;
    size_t           elmt_size;
    unsigned char   *fill1;
    unsigned char   *fill2;
    unsigned char   *fill3;
    void            *zero;
} type_info;

/* Array of all the free lists & info */
static type_info test_types[] = {
    {&H5FL_REG_NAME(test_type_1), sizeof(test_type_1), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(test_type_2), sizeof(test_type_2), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(test_type_3), sizeof(test_type_3), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(test_type_4), sizeof(test_type_4), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(test_type_5), sizeof(test_type_5), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(test_type_6), sizeof(test_type_6), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(test_type_7), sizeof(test_type_7), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(test_type_8), sizeof(test_type_8), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(test_type_9), sizeof(test_type_9), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(test_type_10), sizeof(test_type_10), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(test_type_11), sizeof(test_type_11), NULL, NULL, NULL, NULL},
    {&H5FL_REG_NAME(test_type_12), sizeof(test_type_12), NULL, NULL, NULL, NULL},
};

typedef enum {
    OP_MALLOC,
    OP_CALLOC,
    OP_ZERO,
    OP_FILL1,
    OP_FILL2,
    OP_FILL3,
    OP_FREE,
} test_op_code;

typedef enum { ST_UNINIT, ST_ZERO, ST_FILL1, ST_FILL2, ST_FILL3 } token_state;

typedef struct {
    void       *val;
    unsigned    type_idx;
    token_state state;
} test_token;

typedef union {
    unsigned    type_idx;
    test_token *token;
} test_op_param;

typedef struct {
    test_op_code  op_code;
    test_token   *token;
    test_op_param param;
} test_op;

typedef struct {
    unsigned vec_size;
    test_op *op_vector;
} test_vector;

typedef struct {
    unsigned     odds;
    test_op_code op_code;
} test_op_odds;

/* Operation odds when token array is not full */
/* (Must sum to 1000 (i.e. 100%) */
static const test_op_odds all_ops_odds[] = {
    {221, OP_MALLOC}, /* 22.1%  = OP_MALLOC */
    {221, OP_CALLOC}, /* 22.1%  = OP_CALLOC */
    {64, OP_ZERO},    /* 6.4% = OP_ZERO */
    {64, OP_FILL1},   /* 6.4% = OP_FILL1 */
    {64, OP_FILL2},   /* 6.4% = OP_FILL2 */
    {64, OP_FILL3},   /* 6.4% = OP_FILL3 */
    {302, OP_FREE},   /* 30.2%   = OP_FREE */
};

/* Operation odds when token array is full */
/* (Must sum to 1000 (i.e. 100%) */
static const test_op_odds full_ops_odds[] = {
    {0, OP_MALLOC},  /* 0%  = OP_MALLOC */
    {0, OP_CALLOC},  /* 0%  = OP_CALLOC */
    {104, OP_ZERO},  /* 10.4% = OP_ZERO */
    {104, OP_FILL1}, /* 10.4% = OP_FILL1 */
    {104, OP_FILL2}, /* 10.4% = OP_FILL2 */
    {104, OP_FILL3}, /* 10.4% = OP_FILL3 */
    {584, OP_FREE},  /* 58.4% = OP_FREE */
};

/* Operation odds when vector is nearly full */
/* (Must sum to 1000 (i.e. 100%) */
static const test_op_odds vec_almost_full_ops_odds[] = {
    {0, OP_MALLOC},  /* 0%  = OP_MALLOC */
    {0, OP_CALLOC},  /* 0%  = OP_CALLOC */
    {250, OP_ZERO},  /* 25% = OP_ZERO */
    {250, OP_FILL1}, /* 25% = OP_FILL1 */
    {250, OP_FILL2}, /* 25% = OP_FILL2 */
    {250, OP_FILL3}, /* 25% = OP_FILL3 */
    {0, OP_FREE},    /* 0% = OP_FREE */
};

/* Operation odds when token array is empty */
/* (Must sum to 1000 (i.e. 100%) */
static const test_op_odds empty_ops_odds[] = {
    {500, OP_MALLOC}, /* 50%  = OP_MALLOC */
    {500, OP_CALLOC}, /* 50%  = OP_CALLOC */
    {0, OP_ZERO},     /* 0% = OP_ZERO */
    {0, OP_FILL1},    /* 0% = OP_FILL1 */
    {0, OP_FILL2},    /* 0% = OP_FILL2 */
    {0, OP_FILL3},    /* 0% = OP_FILL3 */
    {0, OP_FREE},     /* 0% = OP_FREE */
};

static unsigned
get_new_token(test_token *tokens, unsigned *next_token)
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

static test_op_code
get_new_op(const test_op_odds *op_odds)
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
get_active_token(test_token *tokens, unsigned num_possible_tokens)
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
print_h5fl_reg_vector(test_vector *vector, test_token *tokens)
{
    unsigned num_active_tokens = 0; /* # of active tokens at any position in the test vector execution */

    /* Print test vector */
    for (unsigned u = 0; u < vector->vec_size; u++) {
        switch (vector->op_vector[u].op_code) {
            case OP_MALLOC:
                fprintf(stderr, "%04u (%u): OP_MALLOC - token: %p, type_idx = %u\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].param.type_idx);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state = ST_UNINIT;

                /* Increment # of active tokens */
                num_active_tokens++;
                break;

            case OP_CALLOC:
                fprintf(stderr, "%04u (%u): OP_CALLOC - token: %p, type_idx = %u\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].param.type_idx);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state = ST_ZERO;

                /* Increment # of active tokens */
                num_active_tokens++;
                break;

            case OP_ZERO:
                fprintf(stderr, "%04u (%u): OP_ZERO   - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = ST_ZERO;
                break;

            case OP_FILL1:
                fprintf(stderr, "%04u (%u): OP_FILL1  - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = ST_FILL1;
                break;

            case OP_FILL2:
                fprintf(stderr, "%04u (%u): OP_FILL2  - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = ST_FILL2;
                break;

            case OP_FILL3:
                fprintf(stderr, "%04u (%u): OP_FILL3  - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);
                vector->op_vector[u].token->state = ST_FILL3;
                break;

            case OP_FREE:
                fprintf(stderr, "%04u (%u): OP_FREE   - token: %p (type_idx: %u, state: %u)\n", u, num_active_tokens, (void *)vector->op_vector[u].token, vector->op_vector[u].token->type_idx, vector->op_vector[u].token->state);

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
init_h5fl_reg_vector(unsigned vec_size, test_vector *vector, unsigned num_tokens, test_token *tokens)
{
    unsigned num_active_tokens = 0; /* # of active tokens at any position in the test vector execution */
    unsigned curr_alloc_token;      /* Current position for allocating tokens */
    unsigned pos;                   /* Current position in the test vector */
    bool     tokens_wrapped = false;

    /* Allocate the test vector */
    vector->vec_size  = vec_size;
    vector->op_vector = calloc(vec_size, sizeof(test_op));
    CHECK_PTR(vector->op_vector, "calloc");

    /* Fiil the test vector, leaving room to free active tokens */
    pos              = 0;
    curr_alloc_token = 0;
    while (pos < (vec_size - num_active_tokens)) {
        test_op_code op_code;

        /* Check for active tokens */
        /* (Also must have enough room for both alloc & free operations) */
        if (0 == num_active_tokens && pos < (vec_size - 2))
            op_code = get_new_op(empty_ops_odds);
        else {
            /* Don't create new tokens when there won't be enough room in the
             * vector for both the alloc & free operations.
             */
            if (pos > ((vec_size - num_active_tokens) - 2))
                op_code = get_new_op(vec_almost_full_ops_odds);
            /* Don't create new tokens when the token array is full */
            else if (num_tokens == num_active_tokens)
                op_code = get_new_op(full_ops_odds);
            else
                op_code = get_new_op(all_ops_odds);
        }

        /* Set op code */
        vector->op_vector[pos].op_code = op_code;

        /* Set up specific parameters for each op code */
        switch (op_code) {
            case OP_MALLOC:
            case OP_CALLOC: {
                unsigned prev_alloc_token = curr_alloc_token;
                unsigned type_idx;
                unsigned new_token;

                /* RNG type to allocate */
                type_idx                     = (unsigned)h5_local_rand() % (unsigned)NELMTS(test_types);
                new_token                    = get_new_token(tokens, &curr_alloc_token);
                vector->op_vector[pos].token = &tokens[new_token];
                vector->op_vector[pos].param.type_idx = type_idx;

                /* Mark token as used */
                tokens[new_token].val = (void *)(~(uintptr_t)NULL);

                /* Increment # of active tokens */
                num_active_tokens++;

                /* Check for tokens wrapping */
                if (curr_alloc_token < prev_alloc_token)
                    tokens_wrapped = true;
            } break;

            case OP_ZERO:
            case OP_FILL1:
            case OP_FILL2:
            case OP_FILL3:
            case OP_FREE: {
                unsigned token_idx;

                token_idx = get_active_token(tokens, tokens_wrapped ? num_tokens : curr_alloc_token);
                vector->op_vector[pos].token = &tokens[token_idx];

                if (OP_FREE == op_code) {
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
        vector->op_vector[pos].op_code = OP_FREE;

        token_idx = get_active_token(tokens, tokens_wrapped ? num_tokens : curr_alloc_token);
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
validate_token(const test_token *token)
{
    int v;

    switch (token->state) {
        case ST_UNINIT:
            break;

        case ST_ZERO:
            v = memcmp(token->val, test_types[token->type_idx].zero, test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "ST_ZERO");
            if (0 != v)
                return(1);
            break;

        case ST_FILL1:
            v = memcmp(token->val, test_types[token->type_idx].fill1, test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "ST_FILL1");
            if (0 != v)
                return(1);
            break;

        case ST_FILL2:
            v = memcmp(token->val, test_types[token->type_idx].fill2, test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "ST_FILL2");
            if (0 != v)
                return(1);
            break;

        case ST_FILL3:
            v = memcmp(token->val, test_types[token->type_idx].fill3, test_types[token->type_idx].elmt_size);
            VERIFY(v, 0, "ST_FILL3");
            if (0 != v)
                return(1);
            break;

        default:
            assert(0 && "Invalid state for token");
            abort();
    }

    return(0);
}

static unsigned
run_h5fl_reg_vector(test_vector *vector)
{
    /* Execute test vector */
    for (unsigned u = 0; u < vector->vec_size; u++) {
        switch (vector->op_vector[u].op_code) {
            case OP_MALLOC:
                vector->op_vector[u].token->val =
                    H5FL_reg_malloc(test_types[vector->op_vector[u].param.type_idx].free_list);
                CHECK_PTR(vector->op_vector[u].token->val, "H5FL_reg_malloc");
                if (NULL == vector->op_vector[u].token->val)
                    return(1);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state    = ST_UNINIT;
                break;

            case OP_CALLOC:
                vector->op_vector[u].token->val =
                    H5FL_reg_calloc(test_types[vector->op_vector[u].param.type_idx].free_list);
                CHECK_PTR(vector->op_vector[u].token->val, "H5FL_reg_calloc");
                if (NULL == vector->op_vector[u].token->val)
                    return(1);
                vector->op_vector[u].token->type_idx = vector->op_vector[u].param.type_idx;
                vector->op_vector[u].token->state    = ST_ZERO;
                break;

            case OP_ZERO:
                if (ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_token(vector->op_vector[u].token))
                        return(1);
                memset(vector->op_vector[u].token->val, 0,
                       test_types[vector->op_vector[u].token->type_idx].elmt_size);
                vector->op_vector[u].token->state = ST_ZERO;
                break;

            case OP_FILL1:
                if (ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_token(vector->op_vector[u].token))
                        return(1);
                memcpy(vector->op_vector[u].token->val,
                       test_types[vector->op_vector[u].token->type_idx].fill1,
                       test_types[vector->op_vector[u].token->type_idx].elmt_size);
                vector->op_vector[u].token->state = ST_FILL1;
                break;

            case OP_FILL2:
                if (ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_token(vector->op_vector[u].token))
                        return(1);
                memcpy(vector->op_vector[u].token->val,
                       test_types[vector->op_vector[u].token->type_idx].fill2,
                       test_types[vector->op_vector[u].token->type_idx].elmt_size);
                vector->op_vector[u].token->state = ST_FILL2;
                break;

            case OP_FILL3:
                if (ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_token(vector->op_vector[u].token))
                        return(1);
                memcpy(vector->op_vector[u].token->val,
                       test_types[vector->op_vector[u].token->type_idx].fill3,
                       test_types[vector->op_vector[u].token->type_idx].elmt_size);
                vector->op_vector[u].token->state = ST_FILL3;
                break;

            case OP_FREE:
                if (ST_UNINIT != vector->op_vector[u].token->state)
                    if (0 != validate_token(vector->op_vector[u].token))
                        return(1);
                H5FL_reg_free(test_types[vector->op_vector[u].token->type_idx].free_list,
                              vector->op_vector[u].token->val);
                vector->op_vector[u].token->val = NULL;
                break;

            default:
                assert(0 && "Invalid op code");
                abort();
        }
    }

    return(0);
}

static H5TS_THREAD_RETURN_TYPE
test_h5fl_reg(void *_vectors)
{
    test_vector      *vectors = (test_vector *)_vectors;
    unsigned          errors = 0;
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

static test_vector *h5fl_reg_vectors[NUM_THREADS]; /* 'regular' H5FL test vectors */

/*
 **********************************************************************
 * Test H5FL package
 **********************************************************************
 */
void
tts_h5fl(const void H5_ATTR_UNUSED *params)
{
    test_token   *tokens[NUM_THREADS];               /* Test tokens */
    H5TS_thread_t threads[NUM_THREADS];
    herr_t        result;

    /* Set up local RNG */
    h5_setup_local_rand("tts_h5fl", 0);

    /* Initialize the zero values for each type */
    for (unsigned u = 0; u < (unsigned)NELMTS(test_types); u++)
        test_types[u].zero = calloc(1, test_types[u].elmt_size);

    /* Initialize the fill values for each type to RNG values */
    for (unsigned u = 0; u < (unsigned)NELMTS(test_types); u++) {
        test_types[u].fill1 = malloc(test_types[u].elmt_size);
        for (unsigned v = 0; v < test_types[u].elmt_size; v++)
            test_types[u].fill1[v] = (unsigned char)h5_local_rand();

        test_types[u].fill2 = malloc(test_types[u].elmt_size);
        for (unsigned v = 0; v < test_types[u].elmt_size; v++)
            test_types[u].fill2[v] = (unsigned char)h5_local_rand();

        test_types[u].fill3 = malloc(test_types[u].elmt_size);
        for (unsigned v = 0; v < test_types[u].elmt_size; v++)
            test_types[u].fill3[v] = (unsigned char)h5_local_rand();
    }

    /* Initialize the fill values for each type to RNG values */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        /* Allocate the test tokens */
        tokens[u] = calloc(MAX_TOKENS, sizeof(test_token));
        CHECK_PTR(tokens[u], "calloc");

        /* Initialize the test vectors */
        h5fl_reg_vectors[u] = calloc(NUM_VECTORS, sizeof(test_vector));
        CHECK_PTR(h5fl_reg_vectors[u], "calloc");

        for (unsigned v = 0; v < NUM_VECTORS; v++)
            init_h5fl_reg_vector(NUM_TEST_OPS, &h5fl_reg_vectors[u][v], MAX_TOKENS, tokens[u]);
    }

    /* Create threads and have them execute the vector */
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        result = H5TS_thread_create(&threads[u], test_h5fl_reg, h5fl_reg_vectors[u]);
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
    for (unsigned u = 0; u < (unsigned)NELMTS(test_types); u++) {
        free(test_types[u].zero);
        free(test_types[u].fill1);
        free(test_types[u].fill2);
        free(test_types[u].fill3);
    }

} /* end tts_h5fl() */

#endif /*H5_HAVE_THREADS*/
