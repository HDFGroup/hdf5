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

/*
 *  Programmer: Bill Wendling <wendling@ncsa.uiuc.edu>
 *              25. August 2003
 */

/*
 * This is the context-free grammar for our expressions:
 *
 * expr     :=  term    | term '+ term      | term '-' term
 * term     :=  factor  | factor '*' factor | factor '/' factor
 * factor   :=  number      |
 *              symbol      |
 *              '-' factor  |   // unary minus
 *              '+' factor  |   // unary plus
 *              '(' expr ')'
 * symbol   :=  [a-zA-Z][a-zA-Z0-9]*
 * number   :=  INTEGER | FLOAT
 *      // INTEGER is a C long int
 *      // FLOAT is a C double
 */

#define H5DZ_DEBUG

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/* Token types */
typedef enum {
    ERROR,
    INTEGER,
    FLOAT,
    SYMBOL,
    PLUS,
    MINUS,
    MULT,
    DIVIDE,
    LPAREN,
    RPAREN,
    END
} token_type;

/* The token */
typedef struct {
    const char *tok_expr;       /* Holds the original expression        */

    /* Current token values */
    token_type  tok_type;       /* The type of the current token        */
    const char *tok_begin;      /* The beginning of the current token   */
    const char *tok_end;        /* The end of the current token         */

    /* Previous token values */
    token_type  tok_last_type;  /* The type of the last token           */
    const char *tok_last_begin; /* The beginning of the last token      */
    const char *tok_last_end;   /* The end of the last token            */
} token;

typedef union {
    char   *sym_val;
    long    int_val;
    double  float_val;
} num_val;

typedef struct node {
    struct node    *lchild;
    struct node    *rchild;
    token_type      type;
    num_val         value;
} node;

/* local functions */
static void H5DZ_destroy_parse_tree(node *tree);
static token *H5DZ_get_token(token *current);
static node *H5DZ_parse_expression(token *current);
static node *H5DZ_parse_term(token *current);
static node *H5DZ_parse_factor(token *current);
static node *H5DZ_new_node(token_type type);

#ifdef H5DZ_DEBUG
static void H5DZ_debug(node *tree);
static void H5DZ_print(node *tree, FILE *stream);
#endif  /* H5DZ_DEBUG */

/*-------------------------------------------------------------------------
 * Function:    H5DZ_unget_token
 * Purpose:     Rollback the token to the previous token retrieved. There
 *              should only need to be one level of rollback necessary
 *              for our grammar.
 * Return:      Always succeeds.
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
H5DZ_unget_token(token *current)
{
    /* check args */
    assert(current);

    current->tok_type = current->tok_last_type;
    current->tok_begin = current->tok_last_begin;
    current->tok_end = current->tok_last_end;
}

/*-------------------------------------------------------------------------
 * Function:    H5DZ_get_token
 * Purpose:     Determine what the next valid token is in the expression
 *              string. The current position within the token string is
 *              kept internal to the token and handled by this and the
 *              unget_token function.
 * Return:      Succeess:       The passed in token with a valid tok_type
 *                              field.
 *              Failure:        The passed in token but with the tok_type
 *                              field set to ERROR.
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static token *
H5DZ_get_token(token *current)
{
    /* check args */
    assert(current);

    /* Save the last position for possible ungets */
    current->tok_last_type = current->tok_type;
    current->tok_last_begin = current->tok_begin;
    current->tok_last_end = current->tok_end;

    current->tok_begin = current->tok_end;

    while (current->tok_begin[0] != '\0') {
        if (isspace(current->tok_begin[0])) {
            /* ignore whitespace */
        } else if (isdigit(current->tok_begin[0]) ||
                   current->tok_begin[0] == '.') {
            current->tok_end = current->tok_begin;

            /*
             * integer          :=  digit-sequence
             * digit-sequence   :=  digit | digit digit-sequence
             * digit            :=  0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
             */
            if (current->tok_end[0] != '.') {
                /* is number */
                current->tok_type = INTEGER;

                while (isdigit(current->tok_end[0]))
                    ++current->tok_end;
            }

            /*
             * float            :=  digit-sequence exponent |
             *                      dotted-digits exponent?
             * dotted-digits    :=  digit-sequence '.' digit-sequence?  |
             *                      '.' digit-sequence
             * exponent         :=  [Ee] [-+]? digit-sequence
             */
            if (current->tok_end[0] == '.' ||
                    current->tok_end[0] == 'e' ||
                    current->tok_end[0] == 'E') {
                current->tok_type = FLOAT;

                if (current->tok_end[0] == '.')
                    do {
                        ++current->tok_end;
                    } while (isdigit(current->tok_end[0]));

                if (current->tok_end[0] == 'e' ||
                    current->tok_end[0] == 'E') {
                    ++current->tok_end;

                    if (current->tok_end[0] == '-' ||
                        current->tok_end[0] == '+')
                        ++current->tok_end;

                    if (!isdigit(current->tok_end[0])) {
                        current->tok_type = ERROR;
                        fprintf(stderr, "Invalidly formatted floating point number\n");
                    }

                    while (isdigit(current->tok_end[0]))
                        ++current->tok_end;
                }

                /* Check that this is a properly formatted numerical value */
                if (isalpha(current->tok_end[0]) || current->tok_end[0] == '.') {
                    current->tok_type = ERROR;
                    fprintf(stderr, "Invalidly formatted floating point number\n");
                }
            }

            break;
        } else if (isalpha(current->tok_begin[0])) {
            /* is symbol */
            current->tok_type = SYMBOL;
            current->tok_end = current->tok_begin;

            while (isalnum(current->tok_end[0]))
                ++current->tok_end;

            break;
        } else {
            /* should be +, -, *, /, (, or ) */
            switch (current->tok_begin[0]) {
                case '+':   current->tok_type = PLUS;    break;
                case '-':   current->tok_type = MINUS;   break;
                case '*':   current->tok_type = MULT;    break;
                case '/':   current->tok_type = DIVIDE;  break;
                case '(':   current->tok_type = LPAREN;  break;
                case ')':   current->tok_type = RPAREN;  break;
                default:
                    current->tok_type = ERROR;
                    fprintf(stderr, "Unknown token: %c\n", current->tok_begin[0]);
                    return current;
            }

            current->tok_end = current->tok_begin + 1;
            break;
        }

        ++current->tok_begin;
    }

    if (current->tok_begin[0] == '\0')
        current->tok_type = END;

    return current;
}

/*-------------------------------------------------------------------------
 * Function:    H5DZ_destroy_parse_tree
 * Purpose:     Recursively destroys the expression tree.
 * Return:      Nothing
 * Programmer:  Bill Wendling
 *              25. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
H5DZ_destroy_parse_tree(node *tree)
{
    if (!tree)
        return;

    if (tree->type == SYMBOL)
        free(tree->value.sym_val);

    H5DZ_destroy_parse_tree(tree->lchild);
    H5DZ_destroy_parse_tree(tree->rchild);
    free(tree);
}

/*-------------------------------------------------------------------------
 * Function:    H5DZ_parse
 * Purpose:     Entry function for parsing the expression string.
 * Return:      Success:    Valid NODE ptr to an expression tree.
 *              Failure:    NULL
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static node *
H5DZ_parse(const char *expression)
{
    token tok;

    if (!expression)
        return NULL;

    /* Set up the initial token for parsing */
    tok.tok_expr = tok.tok_begin = tok.tok_end = expression;
    return H5DZ_parse_expression(&tok);
}

/*-------------------------------------------------------------------------
 * Function:    H5DZ_parse_expression
 * Purpose:     Beginning of the recursive descent parser to parse the
 *              expression. An expression is:
 *
 *                  expr     :=  term | term '+' term | term '-' term
 *
 * Return:      Success:    Valid NODE ptr to expression tree
 *              Failure:    NULL
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static node *
H5DZ_parse_expression(token *current)
{
    node *expr = H5DZ_parse_term(current);

    for (;;) {
        node *new_node;

        current = H5DZ_get_token(current);

        switch (current->tok_type) {
        case PLUS:
            new_node = H5DZ_new_node(PLUS);

            if (!new_node) {
                H5DZ_destroy_parse_tree(expr);
                expr = NULL;
                goto done;
            }

            new_node->lchild = expr;
            new_node->rchild = H5DZ_parse_term(current);

            if (!new_node->rchild) {
                H5DZ_destroy_parse_tree(new_node);
                fprintf(stderr, "Parsing error occurred\n");
                expr = NULL;
                goto done;
            }

            expr = new_node;
            break;
        case MINUS:
            new_node = H5DZ_new_node(MINUS);

            if (!new_node) {
                H5DZ_destroy_parse_tree(expr);
                expr = NULL;
                goto done;
            }

            new_node->lchild = expr;
            new_node->rchild = H5DZ_parse_term(current);

            if (!new_node->rchild) {
                H5DZ_destroy_parse_tree(new_node);
                fprintf(stderr, "Parsing error occurred\n");
                expr = NULL;
                goto done;
            }

            expr = new_node;
            break;
        case RPAREN:
            H5DZ_unget_token(current);
            goto done;
        case END:
            goto done;
        default:
            H5DZ_destroy_parse_tree(expr);
            expr = NULL;
            fprintf(stderr, "Parsing error around %c\n", current->tok_begin[0]);
            goto done;
        }
    }

done:
    return expr;
}

/*-------------------------------------------------------------------------
 * Function:    H5DZ_parse_term
 * Purpose:     Parses a term in our expression language. A term is:
 *
 *                  term :=  factor | factor '*' factor | factor '/' factor
 *
 * Return:      Success:    Valid NODE ptr to expression tree
 *              Failure:    NULL
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static node *
H5DZ_parse_term(token *current)
{
    node *term = H5DZ_parse_factor(current);

    for (;;) {
        node *new_node;

        current = H5DZ_get_token(current);

        switch (current->tok_type) {
        case MULT:
            new_node = H5DZ_new_node(MULT);

            if (!new_node) {
                H5DZ_destroy_parse_tree(term);
                term = NULL;
                goto done;
            }

            new_node->lchild = term;
            new_node->rchild = H5DZ_parse_factor(current);

            if (!new_node->rchild) {
                H5DZ_destroy_parse_tree(term);
                fprintf(stderr, "Parsing error occurred\n");
                term = NULL;
                goto done;
            }

            term = new_node;
            break;
        case DIVIDE:
            new_node = H5DZ_new_node(DIVIDE);

            if (!new_node) {
                H5DZ_destroy_parse_tree(term);
                term = NULL;
                goto done;
            }

            new_node->lchild = term;
            new_node->rchild = H5DZ_parse_factor(current);
            term = new_node;

            if (!new_node->rchild) {
                H5DZ_destroy_parse_tree(term);
                fprintf(stderr, "Parsing error occurred\n");
                term = NULL;
                goto done;
            }

            break;
        case RPAREN:
            H5DZ_unget_token(current);
            goto done;
        case END:
            goto done;
        default:
            H5DZ_unget_token(current);
            goto done;
        }
    }

done:
    return term;
}

/*-------------------------------------------------------------------------
 * Function:    H5DZ_parse_factor
 * Purpose:     Parses a factor in our expression language. A factor is:
 *
 *                  factor   :=  number      |  // C long or double
 *                               symbol      |  // C identifier
 *                               '-' factor  |  // unary minus
 *                               '+' factor  |  // unary plus
 *                               '(' expr ')'
 *
 * Return:      Success:    Valid NODE ptr to expression tree
 *              Failure:    NULL
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static node *
H5DZ_parse_factor(token *current)
{
    node *factor, *new_node;

    current = H5DZ_get_token(current);

    switch (current->tok_type) {
    case INTEGER:
        factor = H5DZ_new_node(INTEGER);

        if (!factor)
            goto done;

        sscanf(current->tok_begin, "%ld", &factor->value.int_val);
        break;
    case FLOAT:
        factor = H5DZ_new_node(FLOAT);

        if (!factor)
            goto done;

        sscanf(current->tok_begin, "%lf", &factor->value.float_val);
        break;
    case SYMBOL:
        factor = H5DZ_new_node(SYMBOL);

        if (!factor)
            goto done;

        factor->value.sym_val = calloc(current->tok_end - current->tok_begin + 1, 1);
        strncpy(factor->value.sym_val, current->tok_begin,
                current->tok_end - current->tok_begin);
        break;
    case LPAREN:
        factor = H5DZ_parse_expression(current);

        if (!factor)
            goto done;

        current = H5DZ_get_token(current);

        if (current->tok_type != RPAREN) {
            H5DZ_destroy_parse_tree(factor);
            fprintf(stderr, "Syntax error around %c\n", current->tok_begin[0]);
            factor = NULL;
            goto done;
        }

        break;
    case RPAREN:
        /* We shouldn't see a ) right now */
        H5DZ_destroy_parse_tree(factor);
        fprintf(stderr, "Syntax error: unexpected ')' \n");
        factor = NULL;
        goto done;
    case PLUS:
        /* unary + */
        new_node = H5DZ_parse_factor(current);

        if (new_node) {
            if (new_node->type != INTEGER && new_node->type != FLOAT &&
                    new_node->type != SYMBOL) {
                H5DZ_destroy_parse_tree(new_node);
                H5DZ_destroy_parse_tree(factor);
                fprintf(stderr, "Parsing error occurred\n");
                return NULL;
            }

            factor = new_node;
            new_node = H5DZ_new_node(PLUS);

            if (!new_node) {
                H5DZ_destroy_parse_tree(factor);
                fprintf(stderr, "Parsing error occurred\n");
                factor = NULL;
                goto done;
            }

            new_node->rchild = factor;
            factor = new_node;
        } else {
            H5DZ_destroy_parse_tree(factor);
            fprintf(stderr, "Parsing error occurred\n");
            return NULL;
        }

        break;
    case MINUS:
        /* unary - */
        new_node = H5DZ_parse_factor(current);

        if (new_node) {
            if (new_node->type != INTEGER && new_node->type != FLOAT &&
                    new_node->type != SYMBOL) {
                H5DZ_destroy_parse_tree(new_node);
                H5DZ_destroy_parse_tree(factor);
                fprintf(stderr, "Parsing error occurred\n");
                return NULL;
            }

            factor = new_node;
            new_node = H5DZ_new_node(MINUS);

            if (!new_node) {
                H5DZ_destroy_parse_tree(factor);
                fprintf(stderr, "Parsing error occurred\n");
                factor = NULL;
                goto done;
            }

            new_node->rchild = factor;
            factor = new_node;
        } else {
            H5DZ_destroy_parse_tree(factor);
            fprintf(stderr, "Parsing error occurred\n");
            return NULL;
        }

        break;
    case END:
        goto done;
    }

done:
    return factor;
}

/*-------------------------------------------------------------------------
 * Function:    H5DZ_new_node
 * Purpose:     Create and initilize a new NODE structure.
 * Return:      Success:    Valid NODE ptr
 *              Failure:    NULL
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static node *
H5DZ_new_node(token_type type)
{
    node *new_node = calloc(1, sizeof(node));
 
    if (new_node) {
        new_node->type = type;
    } else {
        fprintf(stderr, "Out of memory\n");
    }

    return new_node;
}

#ifdef H5DZ_DEBUG
/*-------------------------------------------------------------------------
 * Function:    H5DZ_debug
 * Purpose:     Print out the expression in a nice format which displays
 *              the precedences explicitly with parentheses.
 * Return:      Nothing
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
H5DZ_debug(node *tree)
{
    fprintf(stderr, "Expression: ");
    H5DZ_print(tree, stderr);
    fprintf(stderr, "\n");
}

/*-------------------------------------------------------------------------
 * Function:    H5DZ_print
 * Purpose:     Print out the expression in a nice format which displays
 *              the precedences explicitly with parentheses.
 * Return:      Nothing
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
H5DZ_print(node *tree, FILE *stream)
{
    /* check args */
    assert(stream);

    if (!tree)
        return;

    if (tree->type == INTEGER) {
        fprintf(stream, "%ld", tree->value.int_val);
    } else if (tree->type == FLOAT) {
        fprintf(stream, "%f", tree->value.float_val);
    } else if (tree->type == SYMBOL) {
        fprintf(stream, "%s", tree->value.sym_val);
    } else {
        fprintf(stream, "(");
        H5DZ_print(tree->lchild, stream);

        switch (tree->type) {
        case PLUS:      fprintf(stream, "+"); break;
        case MINUS:     fprintf(stream, "-"); break;
        case MULT:      fprintf(stream, "*"); break;
        case DIVIDE:    fprintf(stream, "/"); break;
        default: fprintf(stream, "Invalid expression tree\n");
            return;
        }

        H5DZ_print(tree->rchild, stream);
        fprintf(stream, ")");
    }
}
#endif  /* H5DZ_DEBUG */

int main(int argc, char **argv)
{
    node *n;
    printf("Parsing Expression: \"%s\"\n", argv[1]);
    n = H5DZ_parse(argv[1]);
    H5DZ_debug(n);
    return 0;
}
