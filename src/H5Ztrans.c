/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#define H5Z_PACKAGE		/*suppress error about including H5Zpkg	  */

/* Pablo information */
/* (Put before include files to avoid problems with inline functions) */
#define PABLO_MASK	H5Z_deflate_mask

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Zpkg.h"		/* Data filters				*/

/* Token types */
typedef enum {
    H5Z_XFORM_ERROR,
    H5Z_XFORM_INTEGER, /* this represents an integer type in the data transform expression */
    H5Z_XFORM_FLOAT,  /* this represents a floating point type in the data transform expression */
    H5Z_XFORM_SYMBOL,
    H5Z_XFORM_PLUS,
    H5Z_XFORM_MINUS,
    H5Z_XFORM_MULT,
    H5Z_XFORM_DIVIDE,
    H5Z_XFORM_LPAREN,
    H5Z_XFORM_RPAREN,
    H5Z_XFORM_END
} H5Z_token_type;

typedef union {
    char   *sym_val;
    long    int_val;
    double  float_val;
} H5Z_num_val;

typedef struct H5Z_node {
    struct H5Z_node    *lchild;
    struct H5Z_node    *rchild;
    H5Z_token_type      type;
    H5Z_num_val         value;
} H5Z_node;

struct H5Z_data_xform_t {
    char*       xform_exp;
    struct H5Z_node*       parse_root;
};

typedef struct result {
    H5Z_token_type type;
    H5Z_num_val    value;
    H5T_class_t ar_type;
} H5Z_result;


/* The token */
typedef struct {
    const char *tok_expr;       /* Holds the original expression        */

    /* Current token values */
    H5Z_token_type  tok_type;       /* The type of the current token        */
    const char *tok_begin;      /* The beginning of the current token   */
    const char *tok_end;        /* The end of the current token         */

    /* Previous token values */
    H5Z_token_type  tok_last_type;  /* The type of the last token           */
    const char *tok_last_begin; /* The beginning of the last token      */
    const char *tok_last_end;   /* The end of the last token            */
} H5Z_token;

/* Interface initialization */
static int interface_initialize_g = 0;
#define INTERFACE_INIT NULL 

/* Local function prototypes */
static H5Z_token *H5Z_get_token(H5Z_token *current);
static H5Z_node *H5Z_parse_expression(H5Z_token *current);
static H5Z_node *H5Z_parse_term(H5Z_token *current);
static H5Z_node *H5Z_parse_factor(H5Z_token *current);
static H5Z_node *H5Z_new_node(H5Z_token_type type);
static void H5Z_do_op(H5Z_node* tree);
static hid_t H5Z_xform_find_type(const H5T_t* type);
static H5Z_result H5Z_eval_full(H5Z_node *tree, void* array, hsize_t array_size, hid_t array_type);
static void H5Z_xform_destroy_parse_tree(H5Z_node *tree);
static void* H5Z_xform_parse(const char *expression);
static void* H5Z_xform_copy_tree(H5Z_node* tree);
static void H5Z_xform_reduce_tree(H5Z_node* tree);
#ifdef H5Z_XFORM_DEBUG
static void H5Z_XFORM_DEBUG(H5Z_node *tree);
static void H5Z_print(H5Z_node *tree, FILE *stream);
#endif  /* H5Z_XFORM_DEBUG */


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
 * number   :=  H5Z_XFORM_INTEGER | FLOAT
 *      // H5Z_XFORM_INTEGER is a C long int
 *      // FLOAT is a C double
 */


/*-------------------------------------------------------------------------
 * Function:    H5Z_unget_token
 * Purpose:     Rollback the H5Z_token to the previous H5Z_token retrieved. There
 *              should only need to be one level of rollback necessary
 *              for our grammar.
 * Return:      Always succeeds.
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
  *              Leon Arber:  Added FUNC_ENTER / FUNC_LEAVE pairs
*
 *-------------------------------------------------------------------------
 */
static void
H5Z_unget_token(H5Z_token *current)
{
    /* check args */
    assert(current);


    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5Z_unget_token)
    
    current->tok_type = current->tok_last_type;
    current->tok_begin = current->tok_last_begin;
    current->tok_end = current->tok_last_end;

    FUNC_LEAVE_NOAPI_VOID
            
    
}


/*-------------------------------------------------------------------------
 * Function:    H5Z_get_token
 * Purpose:     Determine what the next valid H5Z_token is in the expression
 *              string. The current position within the H5Z_token string is
 *              kept internal to the H5Z_token and handled by this and the
 *              unget_H5Z_token function.
 * Return:      Succeess:       The passed in H5Z_token with a valid tok_type
 *                              field.
 *              NULLure:        The passed in H5Z_token but with the tok_type
 *                              field set to ERROR.
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
 *               Leon Arber:  Added FUNC_ENTER / FUNC_LEAVE pairs
 *-------------------------------------------------------------------------
 */
static H5Z_token *
H5Z_get_token(H5Z_token *current)
{
    
    void*         ret_value=current;
    
    FUNC_ENTER_NOAPI(H5Z_get_token, NULL)

    
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
             * H5Z_XFORM_INTEGER          :=  digit-sequence
             * digit-sequence   :=  digit | digit digit-sequence
             * digit            :=  0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
             */
            if (current->tok_end[0] != '.') {
                /* is number */
                current->tok_type = H5Z_XFORM_INTEGER;

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
                current->tok_type = H5Z_XFORM_FLOAT;

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
                        current->tok_type = H5Z_XFORM_ERROR;
                        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, current, "Invalidly formatted floating point number")
                    }

                    while (isdigit(current->tok_end[0]))
                        ++current->tok_end;
                }

                /* Check that this is a properly formatted numerical value */
                if (isalpha(current->tok_end[0]) || current->tok_end[0] == '.') {
                    current->tok_type = H5Z_XFORM_ERROR;
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, current, "Invalidly formatted floating point number")
                }
            }

            break;
        } else if (isalpha(current->tok_begin[0])) {
            /* is symbol */
            current->tok_type = H5Z_XFORM_SYMBOL;
            current->tok_end = current->tok_begin;

            while (isalnum(current->tok_end[0]))
                ++current->tok_end;

            break;
        } else {
            /* should be +, -, *, /, (, or ) */
            switch (current->tok_begin[0]) {
                case '+':   current->tok_type = H5Z_XFORM_PLUS;    break;
                case '-':   current->tok_type = H5Z_XFORM_MINUS;   break;
                case '*':   current->tok_type = H5Z_XFORM_MULT;    break;
                case '/':   current->tok_type = H5Z_XFORM_DIVIDE;  break;
                case '(':   current->tok_type = H5Z_XFORM_LPAREN;  break;
                case ')':   current->tok_type = H5Z_XFORM_RPAREN;  break;
                default:
                    current->tok_type = H5Z_XFORM_ERROR;
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, current, "Unknown H5Z_token in data transform expression ")
            }

            current->tok_end = current->tok_begin + 1;
            break;
        }

        ++current->tok_begin;
    }

    if (current->tok_begin[0] == '\0')
        current->tok_type = H5Z_XFORM_END;

    HGOTO_DONE(current);

done:
    FUNC_LEAVE_NOAPI(ret_value)
      
}


/*-------------------------------------------------------------------------
 * Function:    H5Z_xform_destroy_parse_tree
 * Purpose:     Recursively destroys the expression tree.
 * Return:      Nothing
 * Programmer:  Bill Wendling
 *              25. August 2003
 * Modifications:
 *              Leon Arber: Added FUNC_ENTER / FUNC_LEAVE pairs
 *
 *-------------------------------------------------------------------------
 */
void
H5Z_xform_destroy_parse_tree(H5Z_node *tree)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5Z_xform_destroy_parse_tree)
    
    if (!tree)
        return;

    if (tree->type == H5Z_XFORM_SYMBOL)
        H5MM_xfree(tree->value.sym_val);

    H5Z_xform_destroy_parse_tree(tree->lchild);
    H5Z_xform_destroy_parse_tree(tree->rchild);
    H5MM_xfree(tree);
    tree = NULL;

    FUNC_LEAVE_NOAPI_VOID
}



/*-------------------------------------------------------------------------
 * Function:    H5Z_parse
 * Purpose:     Entry function for parsing the expression string.
 * Return:      Success:    Valid H5Z_node ptr to an expression tree.
 *              NULLure:    NULL
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
 *              Leon Arber:  Added FUNC_ENTER / FUNC_LEAVE pairs
 *
 *-------------------------------------------------------------------------
 */
void *
H5Z_xform_parse(const char *expression)
{
    H5Z_token tok;
    void* ret_value;
   
    FUNC_ENTER_NOAPI(H5Z_xform_parse, NULL)
    
    if (!expression)
        return NULL;

    /* Set up the initial H5Z_token for parsing */
    tok.tok_expr = tok.tok_begin = tok.tok_end = expression;
    
    ret_value = (void*)H5Z_parse_expression(&tok);

    H5Z_xform_reduce_tree((H5Z_node*)ret_value);
    
done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5Z_parse_expression
 * Purpose:     Beginning of the recursive descent parser to parse the
 *              expression. An expression is:
 *
 *                  expr     :=  term | term '+' term | term '-' term
 *
 * Return:      Success:    Valid H5Z_node ptr to expression tree
 *              NULLure:    NULL
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
  *              Leon Arber: Added FUNC_ENTER / FUNC_LEAVE pairs
*
 *-------------------------------------------------------------------------
 */
static H5Z_node *
H5Z_parse_expression(H5Z_token *current)
{
    H5Z_node *expr;
    void*         ret_value=NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5Z_parse_expression)

    expr = H5Z_parse_term(current);

    for (;;) {
        H5Z_node *new_node;
        new_node = NULL;

        current = H5Z_get_token(current);

        switch (current->tok_type) {
        case H5Z_XFORM_PLUS:
            new_node = H5Z_new_node(H5Z_XFORM_PLUS);

            if (!new_node) {
                H5Z_xform_destroy_parse_tree(expr);
                expr = NULL;
                HGOTO_DONE(expr)
            }

            new_node->lchild = expr;
            new_node->rchild = H5Z_parse_term(current);

            if (!new_node->rchild) {
                H5Z_xform_destroy_parse_tree(new_node);
                expr = NULL;
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error parsing data transform expression")
            }

            expr = new_node;
            break;
        case H5Z_XFORM_MINUS:
            new_node = H5Z_new_node(H5Z_XFORM_MINUS);

            if (!new_node) {
                H5Z_xform_destroy_parse_tree(expr);
                expr = NULL;
                HGOTO_DONE(expr)
            }

            new_node->lchild = expr;
            new_node->rchild = H5Z_parse_term(current);

            if (!new_node->rchild) {
                H5Z_xform_destroy_parse_tree(new_node);
                expr = NULL;
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error parsing data transform expression")
            }

            expr = new_node;
            break;
        case H5Z_XFORM_RPAREN:
            H5Z_unget_token(current);
            HGOTO_DONE(expr)
        case H5Z_XFORM_END:
            HGOTO_DONE(expr)
        default:
            H5Z_xform_destroy_parse_tree(expr);
            expr = NULL;
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error parsing data transform expression")
        }
    }

done:
    FUNC_LEAVE_NOAPI(expr)
}


/*-------------------------------------------------------------------------
 * Function:    H5Z_parse_term
 * Purpose:     Parses a term in our expression language. A term is:
 *
 *                  term :=  factor | factor '*' factor | factor '/' factor
 *
 * Return:      Success:    Valid H5Z_node ptr to expression tree
 *              NULLure:    NULL
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
  *              Leon Arber: Added FUNC_ENTER / FUNC_LEAVE pairs
*
 *-------------------------------------------------------------------------
 */
static H5Z_node *
H5Z_parse_term(H5Z_token *current)
{
    H5Z_node *term = NULL;
    void*         ret_value=NULL;
   
    FUNC_ENTER_NOAPI(H5Z_parse_term, NULL);
    term = H5Z_parse_factor(current);
   
    for (;;) {
        H5Z_node *new_node;
        new_node = NULL;
        
        current = H5Z_get_token(current);

        switch (current->tok_type) {
        case H5Z_XFORM_MULT:
            new_node = H5Z_new_node(H5Z_XFORM_MULT);

            if (!new_node) {
                H5Z_xform_destroy_parse_tree(term);
                term = NULL;
                HGOTO_DONE(term)
            }

            new_node->lchild = term;
            new_node->rchild = H5Z_parse_factor(current);

            if (!new_node->rchild) {
                H5Z_xform_destroy_parse_tree(term);
                term = NULL;
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error parsing data transform expression")
            }

            term = new_node;
            break;
        case H5Z_XFORM_DIVIDE:
            new_node = H5Z_new_node(H5Z_XFORM_DIVIDE);

            if (!new_node) {
                H5Z_xform_destroy_parse_tree(term);
                term = NULL;
                HGOTO_DONE(term)
            }

            new_node->lchild = term;
            new_node->rchild = H5Z_parse_factor(current);
            term = new_node;

            if (!new_node->rchild) {
                H5Z_xform_destroy_parse_tree(term);
                term = NULL;
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error parsing data transform expression")
            }

            break;
        case H5Z_XFORM_RPAREN:
            H5Z_unget_token(current);
            HGOTO_DONE(term)
        case H5Z_XFORM_END:
            HGOTO_DONE(term)
        default:
            H5Z_unget_token(current);
            HGOTO_DONE(term)
        }
    }

done:
  FUNC_LEAVE_NOAPI(term)

}


/*-------------------------------------------------------------------------
 * Function:    H5Z_parse_factor
 * Purpose:     Parses a factor in our expression language. A factor is:
 *
 *                  factor   :=  number      |  // C long or double
 *                               symbol      |  // C identifier
 *                               '-' factor  |  // unary minus
 *                               '+' factor  |  // unary plus
 *                               '(' expr ')'
 *
 * Return:      Success:    Valid H5Z_node ptr to expression tree
 *              NULLure:    NULL
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
  *              Leon Arber: Added FUNC_ENTER / FUNC_LEAVE pairs
*
 *-------------------------------------------------------------------------
 */
static H5Z_node *
H5Z_parse_factor(H5Z_token *current)
{
    H5Z_node 	*factor=NULL;
    H5Z_node 	*new_node=NULL;
    void*        ret_value=NULL;
   
    FUNC_ENTER_NOAPI(H5Z_parse_factor, NULL);
    
    current = H5Z_get_token(current);

    switch (current->tok_type) {
    case H5Z_XFORM_INTEGER:
        factor = H5Z_new_node(H5Z_XFORM_INTEGER);

        if (!factor)
            HGOTO_DONE(factor)

        sscanf(current->tok_begin, "%ld", &factor->value.int_val);
        break;
    case H5Z_XFORM_FLOAT:
        factor = H5Z_new_node(H5Z_XFORM_FLOAT);

        if (!factor)
            HGOTO_DONE(factor)

        sscanf(current->tok_begin, "%lf", &factor->value.float_val);
        break;
    case H5Z_XFORM_SYMBOL:
        factor = H5Z_new_node(H5Z_XFORM_SYMBOL);

        if (!factor)
            HGOTO_DONE(factor)

        factor->value.sym_val = H5MM_calloc((size_t)(current->tok_end - current->tok_begin) + 1);
        HDstrncpy(factor->value.sym_val, current->tok_begin,
                (size_t)(current->tok_end - current->tok_begin));
        break;
    case H5Z_XFORM_LPAREN:
        factor = H5Z_parse_expression(current);

        if (!factor)
            HGOTO_DONE(factor)

        current = H5Z_get_token(current);

        if (current->tok_type != H5Z_XFORM_RPAREN) {
            H5Z_xform_destroy_parse_tree(factor);
            factor = NULL;
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Syntax error in data transform expression")
        }

        break;
    case H5Z_XFORM_RPAREN:
        /* We shouldn't see a ) right now */
        H5Z_xform_destroy_parse_tree(factor);
        factor = NULL;
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Syntax error: unexpected ')' ")
    case H5Z_XFORM_PLUS:
        /* unary + */
        new_node = H5Z_parse_factor(current);

        if (new_node) {
            if (new_node->type != H5Z_XFORM_INTEGER && new_node->type != H5Z_XFORM_FLOAT &&
                    new_node->type != H5Z_XFORM_SYMBOL) {
                H5Z_xform_destroy_parse_tree(new_node);
                H5Z_xform_destroy_parse_tree(factor);
                factor=NULL;
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error parsing data transform expression")
            }

            factor = new_node;
            new_node = H5Z_new_node(H5Z_XFORM_PLUS);

            if (!new_node) {
                H5Z_xform_destroy_parse_tree(factor);
                factor = NULL;
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error parsing data transform expression")
            }

            new_node->rchild = factor;
            factor = new_node;
        } else {
            H5Z_xform_destroy_parse_tree(factor);
            factor = NULL;
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error parsing data transform expression")
        }

        break;
    case H5Z_XFORM_MINUS:
        /* unary - */
        new_node = H5Z_parse_factor(current);

        if (new_node) {
            if (new_node->type != H5Z_XFORM_INTEGER && new_node->type != H5Z_XFORM_FLOAT &&
                    new_node->type != H5Z_XFORM_SYMBOL) {
                H5Z_xform_destroy_parse_tree(new_node);
                H5Z_xform_destroy_parse_tree(factor);
                factor = NULL;
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error parsing data transform expression")
            }

            factor = new_node;
            new_node = H5Z_new_node(H5Z_XFORM_MINUS);

            if (!new_node) {
                H5Z_xform_destroy_parse_tree(factor);
                factor = NULL;
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error parsing data transform expression")
            }

            new_node->rchild = factor;
            factor = new_node;
        } else {
            H5Z_xform_destroy_parse_tree(factor);
            factor = NULL;
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error parsing data transform expression")
        }

        break;
    case H5Z_XFORM_END:
        HGOTO_DONE(factor)
    }

done:

    FUNC_LEAVE_NOAPI(factor);
}


/*-------------------------------------------------------------------------
 * Function:    H5Z_new_node
 * Purpose:     Create and initilize a new H5Z_node structure.
 * Return:      Success:    Valid H5Z_node ptr
 *              NULLure:    NULL
 * Programmer:  Bill Wendling
 *              26. August 2003
 * Modifications:
  *              Leon Arber: Added FUNC_ENTER / FUNC_LEAVE pairs
*
 *-------------------------------------------------------------------------
 */
static H5Z_node *
H5Z_new_node(H5Z_token_type type)
{
    H5Z_node* ret_value;
    H5Z_node* new_node;
    
    FUNC_ENTER_NOAPI(H5Z_new_node, NULL)
    
    new_node = H5MM_calloc(sizeof(H5Z_node));
 
    if (new_node) 
        new_node->type = type;
    else 
        assert(new_node);

done:
        
        FUNC_LEAVE_NOAPI(new_node);
}


/*-------------------------------------------------------------------------
 * Function:    H5Z_eval_full
 * Purpose: 	If the transform is trivial, this function applies it.
 * 		Otherwise, it calls H5Z_xform_eval_full to do the full 
 * 		transform.
 * Return:      SUCCEED if transform applied succesfully, FAIL otherwise
 * Programmer:  Leon Arber
 * 		5/1/04
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5Z_xform_eval(const H5Z_data_xform_t *data_xform_prop, void* array, hsize_t array_size, const H5T_t *buf_type)
{
    H5Z_node *tree;
    hid_t array_type;
    unsigned int i;
    int n;  
    float f;
    H5Z_result res;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5Z_xform_eval, FAIL)

	assert(data_xform_prop);

    tree=data_xform_prop->parse_root;

    /* Get the datatype ID for the buffer's type */
    if( (array_type = H5Z_xform_find_type(buf_type)) < 0) 
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Could not find matching data type for data transform.")

    /* If it's a trivial data transform, perform it */
    if( tree->type == H5Z_XFORM_INTEGER)
    {
	if(array_type == H5T_NATIVE_INT)
	{
	    n = tree->value.int_val;
	    for(i=0; i<array_size; i++)
		*((int*)array + i) = n;

	}
	else if(array_type == H5T_NATIVE_FLOAT)
	{
	    f = (float)tree->value.int_val;
	    for(i=0; i<array_size; i++)
		*((float*)array + i) = f;
	}
	else
	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Unsupported data type for data transform")


    }
    else if (tree->type == H5Z_XFORM_FLOAT)
    {
	if(array_type == H5T_NATIVE_INT)
	{
	    n = (int)tree->value.float_val;
	    for(i=0; i<array_size; i++)
		*((int*)array + i) = n;

	}
	else if(array_type == H5T_NATIVE_FLOAT)
	{
	    f = tree->value.float_val;
	    for(i=0; i<array_size; i++)
		*((float*)array + i) = f;
	}
	else
	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Unsupported data type for data transform")

    }
    /* Otherwise, do the full data transform */
    else
    {
	res = H5Z_eval_full(tree, array, array_size, array_type);
	if(res.type == H5Z_XFORM_ERROR)
	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "error while performing data transform")
    }

   	   
done:
    FUNC_LEAVE_NOAPI(ret_value);
}
           



/*-------------------------------------------------------------------------
 * Function:    H5Z_eval_full
 * Purpose: 	Does a full evaluation of the parse tree contained in tree
 * 		and applies this transform to array.
 * Return:      Nothing
 * Programmer:  Leon Arber
 * 		5/1/04
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5Z_result
H5Z_eval_full(H5Z_node *tree, void* array, hsize_t array_size,  hid_t array_type)
{

    H5Z_result res, resl, resr, ret_value, error;
    unsigned int i;
    
    error.type = H5Z_XFORM_ERROR;
        
    FUNC_ENTER_NOAPI(H5Z_eval_full, ret_value);
    
    /* check args */
    assert(tree);

    if (tree->type == H5Z_XFORM_INTEGER) 
    {
        res.type = H5Z_XFORM_INTEGER;
        res.value = tree->value;
        HGOTO_DONE(res)
    } 
    else if (tree->type == H5Z_XFORM_FLOAT) 
    {
        res.type = H5Z_XFORM_FLOAT;
        res.value = tree->value;
        HGOTO_DONE(res)
    } 
    else if (tree->type == H5Z_XFORM_SYMBOL) 
    {
        res.type = H5Z_XFORM_SYMBOL;
        res.value = tree->value;
        res.ar_type = array_type;
        HGOTO_DONE(res)
    } 
    else 
    {
        resl = H5Z_eval_full(tree->lchild, array, array_size, array_type);
        resr = H5Z_eval_full(tree->rchild, array, array_size, array_type);
        switch (tree->type) 
        {
        case H5Z_XFORM_PLUS:  
           if( (resl.type == H5Z_XFORM_SYMBOL) && (resr.type==H5Z_XFORM_INTEGER))
            {
                res.type = H5Z_XFORM_SYMBOL; 
                    if(array_type == H5T_NATIVE_INT)
                    {
			for(i=0; i<array_size; i++)
			    *((int*)array + i) = resr.value.int_val + *((int*)array + i);
                    }
                    else if(array_type == H5T_NATIVE_FLOAT)
                    {
			for(i=0; i<array_size; i++)
			    *((float*)array + i) = resr.value.int_val + *((float*)array + i);
                    }
		    else
			HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")
             HGOTO_DONE(res)
            }
            else if( (resl.type == H5Z_XFORM_SYMBOL) && (resr.type==H5Z_XFORM_FLOAT)) 
            {
                res.type = H5Z_XFORM_SYMBOL;
                    if(array_type == H5T_NATIVE_INT)
                    {
			for(i=0; i<array_size; i++)
			    *((int*)array + i) += resr.value.float_val;
		    }
		    else if(array_type == H5T_NATIVE_FLOAT)
		    {
			for(i=0; i<array_size; i++)
			    *((float*)array + i) += resr.value.float_val;
                    }
		    else
			HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

                HGOTO_DONE(res)
            }
            else if( (resr.type == H5Z_XFORM_SYMBOL) && (resl.type==H5Z_XFORM_INTEGER))
            {
                 res.type = H5Z_XFORM_SYMBOL; 
                    if(array_type == H5T_NATIVE_INT)
                    {
			for(i=0; i<array_size; i++)
			    *((int*)array + i) += resl.value.int_val;
		    }
		    else if(array_type == H5T_NATIVE_FLOAT)
		    {
			for(i=0; i<array_size; i++)
			    *((float*)array + i) += resl.value.int_val ;
		    }
		    else
			HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")
                HGOTO_DONE(res)
          
            }
	   else if( (resr.type == H5Z_XFORM_SYMBOL) && (resl.type==H5Z_XFORM_FLOAT))
	   {

	       res.type = H5Z_XFORM_SYMBOL;
	       if(array_type == H5T_NATIVE_INT)
	       {
		   for(i=0; i<array_size; i++)
		       *((int*)array + i) += resl.value.float_val;
	       }
	       else if(array_type == H5T_NATIVE_FLOAT)
	       {
		   for(i=0; i<array_size; i++)
		       *((float*)array + i) += resl.value.float_val ;
	       }else
		   HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

		       HGOTO_DONE(res)
            }
            else
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Unexpected type conversion operation")

            HGOTO_DONE(res) 
            break;
	case H5Z_XFORM_MINUS:

	    if( (resl.type == H5Z_XFORM_SYMBOL) && (resr.type==H5Z_XFORM_INTEGER))
	    {
		res.type = H5Z_XFORM_SYMBOL; 
		if(array_type == H5T_NATIVE_INT)
		{
		    for(i=0; i<array_size; i++)
			*((int*)array + i) -= resr.value.int_val;
		}
		else if(array_type == H5T_NATIVE_FLOAT)
		{
		    for(i=0; i<array_size; i++)
			*((float*)array + i) -= resr.value.int_val;
		}else
		    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

			HGOTO_DONE(res)
	    }
	    else if( (resl.type == H5Z_XFORM_SYMBOL) && (resr.type==H5Z_XFORM_FLOAT)) /*we can't upgrade an array w/o allocating more memory, so we downgrade the float_val to an int.*/
	    {
                res.type = H5Z_XFORM_SYMBOL;
                    if(array_type == H5T_NATIVE_INT)
                    {
			for(i=0; i<array_size; i++)
			    *((int*)array + i) -= resr.value.float_val;
		    }
		    else if(array_type == H5T_NATIVE_FLOAT)
		    {
			for(i=0; i<array_size; i++)
			    *((float*)array + i) -= resr.value.float_val;
		    }
		    else
		    {
			HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")
		    }
		    HGOTO_DONE(res)
	    }
	    else if( (resr.type == H5Z_XFORM_SYMBOL) && (resl.type==H5Z_XFORM_INTEGER))
	    {
		res.type = H5Z_XFORM_SYMBOL; 
		    if(array_type == H5T_NATIVE_INT)
		    {
			for(i=0; i<array_size; i++)
			    *((int*)array + i) = resl.value.int_val - *((int*)array + i);
		    }
		    else if(array_type == H5T_NATIVE_FLOAT)
		    {
			for(i=0; i<array_size; i++)
			    *((float*)array + i) = resl.value.int_val - *((float*)array + i);
		    }else
			HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

		HGOTO_DONE(res)

	    }
	    else if( (resr.type == H5Z_XFORM_SYMBOL) && (resl.type==H5Z_XFORM_FLOAT))
            {

		res.type = H5Z_XFORM_SYMBOL;
		if(array_type == H5T_NATIVE_INT)
		{
		    for(i=0; i<array_size; i++)
			*((int*)array + i) = resl.value.float_val - *((int*)array + i);
		}
		else if(array_type == H5T_NATIVE_FLOAT)
		{
		    for(i=0; i<array_size; i++)
			*((float*)array + i) =  resl.value.float_val - *((float*)array + i);
		}else
		    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

			HGOTO_DONE(res)
	    }
            else
                 HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Unexpected type conversion operation")


            HGOTO_DONE(res) 
            break;


        case H5Z_XFORM_MULT:

	    if( (resl.type == H5Z_XFORM_SYMBOL) && (resr.type==H5Z_XFORM_INTEGER))
	    {
		res.type = H5Z_XFORM_SYMBOL; 
		if(array_type == H5T_NATIVE_INT)
		{
		    for(i=0; i<array_size; i++)
			*((int*)array + i) *=  resr.value.int_val;
		}
		else if(array_type == H5T_NATIVE_FLOAT)
		{
		    for(i=0; i<array_size; i++)
			*((float*)array + i) *=  resr.value.int_val;
		}else
		    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

			HGOTO_DONE(res)
	    }
	    else if( (resl.type == H5Z_XFORM_SYMBOL) && (resr.type==H5Z_XFORM_FLOAT)) 
	    {
		res.type = H5Z_XFORM_SYMBOL;
		if(array_type == H5T_NATIVE_INT)
		{
		    for(i=0; i<array_size; i++)
			*((int*)array + i) *=  resr.value.float_val;
		}
		else if(array_type == H5T_NATIVE_FLOAT)
		{
		    for(i=0; i<array_size; i++)
			*((float*)array + i) *= resr.value.float_val;
		}else
		    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

			HGOTO_DONE(res)
	    }
	    else if( (resr.type == H5Z_XFORM_SYMBOL) && (resl.type==H5Z_XFORM_INTEGER))
	    {
		res.type = H5Z_XFORM_SYMBOL; 
		if(array_type == H5T_NATIVE_INT)
		{
		    for(i=0; i<array_size; i++)
			*((int*)array + i) *= resl.value.int_val;
		}
		else if(array_type == H5T_NATIVE_FLOAT)
		{
		    for(i=0; i<array_size; i++)
			*((float*)array + i) *= resl.value.int_val;
		}else
		    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

			HGOTO_DONE(res)

	    }
	    else if( (resr.type == H5Z_XFORM_SYMBOL) && (resl.type==H5Z_XFORM_FLOAT)) 
	    {

		res.type = H5Z_XFORM_SYMBOL;
		if(array_type == H5T_NATIVE_INT)
		{
		    for(i=0; i<array_size; i++)
			*((int*)array + i) *= resl.value.float_val;
		}
		else if(array_type == H5T_NATIVE_FLOAT)
		{
		    for(i=0; i<array_size; i++)
			*((float*)array + i) *= resl.value.float_val;
		}else
		    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

			HGOTO_DONE(res)
	    }
	    else
		HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Unexpected type operation")

	    HGOTO_DONE(res) 
	    break;


	case H5Z_XFORM_DIVIDE: 

	    if( (resl.type == H5Z_XFORM_SYMBOL) && (resr.type==H5Z_XFORM_INTEGER))
	    {
		res.type = H5Z_XFORM_SYMBOL; 
		if(array_type == H5T_NATIVE_INT)
		{
		    for(i=0; i<array_size; i++)
			*((int*)array + i) /= resr.value.int_val;
		}
		else if(array_type == H5T_NATIVE_FLOAT)
		{
		    for(i=0; i<array_size; i++)
			*((float*)array + i) /= resr.value.int_val;
		}else
		    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

			HGOTO_DONE(res)
	    }
	    else if( (resl.type == H5Z_XFORM_SYMBOL) && (resr.type==H5Z_XFORM_FLOAT)) 
	    {
		res.type = H5Z_XFORM_SYMBOL;
		if(array_type == H5T_NATIVE_INT)
		{
		    for(i=0; i<array_size; i++)
			*((int*)array + i) /=  resr.value.float_val;
		}
		else if(array_type == H5T_NATIVE_FLOAT)
		{
		    for(i=0; i<array_size; i++)
			*((float*)array + i) /= resr.value.float_val;
		}else
		    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

                HGOTO_DONE(res)
            }
            else if( (resr.type == H5Z_XFORM_SYMBOL) && (resl.type==H5Z_XFORM_INTEGER))
            {
                res.type = H5Z_XFORM_SYMBOL; 
                {
                    if(array_type == H5T_NATIVE_INT)
		    {
			for(i=0; i<array_size; i++)
			    *((int*)array + i) = resl.value.int_val / *((int*)array + i);
		    }
		    else if(array_type == H5T_NATIVE_FLOAT)
		    {
			for(i=0; i<array_size; i++)
			    *((float*)array + i) =  resl.value.int_val / *((float*)array + i);
		    }else
			HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

		}
                HGOTO_DONE(res)

            }
            else if( (resr.type == H5Z_XFORM_SYMBOL) && (resl.type==H5Z_XFORM_FLOAT))
            {

                res.type = H5Z_XFORM_SYMBOL;
		{
		    if(array_type == H5T_NATIVE_INT)
		    {
			for(i=0; i<array_size; i++)
			    *((int*)array + i) = resl.value.float_val / *((int*)array + i);
		    }
		    else if(array_type == H5T_NATIVE_FLOAT)
		    {
			for(i=0; i<array_size; i++)
			    *((float*)array + i) = resl.value.float_val / *((float*)array + i);
		    }else
			HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Cannot perform a data transform on this type")

                }
                HGOTO_DONE(res)
            }
            else
             HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Unexpected type operation")

            HGOTO_DONE(res) 
            break;



        default: 
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, error, "Invalid expression tree")
        }
    }

done:
    
    FUNC_LEAVE_NOAPI(ret_value)

}



/*-------------------------------------------------------------------------
 * Function:    H5Z_find_type
 * Return:      Native type of datatype that is passed in
 * Programmer:  Leon Arber, 4/20/04
 * Modifications:
 *                      
 *
 *-------------------------------------------------------------------------
 */
static hid_t H5Z_xform_find_type(const H5T_t* type)
{
    hid_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5Z_xform_find_type);

    assert(type);

    /* Check for SHORT type */
    if((H5T_cmp(type, H5I_object_verify(H5T_NATIVE_SHORT,H5I_DATATYPE) ))==0)
	HGOTO_DONE(H5T_NATIVE_SHORT)

	    /* Check for INT type */
    else if((H5T_cmp(type,  H5I_object_verify(H5T_NATIVE_INT,H5I_DATATYPE)))==0)
	HGOTO_DONE(H5T_NATIVE_INT)

	    /* Check for LONG type */
    else if((H5T_cmp(type,  H5I_object_verify(H5T_NATIVE_LONG,H5I_DATATYPE)))==0)
	HGOTO_DONE(H5T_NATIVE_LONG)

	    /* Check for LONGLONG type */
    else if((H5T_cmp(type,  H5I_object_verify(H5T_NATIVE_LLONG,H5I_DATATYPE)))==0)
	HGOTO_DONE(H5T_NATIVE_LLONG)

	    /* Check for UCHAR type */
    else if((H5T_cmp(type,  H5I_object_verify(H5T_NATIVE_UCHAR,H5I_DATATYPE)))==0)
	HGOTO_DONE(H5T_NATIVE_UCHAR)

	    /* Check for USHORT type */
    else if((H5T_cmp(type,  H5I_object_verify(H5T_NATIVE_USHORT,H5I_DATATYPE)))==0)
	HGOTO_DONE(H5T_NATIVE_USHORT)

	    /* Check for UINT type */
    else if((H5T_cmp(type,  H5I_object_verify(H5T_NATIVE_UINT,H5I_DATATYPE)))==0)
	HGOTO_DONE(H5T_NATIVE_UINT)

	    /* Check for ULONG type */
    else if((H5T_cmp(type,  H5I_object_verify(H5T_NATIVE_ULONG,H5I_DATATYPE)))==0)
	HGOTO_DONE(H5T_NATIVE_ULONG)

	    /* Check for ULONGLONG type */
    else if((H5T_cmp(type,  H5I_object_verify(H5T_NATIVE_ULLONG,H5I_DATATYPE)))==0)
	HGOTO_DONE(H5T_NATIVE_ULLONG)

	    /* Check for FLOAT type */
    else if((H5T_cmp(type,  H5I_object_verify(H5T_NATIVE_FLOAT,H5I_DATATYPE)))==0)
	HGOTO_DONE(H5T_NATIVE_FLOAT)

	    /* Check for DOUBLE type */
    else if((H5T_cmp(type,  H5I_object_verify(H5T_NATIVE_DOUBLE,H5I_DATATYPE)))==0)
	HGOTO_DONE(H5T_NATIVE_DOUBLE)


	    /* Check for LONGDOUBLE type */
    else if((H5T_cmp(type,  H5I_object_verify(H5T_NATIVE_LDOUBLE,H5I_DATATYPE)))==0)
	HGOTO_DONE(H5T_NATIVE_LDOUBLE)
    else
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "could not find matching type");


done:
    FUNC_LEAVE_NOAPI(ret_value)

}


/*-------------------------------------------------------------------------
 * Function:    H5Z_xform_copy_tree
 * Purpose:     Makes a copy of the parse tree passed in.
 * Return:      A pointer to a root for a new parse tree which is a copy
 *              of the one passed in.
 * Programmer:  Leon Arber
 *              April 1, 2004.
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void* H5Z_xform_copy_tree(H5Z_node* tree)
{ 
    H5Z_node* ret_value=NULL;

    FUNC_ENTER_NOAPI(H5Z_xform_copy_tree, NULL)
   
    assert(tree);   

    if(tree->type == H5Z_XFORM_INTEGER)
    {
        if ((ret_value = (H5Z_node*) H5MM_malloc(sizeof(H5Z_node))) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Ran out of memory trying to copy parse tree")
        else
        {
            ret_value -> type = H5Z_XFORM_INTEGER;
            ret_value ->value.int_val = tree->value.int_val;
        }
    }
    else if (tree->type == H5Z_XFORM_FLOAT)
    {   
        if ((ret_value = (H5Z_node*) H5MM_malloc(sizeof(H5Z_node))) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Ran out of memory trying to copy parse tree")
        else
        {   
            ret_value -> type = H5Z_XFORM_FLOAT;
            ret_value ->value.float_val = tree->value.float_val;
        }   
    }
    else if(tree->type == H5Z_XFORM_SYMBOL)
    {   
        if ((ret_value = (H5Z_node*) H5MM_malloc(sizeof(H5Z_node))) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Ran out of memory trying to copy parse tree")
        else
        {   
            ret_value -> type = H5Z_XFORM_SYMBOL;
            if ((ret_value->value.sym_val = (char*) H5MM_malloc(strlen(tree->value.sym_val)+1)) == NULL)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Ran out of memory trying to copy parse tree")
            else 
                strcpy(ret_value ->value.sym_val, tree->value.sym_val);
        }   
    }   
    else if(tree->type == H5Z_XFORM_MULT)
    {
        if ((ret_value = (H5Z_node*) H5MM_malloc(sizeof(H5Z_node))) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Ran out of memory trying to copy parse tree")
        else
        {
            ret_value->type = H5Z_XFORM_MULT;
            ret_value->lchild = (H5Z_node*) H5Z_xform_copy_tree(tree->lchild);
            ret_value->rchild = (H5Z_node*) H5Z_xform_copy_tree(tree->rchild);
        }
    }
    else if(tree->type == H5Z_XFORM_PLUS)
    {
        if ((ret_value = (H5Z_node*) H5MM_malloc(sizeof(H5Z_node))) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Ran out of memory trying to copy parse tree")
        else
        {
            ret_value->type = H5Z_XFORM_PLUS;
            ret_value->lchild = (H5Z_node*) H5Z_xform_copy_tree(tree->lchild);
            ret_value->rchild = (H5Z_node*) H5Z_xform_copy_tree(tree->rchild);
        }
    }
    else if(tree->type == H5Z_XFORM_MINUS)
    {
        if ((ret_value = (H5Z_node*) H5MM_malloc(sizeof(H5Z_node))) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Ran out of memory trying to copy parse tree")
        else
        {
            ret_value->type = H5Z_XFORM_MINUS;
            ret_value->lchild = (H5Z_node*) H5Z_xform_copy_tree(tree->lchild);
            ret_value->rchild = (H5Z_node*) H5Z_xform_copy_tree(tree->rchild);
        }
    }
    else if(tree->type == H5Z_XFORM_DIVIDE)
    {
        if ((ret_value = (H5Z_node*) H5MM_malloc(sizeof(H5Z_node))) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Ran out of memory trying to copy parse tree")
        else
        {
            ret_value->type = H5Z_XFORM_DIVIDE;
            ret_value->lchild = (H5Z_node*) H5Z_xform_copy_tree(tree->lchild);
            ret_value->rchild = (H5Z_node*) H5Z_xform_copy_tree(tree->rchild);
        }
    }   
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Error in parse tree while trying to copy")



            done:
            FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5Z_xform_reduce_tree
 * Purpose:     Simplifies parse tree passed in by performing any obvious
 *              and trivial arithemtic calculations.
 *
 * Return:      None.
 * Programmer:  Leon Arber
 *              April 1, 2004.
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void H5Z_xform_reduce_tree(H5Z_node* tree)
{
    hid_t ret_value = SUCCEED;
    
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5Z_xform_reduce_tree)
 
        if(!tree)
            HGOTO_DONE(SUCCEED)

    if((tree->type == H5Z_XFORM_PLUS) || (tree->type == H5Z_XFORM_DIVIDE) ||(tree->type == H5Z_XFORM_MULT) ||(tree->type == H5Z_XFORM_MINUS))
    {
        if(((tree->lchild->type == H5Z_XFORM_INTEGER) || (tree->lchild->type == H5Z_XFORM_FLOAT)) && ((tree->rchild->type == H5Z_XFORM_INTEGER) || (tree->rchild->type == H5Z_XFORM_FLOAT)))
            H5Z_do_op(tree);
        else
        {
            H5Z_xform_reduce_tree(tree->lchild);
            if(((tree->lchild->type == H5Z_XFORM_INTEGER) || (tree->lchild->type == H5Z_XFORM_FLOAT)) && ((tree->rchild->type == H5Z_XFORM_INTEGER) || (tree->rchild->type == H5Z_XFORM_FLOAT)))
	    {
	       	H5Z_do_op(tree);
		HGOTO_DONE(SUCCEED)
	    }

            H5Z_xform_reduce_tree(tree->rchild);
            if(((tree->lchild->type == H5Z_XFORM_INTEGER) || (tree->lchild->type == H5Z_XFORM_FLOAT)) && ((tree->rchild->type == H5Z_XFORM_INTEGER) || (tree->rchild->type == H5Z_XFORM_FLOAT)))
	    {
	     	H5Z_do_op(tree);
		HGOTO_DONE(SUCCEED)
	    }
        }
    }

done:
    FUNC_LEAVE_NOAPI_VOID;
}


/*-------------------------------------------------------------------------
 * Function:    H5Z_do_op
 * Purpose:     If the root of the tree passed in points to a simple
 *              arithmetic operation and the left and right subtrees are both
 *              integer or floating point values, this function does that
 *              operation, free the left and rigt subtrees, and replaces
 *              the root with the result of the operation.
 * Return:      None.
 * Programmer:  Leon Arber
 *              April 1, 2004.
 * Modifications:
*
 *-------------------------------------------------------------------------
 */
static void H5Z_do_op(H5Z_node* tree)
{
 
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5Z_do_op)
 

    if(tree->type == H5Z_XFORM_DIVIDE)
    {
        if((tree->lchild->type == H5Z_XFORM_INTEGER) && (tree->rchild->type==H5Z_XFORM_INTEGER))
        {
            tree->type = H5Z_XFORM_INTEGER;
            tree->value.int_val = tree->lchild->value.int_val / tree->rchild->value.int_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if((tree->lchild->type == H5Z_XFORM_FLOAT) && (tree->rchild->type == H5Z_XFORM_FLOAT))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.float_val / tree->rchild->value.float_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if( (tree->lchild->type == H5Z_XFORM_FLOAT) && (tree->rchild->type==H5Z_XFORM_INTEGER))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.float_val / tree->rchild->value.int_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if( (tree->lchild->type == H5Z_XFORM_INTEGER) && (tree->rchild->type == H5Z_XFORM_FLOAT))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.int_val / tree->rchild->value.float_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }

    }
    else if(tree->type == H5Z_XFORM_MULT)
    {


        if((tree->lchild->type == H5Z_XFORM_INTEGER) && (tree->rchild->type==H5Z_XFORM_INTEGER))
        {
            tree->type = H5Z_XFORM_INTEGER;
            tree->value.int_val = tree->lchild->value.int_val * tree->rchild->value.int_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if((tree->lchild->type == H5Z_XFORM_FLOAT) && (tree->rchild->type == H5Z_XFORM_FLOAT))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.float_val * tree->rchild->value.float_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if( (tree->lchild->type == H5Z_XFORM_FLOAT) && (tree->rchild->type==H5Z_XFORM_INTEGER))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.float_val * tree->rchild->value.int_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if( (tree->lchild->type == H5Z_XFORM_INTEGER) && (tree->rchild->type == H5Z_XFORM_FLOAT))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.int_val * tree->rchild->value.float_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }

    }
    else if(tree->type == H5Z_XFORM_PLUS)
    {


        if((tree->lchild->type == H5Z_XFORM_INTEGER) && (tree->rchild->type==H5Z_XFORM_INTEGER))
        {
            tree->type = H5Z_XFORM_INTEGER;
            tree->value.int_val = tree->lchild->value.int_val + tree->rchild->value.int_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if((tree->lchild->type == H5Z_XFORM_FLOAT) && (tree->rchild->type == H5Z_XFORM_FLOAT))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.float_val + tree->rchild->value.float_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if( (tree->lchild->type == H5Z_XFORM_FLOAT) && (tree->rchild->type==H5Z_XFORM_INTEGER))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.float_val + tree->rchild->value.int_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if( (tree->lchild->type == H5Z_XFORM_INTEGER) && (tree->rchild->type == H5Z_XFORM_FLOAT))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.int_val + tree->rchild->value.float_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }

    }
    else if(tree->type == H5Z_XFORM_MINUS)
    {


        if((tree->lchild->type == H5Z_XFORM_INTEGER) && (tree->rchild->type==H5Z_XFORM_INTEGER))
        {
            tree->type = H5Z_XFORM_INTEGER;
            tree->value.int_val = tree->lchild->value.int_val - tree->rchild->value.int_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if((tree->lchild->type == H5Z_XFORM_FLOAT) && (tree->rchild->type == H5Z_XFORM_FLOAT))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.float_val - tree->rchild->value.float_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if( (tree->lchild->type == H5Z_XFORM_FLOAT) && (tree->rchild->type==H5Z_XFORM_INTEGER))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.float_val - tree->rchild->value.int_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
        else if( (tree->lchild->type == H5Z_XFORM_INTEGER) && (tree->rchild->type == H5Z_XFORM_FLOAT))
        {
            tree->type = H5Z_XFORM_FLOAT;
            tree->value.float_val = tree->lchild->value.int_val - tree->rchild->value.float_val;
            H5MM_xfree(tree->lchild);
            H5MM_xfree(tree->rchild);
            tree->lchild = NULL;
            tree->rchild = NULL;
        }
    }
 
    FUNC_LEAVE_NOAPI_VOID;
              
}


/*-------------------------------------------------------------------------
 * Function: H5D_xform_create
 *
 * Purpose: Create a new data transform object from a string.
 *
 * Return:
 *      Success: SUCCEED
 *      Failure: FAIL
 *
 * Programmer: Quincey Koziol, koziol@ncsa.uiuc.edu
 *
 * Date: May 4, 2004
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5Z_data_xform_t *
H5Z_xform_create(const char *expr)
{
    H5Z_data_xform_t *data_xform_prop=NULL;
    H5Z_data_xform_t *ret_value;

    FUNC_ENTER_NOAPI(H5Z_xform_create, NULL)

    assert(expr);

    /* Allocate space for the data transform information */
    if((data_xform_prop = H5MM_calloc(sizeof(H5Z_data_xform_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "unable to allocate memory for data transform info")
   
    /* copy the user's string into the property */
    if((data_xform_prop->xform_exp = H5MM_xstrdup(expr))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "unable to allocate memory for data transform expression")
   
    /* we generate the parse tree right here and store a poitner to its root in the property. */
    if((data_xform_prop->parse_root = H5Z_xform_parse(expr))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "unable to allocate memory for data transform parse tree")

    /* Assign return value */
    ret_value=data_xform_prop;

done:
    /* Clean up on error */
    if(ret_value==NULL) {
        if(data_xform_prop) {
            if(data_xform_prop->parse_root)
                H5Z_xform_destroy_parse_tree(data_xform_prop->parse_root);
            if(data_xform_prop->xform_exp)
                H5MM_xfree(data_xform_prop->xform_exp);
            H5MM_xfree(data_xform_prop);
        } /* end if */
    } /* end if */
    
    FUNC_LEAVE_NOAPI(ret_value)  
} /* H5Z_xform_create() */


/*-------------------------------------------------------------------------
 * Function: H5Z_xform_destroy
 *
 * Purpose: Destroy a data transform object.
 *
 * Return:
 *      Success: SUCCEED
 *      Failure: FAIL
 *
 * Programmer: Quincey Koziol, koziol@ncsa.uiuc.edu
 *
 * Date: May 4, 2004
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Z_xform_destroy(H5Z_data_xform_t *data_xform_prop)
{
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5Z_xform_destroy, FAIL)

    if(data_xform_prop) {
        /* Destroy the parse tree */
        H5Z_xform_destroy_parse_tree(data_xform_prop->parse_root);

        /* Free the expression */
        H5MM_xfree(data_xform_prop->xform_exp);

        /* Free the node */
        H5MM_xfree(data_xform_prop);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)  
} /* H5Z_xform_destroy() */


/*-------------------------------------------------------------------------
 * Function: H5Z_xform_copy
 *
 * Purpose: Clone a data transform object.
 *
 * Return:
 *      Success: SUCCEED
 *      Failure: FAIL
 *
 * Programmer: Quincey Koziol, koziol@ncsa.uiuc.edu
 *
 * Date: May 4, 2004
 *
 * Comments: This is an "in-place" copy, since this routine gets called
 *      after the top-level copy has been performed and this routine finishes
 *      the "deep" part of the copy.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Z_xform_copy(H5Z_data_xform_t **data_xform_prop)
{
    H5Z_data_xform_t *new_data_xform_prop;
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5Z_xform_copy, FAIL)

    if(*data_xform_prop) {
        /* Allocate new node */
        if((new_data_xform_prop = H5MM_calloc(sizeof(H5Z_data_xform_t)))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate memory for data transform info")

        /* Copy string */
        if((new_data_xform_prop->xform_exp = H5MM_xstrdup((*data_xform_prop)->xform_exp))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate memory for data transform expression")

        /* Copy parse tree */
        if((new_data_xform_prop->parse_root = (H5Z_node*)H5Z_xform_copy_tree((*data_xform_prop)->parse_root)) == NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "error copying the parse tree")

        /* Copy new information on top of old information */
        *data_xform_prop=new_data_xform_prop;
    } /* end if */

done:
    /* Clean up on error */
    if(ret_value<0) {
        if(new_data_xform_prop) {
            if(new_data_xform_prop->parse_root)
                H5Z_xform_destroy_parse_tree(new_data_xform_prop->parse_root);
            if(new_data_xform_prop->xform_exp)
                H5MM_xfree(new_data_xform_prop->xform_exp);
            H5MM_xfree(new_data_xform_prop);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)  
} /* H5Z_xform_copy() */


/*-------------------------------------------------------------------------
 * Function: H5Z_xform_noop
 *
 * Purpose: Checks if a data transform will be performed
 *
 * Return:  TRUE for no data transform, FALSE for a data transform
 *
 * Programmer: Quincey Koziol, koziol@ncsa.uiuc.edu
 *
 * Date: May 4, 2004
 *
 * Comments: Can't fail
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5Z_xform_noop(const H5Z_data_xform_t *data_xform_prop)
{
    hbool_t ret_value;

    FUNC_ENTER_NOAPI(H5Z_xform_noop, TRUE)

    ret_value=(data_xform_prop ? FALSE : TRUE);

done:
    FUNC_LEAVE_NOAPI(ret_value)  
} /* H5Z_xform_noop() */

