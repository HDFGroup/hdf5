/*

Copyright (c) 2005-2008, Simon Howard

Permission to use, copy, modify, and/or distribute this software
for any purpose with or without fee is hereby granted, provided
that the above copyright notice and this permission notice appear
in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

 */

/**
 * \file mercury_hash_table.h
 *
 * \brief Hash table.
 *
 * A hash table stores a set of values which can be addressed by a
 * key.  Given the key, the corresponding value can be looked up
 * quickly.
 *
 * To create a hash table, use \ref hg_hash_table_new. To destroy a
 * hash table, use \ref hg_hash_table_free.
 *
 * To insert a value into a hash table, use \ref hg_hash_table_insert.
 *
 * To remove a value from a hash table, use \ref hg_hash_table_remove.
 *
 * To look up a value by its key, use \ref hg_hash_table_lookup.
 *
 * To iterate over all values in a hash table, use
 * \ref hg_hash_table_iterate to initialize a \ref hg_hash_table_iter
 * structure.  Each value can then be read in turn using
 * \ref hg_hash_table_iter_next and \ref hg_hash_table_iter_has_more.
 */

#ifndef HG_HASH_TABLE_H
#define HG_HASH_TABLE_H

#include "mercury_util_config.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * A hash table structure.
 */

typedef struct hg_hash_table hg_hash_table_t;

/**
 * Structure used to iterate over a hash table.
 */

typedef struct hg_hash_table_iter hg_hash_table_iter_t;

/**
 * Internal structure representing an entry in a hash table.
 */

typedef struct hg_hash_table_entry hg_hash_table_entry_t;

/**
 * A key to look up a value in a \ref hg_hash_table_t.
 */

typedef void *hg_hash_table_key_t;

/**
 * A value stored in a \ref hg_hash_table_t.
 */

typedef void *hg_hash_table_value_t;

/**
 * Definition of a \ref hg_hash_table_iter.
 */

struct hg_hash_table_iter {
    hg_hash_table_t *hash_table;
    hg_hash_table_entry_t *next_entry;
    unsigned int next_chain;
};

/**
 * A null \ref HashTableValue.
 */

#define HG_HASH_TABLE_NULL ((void *) 0)

/**
 * Hash function used to generate hash values for keys used in a hash
 * table.
 *
 * \param value  The value to generate a hash value for.
 * \return       The hash value.
 */

typedef unsigned int (*hg_hash_table_hash_func_t)(hg_hash_table_key_t value);

/**
 * Function used to compare two keys for equality.
 *
 * \return   Non-zero if the two keys are equal, zero if the keys are
 *           not equal.
 */

typedef int (*hg_hash_table_equal_func_t)(
    hg_hash_table_key_t value1, hg_hash_table_key_t value2);

/**
 * Type of function used to free keys when entries are removed from a
 * hash table.
 */

typedef void (*hg_hash_table_key_free_func_t)(hg_hash_table_key_t value);

/**
 * Type of function used to free values when entries are removed from a
 * hash table.
 */

typedef void (*hg_hash_table_value_free_func_t)(hg_hash_table_value_t value);

/**
 * Create a new hash table.
 *
 * \param hash_func            Function used to generate hash keys for the
 *                             keys used in the table.
 * \param equal_func           Function used to test keys used in the table
 *                             for equality.
 * \return                     A new hash table structure, or NULL if it
 *                             was not possible to allocate the new hash
 *                             table.
 */
HG_UTIL_PUBLIC hg_hash_table_t *
hg_hash_table_new(
    hg_hash_table_hash_func_t hash_func, hg_hash_table_equal_func_t equal_func);

/**
 * Destroy a hash table.
 *
 * \param hash_table           The hash table to destroy.
 */
HG_UTIL_PUBLIC void
hg_hash_table_free(hg_hash_table_t *hash_table);

/**
 * Register functions used to free the key and value when an entry is
 * removed from a hash table.
 *
 * \param hash_table           The hash table.
 * \param key_free_func        Function used to free keys.
 * \param value_free_func      Function used to free values.
 */
HG_UTIL_PUBLIC void
hg_hash_table_register_free_functions(hg_hash_table_t *hash_table,
    hg_hash_table_key_free_func_t key_free_func,
    hg_hash_table_value_free_func_t value_free_func);

/**
 * Insert a value into a hash table, overwriting any existing entry
 * using the same key.
 *
 * \param hash_table           The hash table.
 * \param key                  The key for the new value.
 * \param value                The value to insert.
 * \return                     Non-zero if the value was added successfully,
 *                             or zero if it was not possible to allocate
 *                             memory for the new entry.
 */
HG_UTIL_PUBLIC int
hg_hash_table_insert(hg_hash_table_t *hash_table, hg_hash_table_key_t key,
    hg_hash_table_value_t value);

/**
 * Look up a value in a hash table by key.
 *
 * \param hash_table          The hash table.
 * \param key                 The key of the value to look up.
 * \return                    The value, or \ref HASH_TABLE_NULL if there
 *                            is no value with that key in the hash table.
 */
HG_UTIL_PUBLIC hg_hash_table_value_t
hg_hash_table_lookup(hg_hash_table_t *hash_table, hg_hash_table_key_t key);

/**
 * Remove a value from a hash table.
 *
 * \param hash_table          The hash table.
 * \param key                 The key of the value to remove.
 * \return                    Non-zero if a key was removed, or zero if the
 *                            specified key was not found in the hash table.
 */
HG_UTIL_PUBLIC int
hg_hash_table_remove(hg_hash_table_t *hash_table, hg_hash_table_key_t key);

/**
 * Retrieve the number of entries in a hash table.
 *
 * \param hash_table          The hash table.
 * \return                    The number of entries in the hash table.
 */
HG_UTIL_PUBLIC unsigned int
hg_hash_table_num_entries(hg_hash_table_t *hash_table);

/**
 * Initialise a \ref HashTableIterator to iterate over a hash table.
 *
 * \param hash_table          The hash table.
 * \param iter                Pointer to an iterator structure to
 *                            initialise.
 */
HG_UTIL_PUBLIC void
hg_hash_table_iterate(hg_hash_table_t *hash_table, hg_hash_table_iter_t *iter);

/**
 * Determine if there are more keys in the hash table to iterate over.
 *
 * \param iterator            The hash table iterator.
 * \return                    Zero if there are no more values to iterate
 *                            over, non-zero if there are more values to
 *                            iterate over.
 */
HG_UTIL_PUBLIC int
hg_hash_table_iter_has_more(hg_hash_table_iter_t *iterator);

/**
 * Using a hash table iterator, retrieve the next key.
 *
 * \param iterator            The hash table iterator.
 * \return                    The next key from the hash table, or
 *                            \ref HG_HASH_TABLE_NULL if there are no more
 *                            keys to iterate over.
 */
HG_UTIL_PUBLIC hg_hash_table_value_t
hg_hash_table_iter_next(hg_hash_table_iter_t *iterator);

#ifdef __cplusplus
}
#endif

#endif /* HG_HASH_TABLE_H */
