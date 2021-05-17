/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        // show how to register a new ID type and how
        // to create an identifier for an associated item
    } //! <!-- [create] -->

    //! <!-- [read] -->
    {
        // show how to retrieve information about an identifier
        // or iterate over identifiers of a given type
    } //! <!-- [read] -->

    //! <!-- [update] -->
    {
        // show how to update an identifier (e.g., bump the reference)
    } //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        // show how to remove and ID from its type or unregister an ID type
    }
    //! <!-- [delete] -->

    return ret_val;
}
