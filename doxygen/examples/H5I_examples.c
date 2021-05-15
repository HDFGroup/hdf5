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
        // show how to register a new ID type (integers) and how
        // to create an identifier for an integer
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        // show how to retrieve information about an identifier
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        // show how to update an identifier (bump the reference count?)
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        // show how to unregister an ID type
    }
    //! <!-- [delete] -->

    return ret_val;
}
