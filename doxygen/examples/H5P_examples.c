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
        // show how to create a property list and a property list class
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        // show how to retrieve properties from a property list
        // show how to iterate over the properties in a property list
        // and a property class
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        // show how to add properties to a property list
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        // show how to delete a property from a property list
        // show how to unregister a property list class
    }
    //! <!-- [delete] -->

    return ret_val;
}
