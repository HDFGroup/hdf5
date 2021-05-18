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
        // show how to create a link to an object
        // show how to copy an object
    } //! <!-- [create] -->

    //! <!-- [read] -->
    {
        // show how to retrieve information about an object
        // show how to visit objects
    } //! <!-- [read] -->

    //! <!-- [update] -->
    {
        // show how to refresh all buffers associated with an object
        // show how to increment an object's reference count(?)
    } //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        // show how to decrement an object's reference count
    }
    //! <!-- [delete] -->

    return ret_val;
}
