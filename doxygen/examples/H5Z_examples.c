/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

//! <!-- [filter] -->
size_t
filter(unsigned int flags, size_t cd_nelmts, const unsigned int cd_values[], size_t nbytes, size_t *buf_size,
       void **buf)
{
    buf_size = 0;

    if (flags & H5Z_FLAG_REVERSE) {
        // read data, e.g., decompress data
        // ...
    }
    else {
        // write data, e.g., compress data
        // ...
    }

    return nbytes;
}
//! <!-- [filter] -->

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        __label__ fail_register;
        H5Z_class_t cls;
        cls.version         = H5Z_CLASS_T_VERS;
        cls.id              = 256;
        cls.encoder_present = 1;
        cls.decoder_present = 1;
        cls.name            = "Identity filter";
        cls.can_apply       = NULL;
        cls.set_local       = NULL;
        cls.filter          = &filter;

        // register the filter
        if (H5Zregister(&cls) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_register;
        }

        // do something with filter
        // ...

        // unregister the filter if no longer required
        // ...

fail_register:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_avail;

        H5Z_filter_t flt   = H5Z_FILTER_DEFLATE;
        unsigned     flags = 0;

        // check if the deflate filter is available
        if (H5Zfilter_avail(flt) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_avail;
        }
        // retrieve the deflate filter info
        if (H5Zget_filter_info(flt, &flags) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_avail;
        }

        // check if the deflate encoder or decoder is enabled
        if (H5Z_FILTER_CONFIG_ENCODE_ENABLED & flags)
            printf("Deflate encoder enabled.\n");
        if (H5Z_FILTER_CONFIG_DECODE_ENABLED & flags)
            printf("Deflate decoder enabled.\n");

fail_avail:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        // N/A
    } //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        // unregister the identity filter
        if (H5Zunregister(256) < 0) {
            ret_val = EXIT_FAILURE;
        }
    }
    //! <!-- [delete] -->

    assert(ret_val == EXIT_SUCCESS);

    return ret_val;
}
