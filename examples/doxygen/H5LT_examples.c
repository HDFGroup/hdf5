/* -*- c-file-style: "stroustrup" -*- */

//! <!-- [get_attribute_info] -->

H5T_class_t type_class;
size_t      type_size;
hsize_t     dims[0];
... status = H5LTget_attribute_info(file_id, "/", STRNAME, dims, &type_class, &type_size);
if (type_class == H5T_STRING) {
    printf("Attribute is a string.\n");
    printf("String size: %i\n", type_size);

    //! <!-- [get_attribute_info] -->

    //! <!-- [enum] -->

    “H5T_ENUM
    {
        H5T_NATIVE_INT;
        “Bob” 0;
        “Elena” 1;
        “Quincey” 2;
        “Frank” 3;
    }
    ”

//! <!-- [enum] -->
