#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (HDF_JAVA_EXAMPLES
    H5Ex_D_Alloc.java
    H5Ex_D_Checksum.java
    H5Ex_D_Chunk.java
    H5Ex_D_Compact.java
    H5Ex_D_External.java
    H5Ex_D_FillValue.java
    H5Ex_D_Hyperslab.java
    H5Ex_D_ReadWrite.java
    H5Ex_D_Nbit.java
    H5Ex_D_Transform.java
    H5Ex_D_Sofloat.java
    H5Ex_D_Soint.java
)

# Unlimited examples have known FFM memory issues - only include with JNI
if (HDF5_PROVIDES_JNI)
    set (HDF_JAVA_EXAMPLES ${HDF_JAVA_EXAMPLES}
        H5Ex_D_UnlimitedAdd.java
        H5Ex_D_UnlimitedMod.java
    )
endif ()

set (HDF_JAVA_ZLIB_EXAMPLES
    H5Ex_D_Gzip.java
    H5Ex_D_Shuffle.java
)

# UnlimitedGzip has known FFM memory issues - only include with JNI
if (HDF5_PROVIDES_JNI)
    set (HDF_JAVA_ZLIB_EXAMPLES ${HDF_JAVA_ZLIB_EXAMPLES}
        H5Ex_D_UnlimitedGzip.java
    )
endif ()

set (HDF_JAVA_SZIP_EXAMPLES
    H5Ex_D_Szip.java
)

# detect whether the encoder is present.
  if (${HDF5_PROVIDES_ZLIB_SUPPORT})
    set (HDF_JAVA_EXAMPLES ${HDF_JAVA_EXAMPLES} ${HDF_JAVA_ZLIB_EXAMPLES})
  endif ()

  if (${HDF5_PROVIDES_SZIP_SUPPORT})
    set (HDF_JAVA_EXAMPLES ${HDF_JAVA_EXAMPLES} ${HDF_JAVA_SZIP_EXAMPLES})
  endif ()
