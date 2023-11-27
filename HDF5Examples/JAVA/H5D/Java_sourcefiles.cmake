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
    H5Ex_D_UnlimitedAdd.java
    H5Ex_D_UnlimitedMod.java
    H5Ex_D_Nbit.java
    H5Ex_D_Transform.java
    H5Ex_D_Sofloat.java
    H5Ex_D_Soint.java
)

set (HDF_JAVA_ZLIB_EXAMPLES
    H5Ex_D_Gzip.java
    H5Ex_D_Shuffle.java
    H5Ex_D_UnlimitedGzip.java
)

set (HDF_JAVA_SZIP_EXAMPLES
    H5Ex_D_Szip.java
)

# detect whether the encoder is present.
  if (${HDF5_ENABLE_Z_LIB_SUPPORT})
    set (HDF_JAVA_EXAMPLES ${HDF_JAVA_EXAMPLES} ${HDF_JAVA_ZLIB_EXAMPLES})
  endif ()

  if (${HDF5_ENABLE_SZIP_SUPPORT})
    set (HDF_JAVA_EXAMPLES ${HDF_JAVA_EXAMPLES} ${HDF_JAVA_SZIP_EXAMPLES})
  endif ()
