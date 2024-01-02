#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (HDF_JAVA_EXAMPLES
    H5Ex_G_Create.java
    H5Ex_G_Compact.java
    H5Ex_G_Corder.java
    H5Ex_G_Phase.java
)
if (${H5_LIBVER_DIR} GREATER 18)
  if ((H5_LIBVER_DIR EQUAL 110) AND NOT (${EXAMPLE_VARNAME}_USE_16_API OR ${EXAMPLE_VARNAME}_USE_18_API OR ${EXAMPLE_VARNAME}_USE_110_API))
    set (HDF_JAVA_EXAMPLES ${HDF_JAVA_EXAMPLES}
        110/H5Ex_G_Iterate.java
        110/H5Ex_G_Intermediate.java
        110/H5Ex_G_Visit.java
    )
  else ()
    set (HDF_JAVA_EXAMPLES ${HDF_JAVA_EXAMPLES}
        H5Ex_G_Iterate.java
        H5Ex_G_Intermediate.java
        H5Ex_G_Visit.java
    )
  endif ()
endif ()
