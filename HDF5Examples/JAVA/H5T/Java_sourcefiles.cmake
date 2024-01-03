#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (HDF_JAVA_EXAMPLES
    H5Ex_T_Array.java
    H5Ex_T_ArrayAttribute.java
    H5Ex_T_Bit.java
    H5Ex_T_BitAttribute.java
    H5Ex_T_Commit.java
    H5Ex_T_Compound.java
    H5Ex_T_CompoundAttribute.java
    H5Ex_T_Float.java
    H5Ex_T_FloatAttribute.java
    H5Ex_T_Integer.java
    H5Ex_T_IntegerAttribute.java
    H5Ex_T_Opaque.java
    H5Ex_T_OpaqueAttribute.java
    H5Ex_T_String.java
    H5Ex_T_StringAttribute.java
    H5Ex_T_VLString.java
)
if (${H5_LIBVER_DIR} GREATER 18)
  if (${H5_LIBVER_DIR} EQUAL 110)
    set (HDF_JAVA_EXAMPLES ${HDF_JAVA_EXAMPLES}
        110/H5Ex_T_ObjectReference.java
        110/H5Ex_T_ObjectReferenceAttribute.java
    )
  else ()
    set (HDF_JAVA_EXAMPLES ${HDF_JAVA_EXAMPLES}
        H5Ex_T_ObjectReference.java
        H5Ex_T_ObjectReferenceAttribute.java
        H5Ex_T_RegionReference.java
        H5Ex_T_RegionReferenceAttribute.java
    )
  endif ()
endif ()
