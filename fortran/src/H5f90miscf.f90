      SUBROUTINE h5init_types_f(error)
        USE H5GLOBAL

        IMPLICIT NONE
        INTEGER, INTENT(OUT) :: error
        INTEGER, EXTERNAL :: h5init_types_c
        error = h5init_types_c(predef_types, floating_types, integer_types)

      END SUBROUTINE h5init_types_f

      SUBROUTINE h5close_types_f(error)
        USE H5GLOBAL

        IMPLICIT NONE
        INTEGER, INTENT(OUT) :: error
        INTEGER, EXTERNAL :: h5close_types_c
        error = h5close_types_c(predef_types, PREDEF_TYPES_LEN, &
                                floating_types, FLOATING_TYPES_LEN, &
                                integer_types, INTEGER_TYPES_LEN )

      END SUBROUTINE h5close_types_f
        
