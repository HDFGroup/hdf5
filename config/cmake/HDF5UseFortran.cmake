
## Check for non-standard extenstion quadmath.h

CHECK_INCLUDE_FILES(quadmath.h C_HAVE_QUADMATH)

if (${C_HAVE_QUADMATH})
  set(HAVE_QUADMATH 1)
else ()
  set(HAVE_QUADMATH 0)
endif()

#
# This file provides functions for HDF5 specific Fortran support.
#
#-------------------------------------------------------------------------------
ENABLE_LANGUAGE (Fortran)

# The provided CMake Fortran macros don't provide a general compile/run function
# so this one is used.
#-----------------------------------------------------------------------------
MACRO (FORTRAN_RUN FUNCTION CODE RUN_RESULT_VAR1 COMPILE_RESULT_VAR RETURN)
# MSB CHECK WHY THIS CHECK?
#  if (NOT DEFINED ${RUN_RESULT_VAR}) 
    message (STATUS "Detecting Fortran ${FUNCTION}")
    if (CMAKE_REQUIRED_LIBRARIES)
      set (CHECK_FUNCTION_EXISTS_ADD_LIBRARIES
          "-DLINK_LIBRARIES:STRING=${CMAKE_REQUIRED_LIBRARIES}")
    else (CMAKE_REQUIRED_LIBRARIES)
      set (CHECK_FUNCTION_EXISTS_ADD_LIBRARIES)
    endif (CMAKE_REQUIRED_LIBRARIES)
    file (WRITE
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler1.f90
        "${CODE}"
    )
    TRY_RUN (RUN_RESULT_VAR COMPILE_RESULT_VAR
        ${CMAKE_BINARY_DIR}
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler1.f90
        CMAKE_FLAGS "${CHECK_FUNCTION_EXISTS_ADD_LIBRARIES}"
        RUN_OUTPUT_VARIABLE OUTPUT
    )

	

    set(${RETURN} ${OUTPUT})
	
    #message ( "Test result1 ${RETURN} ")
    #message ( "Test result3 ${RESULT} ")
    #message ( "Test result2 ${CMAKE_MATCH_0} ")
    #message ( "Test result4 ${CMAKE_MATCH_1} ")
    #message ( "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
    #message ( "Test result2 ${COMPILE_RESULT_VAR} ")
    #message ( "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
    #message ( "Test result1 ${RUN_RESULT_VAR} ")
    #message ( "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")

    if (${COMPILE_RESULT_VAR})
      if (${RUN_RESULT_VAR} MATCHES 0)
        message (STATUS "Testing Fortran ${FUNCTION} - OK")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
          "Determining if the Fortran ${FUNCTION} exists passed with the following output:\n"
          "${OUTPUT}\n\n"
        )
      else ()
        message (STATUS "Testing Fortran ${FUNCTION} - Fail")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
          "Determining if the Fortran ${FUNCTION} exists failed with the following output:\n"
          "${OUTPUT}\n\n")
      endif ()
    endif ()
#  endif (NOT DEFINED ${RUN_RESULT_VAR})
ENDMACRO (FORTRAN_RUN)

#-----------------------------------------------------------------------------
#  Check to see C_LONG_DOUBLE is available
CHECK_FORTRAN_FEATURE(c_long_double
  "
       PROGRAM main
         USE ISO_C_BINDING
         REAL(KIND=C_LONG_DOUBLE) :: d
       END PROGRAM
  "
  FORTRAN_HAVE_C_LONG_DOUBLE
)
if (${FORTRAN_HAVE_C_LONG_DOUBLE})
  set(FORTRAN_HAVE_C_LONG_DOUBLE 1)
else ()
  set(FORTRAN_HAVE_C_LONG_DOUBLE 0)
endif()

# Check to see C_LONG_DOUBLE is different from C_DOUBLE

CHECK_FORTRAN_FEATURE(c_long_double
  "
     MODULE type_mod
       USE ISO_C_BINDING
       INTERFACE h5t	
         MODULE PROCEDURE h5t_c_double
         MODULE PROCEDURE h5t_c_long_double
       END INTERFACE
     CONTAINS
       SUBROUTINE h5t_c_double(r)
         REAL(KIND=C_DOUBLE) :: r
       END SUBROUTINE h5t_c_double
       SUBROUTINE h5t_c_long_double(d)
         REAL(KIND=C_LONG_DOUBLE) :: d
       END SUBROUTINE h5t_c_long_double
     END MODULE type_mod
     PROGRAM main
       USE ISO_C_BINDING
       USE type_mod
       REAL(KIND=C_DOUBLE)      :: r
       REAL(KIND=C_LONG_DOUBLE) :: d
       CALL h5t(r)
       CALL h5t(d)
     END PROGRAM main
  "
  FORTRAN_C_LONG_DOUBLE_IS_UNIQUE
)
if (${FORTRAN_C_LONG_DOUBLE_IS_UNIQUE})
  set(FORTRAN_C_LONG_DOUBLE_IS_UNIQUE 1)
else ()
  set(FORTRAN_C_LONG_DOUBLE_IS_UNIQUE 0)
endif()

## Set the sizeof function for use later in the fortran tests
if(FORTRAN_HAVE_STORAGE_SIZE)
  set(FC_SIZEOF_A "STORAGE_SIZE(a, c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)")
  set(FC_SIZEOF_B "STORAGE_SIZE(b, c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)")
  set(FC_SIZEOF_C "STORAGE_SIZE(c, c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)")
elseif(FORTRAN_HAVE_C_SIZEOF)
  set(FC_SIZEOF_A "SIZEOF(a)")
  set(FC_SIZEOF_B "SIZEOF(b)")
  set(FC_SIZEOF_C "SIZEOF(c)")
else(FORTRAN_HAVE_STORAGE_SIZE)
  message (FATAL_ERROR "Fortran compiler requires either intrinsic functions SIZEOF or STORAGE_SIZE")
endif(FORTRAN_HAVE_STORAGE_SIZE)

#-----------------------------------------------------------------------------
# Determine the available KINDs for REALs and INTEGERs
#-----------------------------------------------------------------------------

FORTRAN_RUN("REAL and INTEGER KINDs"
  "      
         PROGRAM main
           IMPLICIT NONE
           INTEGER :: ik, jk, k, max_decimal_prec
           INTEGER :: num_rkinds = 1, num_ikinds = 1
           INTEGER, DIMENSION(1:10) :: list_ikinds = -1
           INTEGER, DIMENSION(1:10) :: list_rkinds = -1

           ! Find integer KINDs
           list_ikinds(num_ikinds)=SELECTED_INT_KIND(1)
           DO ik = 2, 36
              k = SELECTED_INT_KIND(ik)
              IF(k.LT.0) EXIT
              IF(k.GT.list_ikinds(num_ikinds))THEN
                 num_ikinds = num_ikinds + 1
                 list_ikinds(num_ikinds) = k
              ENDIF
           ENDDO

           DO k = 1, num_ikinds
              WRITE(*,'(I0)', ADVANCE='NO') list_ikinds(k)
              IF(k.NE.num_ikinds)THEN
                 WRITE(*,'(A)',ADVANCE='NO') ','
              ELSE
                 WRITE(*,'()')
              ENDIF
           ENDDO

           ! Find real KINDs
           list_rkinds(num_rkinds)=SELECTED_REAL_KIND(1)
           max_decimal_prec = 1

           prec: DO ik = 2, 36
              exp: DO jk = 1, 17000
                 k = SELECTED_REAL_KIND(ik,jk)
                 IF(k.LT.0) EXIT exp
                 IF(k.GT.list_rkinds(num_rkinds))THEN
                    num_rkinds = num_rkinds + 1
                    list_rkinds(num_rkinds) = k
                 ENDIF
                 max_decimal_prec = ik
              ENDDO exp
           ENDDO prec

           DO k = 1, num_rkinds
              WRITE(*,'(I0)', ADVANCE='NO') list_rkinds(k)
              IF(k.NE.num_rkinds)THEN
                 WRITE(*,'(A)',ADVANCE='NO') ','
              ELSE
                 WRITE(*,'()')
              ENDIF
           ENDDO

           WRITE(*,'(I0)') max_decimal_prec
           WRITE(*,'(I0)') num_ikinds
           WRITE(*,'(I0)') num_rkinds
    
         END PROGRAM main
  "
  XX
  YY
  PROG_OUTPUT
)
# dnl The output from the above program will be:
# dnl    -- LINE 1 --  valid integer kinds (comma seperated list)
# dnl    -- LINE 2 --  valid real kinds (comma seperated list)
# dnl    -- LINE 3 --  max decimal precision for reals
# dnl    -- LINE 4 --  number of valid integer kinds
# dnl    -- LINE 5 --  number of valid real kinds

# Convert the string to a list of strings by replacing the carriage return with a semicolon
string(REGEX REPLACE "\n" ";" PROG_OUTPUT "${PROG_OUTPUT}")

list(GET PROG_OUTPUT 0 pac_validIntKinds)
list(GET PROG_OUTPUT 1 pac_validRealKinds)
list(GET PROG_OUTPUT 2 H5_PAC_FC_MAX_REAL_PRECISION)

set(PAC_FC_ALL_INTEGER_KINDS "\{${pac_validIntKinds}\}")
set(PAC_FC_ALL_REAL_KINDS "\{${pac_validRealKinds}\}")

list(GET PROG_OUTPUT 3 NUM_IKIND)
list(GET PROG_OUTPUT 4 NUM_RKIND)

set(H5CONFIG_F_NUM_IKIND "INTEGER, PARAMETER :: num_ikinds = ${NUM_IKIND}")
set(H5CONFIG_F_IKIND "INTEGER, DIMENSION(1:num_ikinds) :: ikind = (/${pac_validIntKinds}/)")

message (STATUS "....REAL KINDS FOUND ${PAC_FC_ALL_REAL_KINDS}")
message (STATUS "....INTEGER KINDS FOUND ${PAC_FC_ALL_REAL_KINDS}")
message (STATUS "....MAX DECIMAL PRECISION ${H5_PAC_FC_MAX_REAL_PRECISION}")

#-----------------------------------------------------------------------------
# Determine the available KINDs for REALs and INTEGERs
#-----------------------------------------------------------------------------
# **********
# INTEGERS
# **********
string(REGEX REPLACE "," ";" VAR "${pac_validIntKinds}")

foreach( KIND ${VAR} )
  set(PROG_SRC 
    "
        PROGRAM main
        USE ISO_C_BINDING
        IMPLICIT NONE
        INTEGER (KIND=${KIND}) a
        WRITE(*,'(I0)') ${FC_SIZEOF_A}
        END
     "
  )
  FORTRAN_RUN("INTEGER KIND SIZEOF" ${PROG_SRC}
  XX
  YY
  PROG_OUTPUT1
  )
  string(REGEX REPLACE "\n" "" PROG_OUTPUT1 "${PROG_OUTPUT1}")
  set(pack_int_sizeof "${pack_int_sizeof} ${PROG_OUTPUT1},")
endforeach(KIND)
string(STRIP ${pack_int_sizeof} pack_int_sizeof)


#Remove trailing comma
string(REGEX REPLACE ",$" "" pack_int_sizeof "${pack_int_sizeof}")
#Remove spaces
string(REGEX REPLACE " " "" pack_int_sizeof "${pack_int_sizeof}")

set(PAC_FC_ALL_INTEGER_KINDS_SIZEOF "\{${pack_int_sizeof}\}")

message(STATUS "....FOUND SIZEOF for INTEGER KINDs ${PAC_FC_ALL_INTEGER_KINDS_SIZEOF}")
# **********
# REALS
# **********
string(REGEX REPLACE "," ";" VAR "${pac_validRealKinds}")

#find the maximum kind of the real
list(LENGTH VAR LEN_VAR)
MATH (EXPR _LEN "${LEN_VAR}-1")
list(GET VAR ${_LEN} max_real_fortran_kind)

foreach( KIND ${VAR} )
  set(PROG_SRC 
    "
        PROGRAM main
        USE ISO_C_BINDING
        IMPLICIT NONE
        REAL (KIND=${KIND}) a
        WRITE(*,'(I0)') ${FC_SIZEOF_A}
        END
     "
  )
  FORTRAN_RUN("REAL KIND SIZEOF" ${PROG_SRC}
  XX
  YY
  PROG_OUTPUT1
  )
  string(REGEX REPLACE "\n" "" PROG_OUTPUT1 "${PROG_OUTPUT1}")
  set(pack_real_sizeof "${pack_real_sizeof} ${PROG_OUTPUT1},")
endforeach(KIND)
string(STRIP ${pack_real_sizeof} pack_real_sizeof)

#Remove trailing comma
string(REGEX REPLACE ",$" "" pack_real_sizeof "${pack_real_sizeof}")
#Remove spaces
string(REGEX REPLACE " " "" pack_real_sizeof "${pack_real_sizeof}")

set(H5CONFIG_F_RKIND_SIZEOF "INTEGER, DIMENSION(1:num_rkinds) :: rkind_sizeof = (/${pack_real_sizeof}/)")

message(STATUS "....FOUND SIZEOF for REAL KINDs \{${pack_real_sizeof}\}")

set(PAC_FC_ALL_REAL_KINDS_SIZEOF "\{${pack_real_sizeof}\}")

#find the maximum kind of the real
string(REGEX REPLACE "," ";" VAR "${pack_real_sizeof}")
list(LENGTH VAR LEN_VAR)
MATH (EXPR _LEN "${LEN_VAR}-1")
list(GET VAR ${_LEN} max_real_fortran_sizeof)

#-----------------------------------------------------------------------------
# Find sizeof of native kinds
#-----------------------------------------------------------------------------
FORTRAN_RUN("SIZEOF NATIVE KINDs"
  "
       PROGRAM main
          USE ISO_C_BINDING
          IMPLICIT NONE
          INTEGER a
          REAL b
          DOUBLE PRECISION c
          WRITE(*,*) ${FC_SIZEOF_A}
	  WRITE(*,*) kind(a)
	  WRITE(*,*) ${FC_SIZEOF_B}
	  WRITE(*,*) kind(b)
          WRITE(*,*) ${FC_SIZEOF_C}
          WRITE(*,*) kind(c)
       END
  "
  XX
  YY
  PROG_OUTPUT
)
# dnl The output from the above program will be:
# dnl    -- LINE 1 --  sizeof INTEGER
# dnl    -- LINE 2 --  kind of INTEGER
# dnl    -- LINE 3 --  sizeof REAL
# dnl    -- LINE 4 --  kind of REAL
# dnl    -- LINE 5 --  sizeof DOUBLE PRECISION
# dnl    -- LINE 6 --  kind of DOUBLE PRECISION

# Convert the string to a list of strings by replacing the carriage return with a semicolon
string(REGEX REPLACE "\n" ";" PROG_OUTPUT "${PROG_OUTPUT}")

list(GET PROG_OUTPUT 0 PAC_FORTRAN_NATIVE_INTEGER_SIZEOF)
list(GET PROG_OUTPUT 1 PAC_FORTRAN_NATIVE_INTEGER_KIND)
list(GET PROG_OUTPUT 2 PAC_FORTRAN_NATIVE_REAL_SIZEOF)
list(GET PROG_OUTPUT 3 PAC_FORTRAN_NATIVE_REAL_KIND)
list(GET PROG_OUTPUT 4 PAC_FORTRAN_NATIVE_DOUBLE_SIZEOF)
list(GET PROG_OUTPUT 5 PAC_FORTRAN_NATIVE_DOUBLE_KIND)

set(FORTRAN_SIZEOF_LONG_DOUBLE ${${HDF_PREFIX}_SIZEOF_LONG_DOUBLE})
#set(H5_SIZEOF_LONG_DOUBLE ${${HDF_PREFIX}_SIZEOF_LONG_DOUBLE})

# remove the invalid kind from the list
if(NOT(${SIZEOF___FLOAT128} EQUAL 0))
   if(NOT(${SIZEOF___FLOAT128} EQUAL ${max_real_fortran_sizeof}) 
     AND NOT(${FORTRAN_SIZEOF_LONG_DOUBLE} EQUAL ${max_real_fortran_sizeof})
     # account for the fact that the C compiler can have 16-byte __float128 and the fortran compiler only has 8-byte doubles,
     # so we don't want to remove the 8-byte fortran doubles.
     AND NOT(${PAC_FORTRAN_NATIVE_DOUBLE_SIZEOF} EQUAL ${max_real_fortran_sizeof}))
     message(WARNING "
          Fortran REAL(KIND=${max_real_fortran_kind}) is $max_real_fortran_sizeof Bytes, but no corresponding C float type exists of that size
                                           !!! Fortran interfaces will not be generated for REAL(KIND=${max_real_fortran_kind}) !!!")
     string(REGEX REPLACE ",[0-9]+}" "}" PAC_FC_ALL_REAL_KINDS ${PAC_FC_ALL_REAL_KINDS})
     string(REGEX REPLACE ",[0-9]+}" "}" PAC_FC_ALL_REAL_KINDS_SIZEOF ${PAC_FC_ALL_REAL_KINDS_SIZEOF})
     MATH (EXPR NUM_RKIND "${NUM_RKIND} - 1")
   endif()
endif(NOT(${SIZEOF___FLOAT128} EQUAL 0))

set(H5CONFIG_F_NUM_RKIND "INTEGER, PARAMETER :: num_rkinds = ${NUM_RKIND}")

string(REGEX REPLACE "{" "" OUT_VAR ${PAC_FC_ALL_REAL_KINDS})
string(REGEX REPLACE "}" "" OUT_VAR ${OUT_VAR})
set(H5CONFIG_F_RKIND "INTEGER, DIMENSION(1:num_rkinds) :: rkind = (/${OUT_VAR}/)")

string(REGEX REPLACE "{" "" OUT_VAR ${PAC_FC_ALL_REAL_KINDS_SIZEOF})
string(REGEX REPLACE "}" "" OUT_VAR ${OUT_VAR})
set(H5CONFIG_F_RKIND_SIZEOF "INTEGER, DIMENSION(1:num_rkinds) :: rkind_sizeof = (/${OUT_VAR}/)")

ENABLE_LANGUAGE (C)

#-----------------------------------------------------------------------------
# The provided CMake C macros don't provide a general compile/run function
# so this one is used.
#-----------------------------------------------------------------------------
MACRO (C_RUN FUNCTION CODE RETURN)
    message (STATUS "Detecting C ${FUNCTION}")
    if (CMAKE_REQUIRED_LIBRARIES)
      set (CHECK_FUNCTION_EXISTS_ADD_LIBRARIES
          "-DLINK_LIBRARIES:STRING=${CMAKE_REQUIRED_LIBRARIES}")
    else (CMAKE_REQUIRED_LIBRARIES)
      set (CHECK_FUNCTION_EXISTS_ADD_LIBRARIES)
    endif (CMAKE_REQUIRED_LIBRARIES)
    file (WRITE
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testCCompiler1.c
        ${CODE}
    )
    TRY_RUN (RUN_RESULT_VAR COMPILE_RESULT_VAR
        ${CMAKE_BINARY_DIR}
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testCCompiler1.c
        CMAKE_FLAGS "${CHECK_FUNCTION_EXISTS_ADD_LIBRARIES}"
        RUN_OUTPUT_VARIABLE OUTPUT
    )

    set(${RETURN} ${OUTPUT})
	
    #message ( "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
    #message ( "Test COMPILE_RESULT_VAR ${COMPILE_RESULT_VAR} ")
    #message ( "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
    #message ( "Test RUN_RESULT_VAR ${RUN_RESULT_VAR} ")
    #message ( "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")

    if (${COMPILE_RESULT_VAR})
      if (${RUN_RESULT_VAR} MATCHES 1)
        set (${RUN_RESULT_VAR} 1 CACHE INTERNAL "Have C function ${FUNCTION}")
        message (STATUS "Testing C ${FUNCTION} - OK")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
            "Determining if the C ${FUNCTION} exists passed with the following output:\n"
            "${OUTPUT}\n\n"
        )
      else ()
        message (STATUS "Testing C ${FUNCTION} - Fail")
        set (${RUN_RESULT_VAR} 0 CACHE INTERNAL "Have C function ${FUNCTION}")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "Determining if the C ${FUNCTION} exists failed with the following output:\n"
            "${OUTPUT}\n\n")
      endif ()
    else ()
        message (FATAL_ERROR "Compilation of C ${FUNCTION} - Failed")
    endif()
ENDMACRO (C_RUN)

set(PROG_SRC 
    "
#include <float.h>
#include <stdio.h>
#define CHECK_FLOAT128 ${SIZEOF___FLOAT128}
#if CHECK_FLOAT128!=0
# if ${HAVE_QUADMATH}!=0
#include <quadmath.h>
# endif
# ifdef FLT128_DIG
#define C_FLT128_DIG FLT128_DIG
# else
#define C_FLT128_DIG 0
# endif
#else
#define C_FLT128_DIG 0
#endif
#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#define C_LDBL_DIG DECIMAL_DIG 
#else
#define C_LDBL_DIG LDBL_DIG
#endif
   int main() {
       printf(\"%d\\\\n%d\\\\n\", C_LDBL_DIG, C_FLT128_DIG)\\\;
       return 1\\\;
   }
     "
  )

C_RUN("maximum decimal precision for C" ${PROG_SRC} PROG_OUTPUT)

# dnl The output from the above program will be:
# dnl    -- LINE 1 --  long double decimal precision
# dnl    -- LINE 2 --  __float128 decimal precision

# Convert the string to a list of strings by replacing the carriage return with a semicolon
string(REGEX REPLACE "\n" ";" PROG_OUTPUT "${PROG_OUTPUT}")

list(GET PROG_OUTPUT 0 LDBL_DIG)
list(GET PROG_OUTPUT 1 FLT128_DIG)

if(SIZEOF___FLOAT128 EQUAL 0 OR FLT128_DIG EQUAL 0)
  SET(H5_HAVE_FLOAT128 0)
  SET(SIZEOF___FLOAT128 0)
  set(H5_PAC_C_MAX_REAL_PRECISION ${LDBL_DIG})
else ()
  set(H5_PAC_C_MAX_REAL_PRECISION ${FLT128_DIG})
endif()


# Setting definition if there is a 16 byte fortran integer
string(FIND ${PAC_FC_ALL_INTEGER_KINDS_SIZEOF} "16" pos)
if(${pos} EQUAL -1)
  set(HAVE_Fortran_INTEGER_SIZEOF_16 0)
else ()
  set(HAVE_Fortran_INTEGER_SIZEOF_16 1)
endif ()
