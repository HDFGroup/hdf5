dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------
dnl
dnl Copyright by the Board of Trustees of the University of Illinois.
dnl All rights reserved.
dnl
dnl This file is part of HDF5.  The full HDF5 copyright notice, including
dnl terms governing use, modification, and redistribution, is contained in
dnl the files COPYING and Copyright.html.  COPYING can be found at the root
dnl of the source code distribution tree; Copyright.html can be found at the
dnl root level of an installed copy of the electronic HDF5 document set and
dnl is linked from the top-level documents page.  It can also be found at
dnl http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
dnl access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
dnl
dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------

dnl *********************************
dnl PURPOSE
dnl  Contains Macros for HDF5 Fortran
dnl *********************************
dnl
dnl Special characteristics that have no autoconf counterpart but that
dnl we need as part of the C++ support.  To distinquish these, they
dnl have a [PAC] prefix.
dnl
dnl -------------------------------------------------------------------------
dnl
dnl PAC_FC_SEARCH_LIST - expands to a whitespace separated list of modern
dnl fortran compilers for use with AC_PROG_FC that is more suitable for HPC
dnl software packages
AC_DEFUN([PAC_FC_SEARCH_LIST],
         [gfortran ifort pgf90 pathf90 pathf95 xlf90 xlf95 xlf2003 f90 epcf90 f95 fort lf95 g95 ifc efc gfc])
dnl 
dnl PAC_PROG_FC([COMPILERS])
dnl
dnl COMPILERS is a space separated list of Fortran compilers to search for.
dnl
dnl Compilers are ordered by
dnl  1. F90, F95, F2003
dnl  2. Good/tested native compilers, bad/untested native compilers
dnl  3. Wrappers around f2c go last.
dnl
dnl frt is the Fujitsu Fortran compiler.
dnl pgf90 are the Portland Group F90 compilers.
dnl xlf/xlf90/xlf95/xlf2003 are IBM (AIX) F90/F95/F2003 compilers.
dnl lf95 is the Lahey-Fujitsu compiler.
dnl fl32 is the Microsoft Fortran "PowerStation" compiler.
dnl epcf90 is the "Edinburgh Portable Compiler" F90.
dnl fort is the Compaq Fortran 90 (now 95) compiler for Tru64 and Linux/Alpha.
dnl pathf90 is the Pathscale Fortran 90 compiler
dnl ifort is another name for the Intel f90 compiler
dnl efc - An older Intel compiler (?)
dnl ifc - An older Intel compiler
dnl fc  - A compiler on some unknown system.  This has been removed because
dnl       it may also be the name of a command for something other than
dnl       the Fortran compiler (e.g., fc=file system check!)
dnl gfortran - The GNU Fortran compiler (not the same as g95) 
dnl gfc - An alias for gfortran recommended in cygwin installations
dnl NOTE: this macro suffers from a basically intractable "expanded before it
dnl was required" problem when libtool is also used 
dnl [1] MPICH.org
dnl

dnl See if the fortran compiler supports the intrinsic function "SIZEOF"

AC_DEFUN([PAC_PROG_FC_SIZEOF],[
  HAVE_SIZEOF_FORTRAN="no"
  AC_MSG_CHECKING([if Fortran compiler supports intrinsic SIZEOF])
  AC_LINK_IFELSE([AC_LANG_SOURCE([ 
   PROGRAM main
     i = sizeof(x)
   END PROGRAM
  ])],[AC_MSG_RESULT([yes])
     	HAVE_SIZEOF_FORTRAN="yes"],
      [AC_MSG_RESULT([no])])
])

dnl See if the fortran compiler supports the intrinsic function "C_SIZEOF"

AC_DEFUN([PAC_PROG_FC_C_SIZEOF],[
  HAVE_C_SIZEOF_FORTRAN="no"
  AC_MSG_CHECKING([if Fortran compiler supports intrinsic C_SIZEOF])
  AC_LINK_IFELSE([AC_LANG_SOURCE([ 
   PROGRAM main
     USE ISO_C_BINDING
     INTEGER(C_INT) :: a
     INTEGER(C_SIZE_T) :: result
     result = C_SIZEOF(a)
   END PROGRAM
  ])], [AC_MSG_RESULT([yes])
     	HAVE_C_SIZEOF_FORTRAN="yes"],
     [AC_MSG_RESULT([no])])
])

dnl See if the fortran compiler supports the intrinsic function "STORAGE_SIZE"

AC_DEFUN([PAC_PROG_FC_STORAGE_SIZE],[
  HAVE_STORAGE_SIZE_FORTRAN="no"
  AC_MSG_CHECKING([if Fortran compiler supports intrinsic STORAGE_SIZE])
  AC_LINK_IFELSE([AC_LANG_SOURCE([
   PROGRAM main
     INTEGER :: a
     INTEGER :: result
     result = STORAGE_SIZE(a)
   END PROGRAM
  ])], [AC_MSG_RESULT([yes])
     	HAVE_STORAGE_SIZE_FORTRAN="yes"],
     [AC_MSG_RESULT([no])])

])

dnl Check to see if -r8 was specified to determine if we need to
dnl compile the DOUBLE PRECISION interfaces.

AC_DEFUN([PAC_PROG_FC_DEFAULT_REALisDBLE],[
  FORTRAN_DEFAULT_REALisDBLE="no"	
  AC_MSG_CHECKING([if Fortran default REAL is DOUBLE PRECISION])
  
  AC_COMPILE_IFELSE([AC_LANG_SOURCE([
     MODULE type_mod
       INTERFACE h5t	
         MODULE PROCEDURE h5t_real
         MODULE PROCEDURE h5t_dble
       END INTERFACE
     CONTAINS
       SUBROUTINE h5t_real(r)
         REAL :: r
       END SUBROUTINE h5t_real
       SUBROUTINE h5t_dble(d)
         DOUBLE PRECISION :: d
       END SUBROUTINE h5t_dble
     END MODULE type_mod
     PROGRAM main
       USE type_mod
       REAL :: r
       DOUBLE PRECISION :: d
       CALL h5t(r)
       CALL h5t(d)
     END PROGRAM main
    ])], [AC_MSG_RESULT([no])], 
         [AC_MSG_RESULT([yes])
            FORTRAN_DEFAULT_REALisDBLE="yes"])
])

dnl Checking if the compiler supports the required Fortran 2003 features and
dnl disable Fortran 2003 if it does not.

AC_DEFUN([PAC_PROG_FC_HAVE_F2003_REQUIREMENTS],[
   AC_MSG_CHECKING([if Fortran compiler version compatible with Fortran 2003 HDF])
dnl --------------------------------------------------------------------
dnl Default for FORTRAN 2003 compliant compilers
dnl
    HAVE_FORTRAN_2003="no"
    HAVE_F2003_REQUIREMENTS="no"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([],[

        USE iso_c_binding
        IMPLICIT NONE
        TYPE(C_PTR) :: ptr
        TYPE(C_FUNPTR) :: funptr
        CHARACTER(LEN=80, KIND=c_char), TARGET :: ichr

        ptr = C_LOC(ichr(1:1))

        ])],[AC_MSG_RESULT([yes])
        HAVE_F2003_REQUIREMENTS=[yes]], 
      [AC_MSG_RESULT([no])])
])

dnl -------------------------------------------------------------------------
dnl AC_F9X_MODS()
dnl
dnl	Check how F9X handles modules. This macro also checks which
dnl	command-line option to use to include the module once it's built.
dnl
AC_DEFUN([AC_F9X_MODS],
[AC_MSG_CHECKING(what $FC does with modules)
AC_LANG_PUSH(Fortran)

test -d conftestdir || mkdir conftestdir
cd conftestdir
rm -rf *

cat >conftest.$ac_ext <<EOF
      module module
         integer foo
      end module module
EOF

eval $ac_compile
modfiles=""
F9XMODEXT=""

for f in conftest.o module.mod MODULE.mod module.M MODULE.M; do
  if test -f "$f" ; then
    modfiles="$f"

    case "$f" in
      *.o)   F9XMODEXT="o" ;;
      *.mod) F9XMODEXT="mod" ;;
      *.M)   F9XMODEXT="M" ;;
    esac
  fi
done

echo $modfiles 6>&1
if test "$modfiles" = file.o; then
  echo $ac_n "checking whether $FC -em is saner""... $ac_c" 1>&6
  OLD_FCFLAGS=$FCFLAGS
  FCFLAGS="$FCFLAGS -em"
  eval $ac_compile
  modfiles=""
  for f in file.o module.mod MODULE.mod module.M MODULE.M; do
    test -f $f && modfiles="$f"
  done
  if test "$modfiles" = "file.o"; then
    FCFLAGS=$OLD_FCFLAGS
    echo no 6>&1
  else
    echo yes 6>&1
  fi
fi
cd ..

AC_MSG_CHECKING(how $FC finds modules)

for flag in "-I" "-M" "-p"; do
  cat >conftest.$ac_ext <<EOF
      program conftest
          use module
      end program conftest
EOF

  ac_compile='${FC-f90} $FCFLAGS ${flag}conftestdir -c conftest.$ac_ext 1>&AS_MESSAGE_LOG_FD'

  if AC_TRY_EVAL(ac_compile); then
    F9XMODFLAG=$flag
    break
  fi
done

if test -n "$F9XMODFLAG"; then
  echo $F9XMODFLAG 1>&6
  FCFLAGS="$F9XMODFLAG. $FCFLAGS"
else
  echo unknown 1>&6
fi
AC_SUBST(F9XMODFLAG)
AC_SUBST(F9XMODEXT)
rm -rf conftest*
AC_LANG_POP(Fortran)
])

dnl ----------------------
dnl Parallel Test Programs
dnl ----------------------

dnl Try link a simple MPI program.

AC_DEFUN([PAC_PROG_FC_MPI_CHECK],[

dnl   Change to the Fortran 90 language
      AC_LANG_PUSH(Fortran)

dnl   Try link a simple MPI program.
      AC_MSG_CHECKING([whether a simple MPI-IO Fortran program can be linked])
      AC_LINK_IFELSE([ 
          PROGRAM main
          USE mpi
          INTEGER :: comm, amode, info, fh, ierror
          CHARACTER(LEN=1) :: filename 
          CALL MPI_File_open( comm, filename, amode, info, fh, ierror)
          END],
	  [AC_MSG_RESULT([yes])],
	  [AC_MSG_RESULT([no])
	   AC_MSG_ERROR([unable to link a simple MPI-IO Fortran program])])

dnl   Change to the C language
      AC_LANG_POP(Fortran)
])
	
