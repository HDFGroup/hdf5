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
dnl Macros for HDF5 Fortran
dnl
dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------

dnl -------------------------------------------------------------------------
dnl AC_F9X_MODS()
dnl
dnl	Check how F9X handles modules. This macro also checks which
dnl	command-line option to use to include the module once it's built.
dnl
AC_DEFUN(AC_F9X_MODS,
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

dnl -------------------------------------------------------------------------
dnl AC_TRY_FCOMPILE()
dnl
dnl	Check if we can compile a simple Fortran 90 program.
dnl
dnl AC_TRY_FCOMPILE(FUNCTION-BODY,
dnl                 [ACTION-IF-SUCCESS], [ACTION-IF-NOT-SUCCESS])
dnl
AC_DEFUN([AC_TRY_FCOMPILE],
[AC_LANG_PUSH(Fortran)

test -d conftestdir || mkdir conftestdir
cd conftestdir
rm -rf *

cat >conftest.$ac_ext <<EOF
          $1
EOF

if AC_TRY_EVAL(ac_compile); then
  :
  [$2]
else
  :
  [$3]
fi
cd ..
rm -rf conftest*
AC_LANG_POP(Fortran)
])

dnl -------------------------------------------------------------------------
dnl AC_TRY_FLINK()
dnl
dnl	Check if we can link a simple Fortran 90 program.
dnl
dnl AC_TRY_FLINK(INCLUDES, FUNCTION-BODY,
dnl              [ACTION-IF-SUCCESS], [ACTION-IF-NOT-SUCCESS])
dnl
AC_DEFUN([AC_TRY_FLINK],
[AC_LANG_PUSH(Fortran)

test -d conftestdir || mkdir conftestdir
cd conftestdir
rm -rf *

cat >conftest.$ac_ext <<EOF
        program conftest
          include '$1'
          $2
        end
EOF

if AC_TRY_EVAL(ac_compile) && AC_TRY_EVAL(ac_link); then
  :
  [$3]
else
  :
  [$4]
fi
cd ..
rm -rf conftest*
AC_LANG_POP(Fortran)
])

dnl -------------------------------------------------------------------------
dnl AC_CHECK_FLIB()
dnl
dnl	Check if we can link a simple Fortran 90 program with the specified library.
dnl
dnl AC_CHECK_FLIB(LIBRARY, FUNCTION-BODY,
dnl               [ACTION-IF-SUCCESS], [ACTION-IF-NOT-SUCCESS])
dnl
AC_DEFUN([AC_CHECK_FLIB],
[AC_LANG_PUSH(Fortran)

test -d conftestdir || mkdir conftestdir
cd conftestdir
rm -rf *

cat >conftest.$ac_ext <<EOF
        program conftest
          $2
        end
EOF

if test -n "$1"; then
  saved_LIBS="$LIBS"
  LIBS="$LIBS -l$1"
fi

if AC_TRY_EVAL(ac_compile) && AC_TRY_EVAL(ac_link); then
  :
  [$3]
else
  LIBS="$saved_LIBS"
  [$4]
fi
cd ..
rm -rf conftest*
AC_LANG_POP(Fortran)
])

dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------
dnl
dnl Possible future tests for the Fortran stuff...
dnl
dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------
dnl
dnl echo $ac_n "checking whether f90 real type has 12 digits of precision""... $ac_c" 1>&6
dnl cat >conftest.f90 <<'EOF'
dnl program conftest
dnl 	if (selected_real_kind(12) .eq. kind(0.0)) then
dnl 		print '(a)','YES'
dnl 	else
dnl 		print '(a)','NO'
dnl 	end if
dnl end program conftest
dnl EOF
dnl $ac_cv_prog_F90 $FCFLAGS -o conftest conftest.f90 > /dev/null 2>&1
dnl if test "`./conftest | head -1`" = YES; then
dnl 	echo "yes" 1>&6
dnl 	AC_DEFINE(HIPREC)
dnl else
dnl 	echo "no" 1>&6
dnl fi

dnl echo $ac_n "checking whether f90 precision of default real type""... $ac_c" 1>&6
dnl cat >conftest.f90 <<'EOF'
dnl program conftest
dnl 	if (kind(0.0) .eq. selected_real_kind(12) .or.&
dnl 		&kind(0.0) .eq. selected_real_kind(6) ) then
dnl 		print '(a)','YES'
dnl 	else
dnl 		print '(a)','NO'
dnl 	end if
dnl end program conftest
dnl EOF
dnl $ac_cv_prog_F90 $FCFLAGS -o conftest conftest.f90 > /dev/null 2>&1
dnl if test "`./conftest | head -1`" = YES; then
dnl 	echo "OK" 1>&6
dnl 	AC_DEFINE(REALOK)
dnl else
dnl 	echo "no" 1>&6
dnl fi

dnl echo $ac_n "checking accuracy of arithmetic""... $ac_c" 1>&6
dnl cat >conftest.f90 <<'EOF'
dnl program conftest
dnl 	integer, parameter :: double=selected_real_kind(12)
dnl 	real(double), parameter :: ulp=2.0_double**(-47)
dnl 	real(double), parameter :: ans=0.92699498904359955986_double
dnl 	real(double) :: x,y,z
dnl 	integer i
dnl 	x=0.0_double
dnl 	y=ulp
dnl 	do i=2,100
dnl 		z=x+y
dnl 		x=y
dnl 		y=z-int(z)
dnl 	end do
dnl 	if (y-ans .lt. ulp/2.0_double) then
dnl 		print '(a)','YES'
dnl 	else
dnl 		print '(a)','NO'
dnl 	end if
dnl end program conftest
dnl EOF
dnl $ac_cv_prog_F90 $FCFLAGS -o conftest conftest.f90 > /dev/null 2>&1
dnl if test "`./conftest | head -1`" = YES; then
dnl 	echo "OK" 1>&6
dnl 	AC_DEFINE(ARITHOK)
dnl else
dnl 	echo "no" 1>&6
dnl fi

dnl echo $ac_n "checking whether int is the fastest way to truncate""... $ac_c" 1>&6
dnl cat >conftest.f90 <<'EOF'
dnl program conftest
dnl 	integer, parameter :: double=selected_real_kind(12)
dnl 	real(double), parameter :: ulp=2.0_double**(-47)
dnl 	real(double) :: x,y,z,ya,yb
dnl 	integer :: t0,t1,i,timea,timeb
dnl 
dnl 	x=0.0_double
dnl 	y=ulp
dnl 	call system_clock(t0)
dnl 	do i=2,1000000
dnl 		z=x+y
dnl 		x=y
dnl 		y=z-int(z)
dnl 	end do
dnl 	ya=y
dnl 	call system_clock(t1)
dnl 	timea=t1-t0
dnl 
dnl 	x=0.0_double
dnl 	y=ulp
dnl 	call system_clock(t0)
dnl 	do i=2,1000000
dnl 		z=x+y
dnl 		x=y
dnl 		if (z.ge.1.0_double) then
dnl 			y=z-1.0_double
dnl 		else
dnl 			y=z
dnl 		end if
dnl 	end do
dnl 	yb=y
dnl 	call system_clock(t1)
dnl 	timeb=t1-t0
dnl 
dnl 	if (timea.lt.timeb) then
dnl 		print '(a)','YES'
dnl 	else
dnl 		print '(a)','NO'
dnl 	end if
dnl 	print *,ya,yb
dnl end program conftest
dnl EOF
dnl $ac_cv_prog_F90 $FCFLAGS -o conftest conftest.f90 > /dev/null 2>&1
dnl if test "`./conftest | head -1`" = YES; then
dnl 	echo "yes" 1>&6
dnl 	AC_DEFINE(USEINT)
dnl else
dnl 	echo "no" 1>&6
dnl fi
dnl
dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------
