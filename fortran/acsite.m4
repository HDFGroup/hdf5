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
dnl AC_PROG_F9X
dnl
dnl	Check for a Fortran 9X compiler.
dnl
AC_DEFUN(AC_PROG_F9X,
[AC_CHECK_PROGS(F9X, f90 pgf90 xlf90 f95 g95 xlf95 ftn)
test -z "$F9X" && AC_MSG_ERROR([no acceptable f9X compiler found in \$PATH])

AC_PROG_F9X_WORKS
AC_PROG_F9X_GNU

if test $ac_cv_prog_g9x = yes; then
  G9X=yes
  dnl Check whether -g works, even if FFLAGS is set, in case the package
  dnl plays around with FFLAGS (such as to build both debugging and
  dnl normal versions of a library), tasteless as that idea is.
  ac_test_FFLAGS="${FFLAGS+set}"
  ac_save_FFLAGS="$FFLAGS"
  FFLAGS=
  AC_PROG_F9X_G
  if test "$ac_test_FFLAGS" = set; then
    FFLAGS="$ac_save_FFLAGS"
  elif test $ac_cv_prog_f9x_g = yes; then
    FFLAGS="-g -O2"
  else
    FFLAGS="-O2"
  fi
else
  G9X=
  test "${FFLAGS+set}" = set || FFLAGS="-g"
fi
])

dnl -------------------------------------------------------------------------
dnl AC_TRY_F9X_COMPILER()
dnl
dnl	It would be nice if the compiler actually works.
dnl
AC_DEFUN(AC_TRY_F9X_COMPILER, [
cat > conftest.$ac_ext << EOF
[$1]
EOF
if AC_TRY_EVAL(ac_link) && test -s conftest${ac_exeext}; then
  [$2]=yes
  # If we can't run a trivial program, we are probably using a cross compiler.
  if (./conftest; exit) 2>/dev/null; then
    [$3]=no
  else
    [$3]=yes
  fi
else
  echo "configure: failed program was:" >&AS_MESSAGE_LOG_FD
  cat conftest.$ac_ext >&AS_MESSAGE_LOG_FD
  [$2]=no
fi
rm -fr conftest*
])

dnl -------------------------------------------------------------------------
dnl AC_LANG_FORTRAN9X()
dnl
dnl	Generic macro to setup the Fortran 9X specific env variables.
dnl
m4_define([AC_LANG(FORTRAN9X)],
[ac_ext=f90
ac_compile='${F9X-f90} -c $FFLAGS conftest.$ac_ext 1>&AS_MESSAGE_LOG_FD'
ac_link='${F9X-f90} -o conftest${ac_exeext} $FFLAGS conftest.$ac_ext $LDFLAGS $LIBS 1>&AS_MESSAGE_LOG_FD'
cross_compiling=$ac_cv_prog_f9x_cross
])

AU_DEFUN([AC_LANG_FORTRAN9X], [AC_LANG(FORTRAN9X)])

dnl -------------------------------------------------------------------------
dnl AC_LANG_F9X_WORKS()
dnl
dnl	It would be nice if the compiler actually works.
dnl
AC_DEFUN(AC_PROG_F9X_WORKS, [
AC_MSG_CHECKING([whether the Fortran 9X compiler ($F9X $FFLAGS $LDFLAGS) works])
AC_LANG_SAVE
AC_LANG_FORTRAN9X
AC_TRY_F9X_COMPILER([
      program conftest
      end
], ac_cv_prog_f9x_works, ac_cv_prog_f9x_cross)
AC_LANG_RESTORE
AC_MSG_RESULT($ac_cv_prog_f9x_works)
if test $ac_cv_prog_f9x_works = no; then
  AC_MSG_ERROR([installation or configuration problem: Fortran 9X compiler cannot create executables.])
fi
AC_MSG_CHECKING([whether the Fortran 9X compiler ($F9X $FFLAGS $LDFLAGS) is a cross-compiler])
AC_MSG_RESULT($ac_cv_prog_f9x_cross)
cross_compiling=$ac_cv_prog_f9x_cross
])

dnl -------------------------------------------------------------------------
dnl AC_PROG_F9X_GNU
dnl
dnl	Test whether for Fortran 9X compiler is `g95' (the GNU Fortran 95
dnl	Compiler). This test depends on whether the Fortran 9X compiler
dnl	can do CPP pre-processing.
dnl
AC_DEFUN(AC_PROG_F9X_GNU,
[AC_CACHE_CHECK(whether we are using GNU Fortran 95, ac_cv_prog_g9x,
[cat > conftest.fpp <<EOF
#ifdef __GNUC__
  yes
#endif
EOF
if AC_TRY_COMMAND($F9X -E conftest.fpp) | egrep yes >/dev/null 2>&1; then
  ac_cv_prog_g9x=yes
else
  ac_cv_prog_g9x=no
fi])])

dnl -------------------------------------------------------------------------
dnl AC_PROG_F9X_G
dnl
dnl	Test whether the Fortran 9X compiler can accept the `-g' option
dnl	to enable debugging.
dnl
AC_DEFUN(AC_PROG_F9X_G,
[AC_CACHE_CHECK(whether $F9X accepts -g, ac_cv_prog_f9x_g,
[cat > conftest.f << EOF
       program conftest
       end
EOF
if test -z "`$F9X -g -c conftest.f 2>&1`"; then
  ac_cv_prog_f9x_g=yes
else
  ac_cv_prog_f9x_g=no
fi
rm -f conftest*
])])

dnl -------------------------------------------------------------------------
dnl AC_F9X_OPT_FLAGS()
dnl
dnl	Check for optimizer flags the Fortran compiler can use.
dnl
AC_DEFUN(AC_F9X_OPT_FLAGS,
[AC_MSG_CHECKING([for $F9X optimizer flags])
AC_LANG_SAVE
AC_LANG_FORTRAN9X

for flags in "-fast" "-O3" "-O" "";do
  cat > conftest.$ac_ext <<EOF
      program main
      end
EOF
  ac_compile='${F9X-f90} -c $flag $FFLAGS conftest.$ac_ext 1>&AS_MESSAGE_LOG_FD'
  if AC_TRY_EVAL(ac_compile); then
    if grep 'passed to ld' conftest.out > /dev/null 2>&1; then :; else
      FFLAGS="$FFLAGS $flags"
      break
    fi
  fi
done

if test -n "$flags"; then
  echo "$flags" 1>&6
else
  echo "none" 1>&6
fi
rm -f conftest*
])

dnl -------------------------------------------------------------------------
dnl AC_F9X_MODS()
dnl
dnl	Check how F9X handles modules. This macro also checks which
dnl	command-line option to use to include the module once it's built.
dnl
AC_DEFUN(AC_F9X_MODS,
[AC_MSG_CHECKING(what $F9X does with modules)
AC_LANG_SAVE
AC_LANG_FORTRAN9X

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
  echo $ac_n "checking whether $F9X -em is saner""... $ac_c" 1>&6
  OLD_FFLAGS=$FFLAGS
  FFLAGS="$FFLAGS -em"
  eval $ac_compile
  modfiles=""
  for f in file.o module.mod MODULE.mod module.M MODULE.M; do
    test -f $f && modfiles="$f"
  done
  if test "$modfiles" = "file.o"; then
    FFLAGS=$OLD_FFLAGS
    echo no 6>&1
  else
    echo yes 6>&1
  fi
fi
cd ..

AC_MSG_CHECKING(how $F9X finds modules)

for flag in "-I" "-M" "-p"; do
  cat >conftest.$ac_ext <<EOF
      program conftest
          use module
      end program conftest
EOF

  ac_compile='${F9X-f90} $FFLAGS ${flag}conftestdir -c conftest.$ac_ext 1>&AS_MESSAGE_LOG_FD'

  if AC_TRY_EVAL(ac_compile); then
    F9XMODFLAG=$flag
    break
  fi
done

if test -n "$F9XMODFLAG"; then
  echo $F9XMODFLAG 1>&6
#  FFLAGS="$F9XMODFLAG. $F9XMODFLAG../src $FFLAGS"
  FFLAGS="$F9XMODFLAG. $FFLAGS"
else
  echo unknown 1>&6
fi
AC_SUBST(F9XMODFLAG)
AC_SUBST(F9XMODEXT)
rm -rf conftest*
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
[AC_LANG_SAVE
AC_LANG_FORTRAN9X

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
[AC_LANG_SAVE
AC_LANG_FORTRAN9X

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
[AC_LANG_SAVE
AC_LANG_FORTRAN9X

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
dnl $ac_cv_prog_F90 $FFLAGS -o conftest conftest.f90 > /dev/null 2>&1
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
dnl $ac_cv_prog_F90 $FFLAGS -o conftest conftest.f90 > /dev/null 2>&1
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
dnl $ac_cv_prog_F90 $FFLAGS -o conftest conftest.f90 > /dev/null 2>&1
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
dnl $ac_cv_prog_F90 $FFLAGS -o conftest conftest.f90 > /dev/null 2>&1
dnl if test "`./conftest | head -1`" = YES; then
dnl 	echo "yes" 1>&6
dnl 	AC_DEFINE(USEINT)
dnl else
dnl 	echo "no" 1>&6
dnl fi
dnl
dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------
