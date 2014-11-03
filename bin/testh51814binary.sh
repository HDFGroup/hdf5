#! /bin/sh

FORTRAN=yes
ARG=$1
if [ -n "${ARG}" ] && [ "${ARG}" = "-nofortran" ]; then
  FORTRAN=no
fi

echo "Fortran:  ${FORTRAN}"

THIS_DIR=`pwd`

THIS_TEST_RESULT=0
TEST_RESULT=0
FILE_LIST=`ls`
RESULTSLOG=""
SUMMARYLOG=""
EXAMPLE1LOG=""
EXAMPLE2LOG=""
FAILED_TEST_LOGS=""

# step 0:  Check presence of top-level README file
STEP0RESULT=0
README_FILE=""
README_FILE=`ls README`

echo "Check for presence of top-level README file"
if test -z "$README_FILE"; then
  echo "Missing Outer README file."
  STEP0RESULT=1
else
  echo "Present"
fi

for f in $FILE_LIST ; do
  #be sure to start in the "top level" directory
  cd $THIS_DIR
  IS_TARFILE=""
  UTILTEST=""
  THIS_TEST_RESULT=0
  
  IS_TARFILE=`ls $f | grep tar.gz`

  if [ "$f" = "utilities" ]; then
    UTILTEST="yes"
    cd utilities
    RESULTSLOG=$THIS_DIR/utilities_Results.log
    SUMMARYLOG=$THIS_DIR/utilities_Summary
    echo "    SUMMARY" > $SUMMARYLOG
    echo ""
    echo "Testing $f"
    echo ""
  elif [ -n "$IS_TARFILE" ] ; then
    UTILTEST=""
    EXTRACTED=`echo $f | sed 's/\.tar\.gz//'`
    STORSH=`echo $EXTRACTED | awk -F- '{ print $NF }'`

    if [ "$STORSH" = "shared" ]; then
      SHAREDLIBS=yes
    else
      SHAREDLIBS=""
    fi
    echo ""
    echo "Testing $f"
    
    echo
    echo "Unpack and install $f."
    echo 
    EXTRACTLOG=extract_$f.log
    RESULTSLOG=$THIS_DIR/${STORSH}_Results.log
    SUMMARYLOG=$THIS_DIR/${STORSH}_Summary
    EXAMPLE1LOG=$THIS_DIR/${STORSH}_hdf5-examples.log
    EXAMPLE2LOG=$THIS_DIR/${STORSH}_share_examples.log
    echo "    SUMMARY" > $SUMMARYLOG

    
  # Step 1:  unpack binary and run h5redeploy
  # Extraction not needed when testing the utilities directory ($UTILTEST = "yes")
    echo "Executing step 1:  Extracting $f" 
    tar zxvf $f > $EXTRACTLOG
    cd $EXTRACTED/bin
    # Check for internal README file in $EXTRACTED
    echo "Check for presence of top-level README file"
    STEP1RESULT=0
    README_FILE=""
    README_FILE=`ls ../README`

    if test -z "$README_FILE"; then
      echo "Missing Internal README file."
      STEP1RESULT=1
    else
      echo "Present"
    fi
    
  else 
    continue
  fi
  
  ./h5redeploy -force > ${RESULTSLOG}

  # Steps 2 & 3 don't apply for testing the utilities directory ($UTILTEST = "yes")
  if [ -z "$UTILTEST" ]; then
    # For testing tar files, go back up to the test directory 
    cd $THIS_DIR
    
    # Step 2
    STEP2RESULT=0
    echo "Executing step 2: Check and verify contents of the lib/libhdf5.settings file." 
    
    # 2.1 Check Platform
    # The platform information won't match exactly and is somewhat irregular.  We
    # can write a script with awk and sed to match enough information to determine
    # a match, but it will take some thought and research to determine which parts 
    # to check.  For now, present the output of "uname -a" and the "Uname information"
    # line from libhdf5.settings and let the developer decide.
    
    MACH_UNAME=`uname -a`
    BIN_UNAME=`grep "Uname information" $EXTRACTED/lib/libhdf5.settings  | sed 's/\s*Uname information://'`

    IS_SOLARIS=`echo $BIN_UNAME | grep SunOS`
    IS_PPC64=`echo$BIN_UNAME | grep ppc64`
    IS_MAC=`echo $BIN_UNAME | grep Darwin`
#    if [ -n "$IS_MAC" ]; then
#    IS_MAC32=`echo $BIN_UNAME | grep tejeda`
#    IS_MAC64=`echo $BIN_UNAME | grep fred`
#    fi
    # at present we only need to know if the binary is for mac or not, so if

#    echo "The output of \"uname -a\" for this machine is:"
#    echo ""
#    echo "    $MACH_UNAME"
#    echo ""
#    echo "The \"Uname information\" line from the libhdf5.settings in this binary is:"
#    echo ""
#    echo "    $BIN_UNAME"
#    echo ""

    # Probably we won't ask, just print the results in the summary
    #echo "Is this the correct binary to install on this machine? (yes/no - yes to continue, no to exit)"
    #read CORRECT
    #if [ "$CORRECT" = "yes" ]; then
    #  echo "Continuing with binary tests."
    #else
    #  echo "Exiting - Try downloading another binary for this machine."
    #fi
    
    # 2.2 Check compiler versions in libhdf5.settings against RELEASE.txt
    # Need to compare both files in detail to find unique string to compare 
#    grep -A 70 "^Platforms Tested$"  $EXTRACTED/RELEASE.txt
#    echo ""
#    echo "We need to check that the compilers listed in"
#    echo "$EXTRACTED/lib/libhdf5.settings are also listed"
#    echo "in the \"Platforms Tested\" section of $EXTRACTED/RELEASE.txt, displayed above."
#    echo "Please find the compiler version information for the following compilers"
#    echo "and for this platform, $BIN_UNAME, in the \"Platforms Tested\" section (printed above)"
#    echo "and enter it below."
#    echo ""
#    echo "For multiple lines copy a line at a time separated by a space."
#    echo ""
#    echo "If the compiler cannot in the \"Platforms Tested\" section above please check the RELEASE.txt file."
#    echo "If no match is found there, please enter \"NO MATCH\"" 
#    
    C_COMPILER=`grep "C Compiler:" $EXTRACTED/lib/libhdf5.settings | sed 's/\s*//'`
    F_COMPILER=`grep "Fortran Compiler:" $EXTRACTED/lib/libhdf5.settings | sed 's/\s*//'`
    CXX_COMPILER=`grep "C++ Compiler:" $EXTRACTED/lib/libhdf5.settings | sed 's/\s*//'`
#    echo ""
#    echo $C_COMPILER
#    echo ""
#    echo $F_COMPILER
#    echo ""
#    echo $CXX_COMPILER
#    echo ""
#    echo "Enter info from RELEASE.txt for C Compiler:"
#    read C_COMP_INFO
#    echo "Enter info for Fortran Compiler:"
#    read F_COMP_INFO
#    echo "Enter info for C++ Compiler:"
#    read CXX_COMP_INFO
    
    # 2.3 Check libhdf5.settings for Shared and Static (Static Executables?)
    STATIC=`grep Static $EXTRACTED/lib/libhdf5.settings | grep Library | grep yes`
    RETVAL=$?
    if [ "$RETVAL" = "0" ]; then
       #echo "Static Libraries PASSED."
       echo "Static Libraries PASSED." >> $RESULTSLOG
       
    else
       #echo "No static libraries: FAILED."
       STEP2RESULT=1
       echo "No static libraries: FAILED." >> $RESULTSLOG
    fi
    
    if [ -n "$SHAREDLIBS" ]; then
      SHARED=`grep Shared $EXTRACTED/lib/libhdf5.settings | grep Library | grep yes`
     RETVAL=$?
      if [ "$RETVAL" = "0" ]; then
       #  echo "Shared Libraries present: PASSED."
       #  echo $SHARED
       echo "Shared Libraries present: PASSED." >> $RESULTSLOG
      else
       #  echo "Shared libraries are missing: FAILED."
       STEP2RESULT=1
       echo "Shared libraries are missing: FAILED." >> $RESULTSLOG
      fi
    fi
    
    #2.4 Check for Fortran support
    SUPPORT=`grep "Fortran: yes" $EXTRACTED/lib/libhdf5.settings`
 RETVAL=$?
    if [ "$RETVAL" = "0" ]; then
    #  echo "Fortran supported: PASSED"
      echo "Fortran supported: PASSED" >> $RESULTSLOG
    else
    #  echo "Fortran not supported: FAILED"
       STEP2RESULT=1
      echo "Fortran not supported: FAILED" >> $RESULTSLOG
    fi 
    
    #2.5 Check for C++ support
    SUPPORT=`grep "C++: yes" $EXTRACTED/lib/libhdf5.settings`
    if [ "$RETVAL" = "0" ]; then
      # echo "C++ supported: PASSED"
      echo "C++ supported: PASSED" >> $RESULTSLOG
    else
      # echo "C++ not supported: FAILED"
      STEP2RESULT=1
      echo "C++ not supported: FAILED" >> $RESULTSLOG
    fi
     
    #2.6 Check that zlib and szip are enabled
    SUPPORT=`grep "I/O filters (external): " $EXTRACTED/lib/libhdf5.settings | grep "deflate(zlib)"`
    if [ "$RETVAL" = "0" ]; then
      echo "zlib supported: PASSED" >> $RESULTSLOG
    else
      STEP2RESULT=1
      echo "zlib not supported: FAILED" >> $RESULTSLOG
    fi
    SUPPORT=`grep "I/O filters (external): " $EXTRACTED/lib/libhdf5.settings | grep "szip(encoder)"`
    if [ "$RETVAL" = "0" ]; then
      # echo "szip with encoder supported: PASSED"
      echo "szip with encoder supported: PASSED" >> $RESULTSLOG
    else
      # echo "szip with encoder not supported: FAILED"
      STEP2RESULT=1
      echo "szip with encoder not supported: FAILED" >> $RESULTSLOG
    fi
    
    #2.7 Check for Compilation Mode = production
    PRODUCTION=`grep "Compilation Mode: production" $EXTRACTED/lib/libhdf5.settings`
    if [ "$RETVAL" = "0" ]; then
      #echo "Compilation Mode is production: PASSED"
      echo "Compilation Mode is production: PASSED" >> $RESULTSLOG
    else
      #echo "Compilation Mode is development: FAILED"
      STEP2RESULT=1
      echo "Compilation Mode is development: FAILED" >> $RESULTSLOG
    fi
    
    #2.8 Check for optimization flag
    OPTIMIZED=`grep -i FLAGS $EXTRACTED/lib/libhdf5.settings | grep "\-O"`
    if [ "$RETVAL" = "0" ]; then
      # echo "Optimization is on: PASSED"
      echo "Optimization is on: PASSED" >> $RESULTSLOG
    else
      # echo "No optimization found in FLAGS: FAILED"
      STEP2RESULT=1
      echo "No optimization found in FLAGS: FAILED" >> $RESULTSLOG
    fi
    
    #2.9 Check for no "-g" flags
    DEBUG=`grep -i FLAGS $EXTRACTED/lib/libhdf5.settings | grep " \-g "`
    RETVAL=$?
    if [ "$RETVAL" = "0" ]; then
      #echo "-g flag is present: $DEBUG FAILED"
      echo "-g flag is present: $DEBUG FAILED" >> $RESULTSLOG
    else
      #echo "-g flag is not present:  PASSED"
      echo "-g flag is not present:  PASSED" >> $RESULTSLOG
    fi
    
    if [ "$STEP2RESULT" = "0" ]; then
      echo "PASSED"
      echo "Step 2 PASSED" >> $SUMMARYLOG
    else
      echo "**** FAILED"
      echo "**** Step 2 FAILED ****" >> $RESULTSLOG
      echo "**** Step 2 FAILED ****" >> $SUMMARYLOG
    fi

    # Step 3
    echo "Executing step 3:  Check and verify included libraries."
    STEP3RESULT=0
    
    RETVAL=0
    THIS_DIR=`pwd`
    cd $EXTRACTED/lib
    STATIC_LIBFILES="libhdf5.a libhdf5_cpp.a libhdf5_fortran.a libhdf5_hl.a libhdf5_hl_cpp.a libhdf5hl_fortran.a libhdf5_cpp.a"
    for s in $STATIC_LIBFILES ; do
      if test -f $s; then
        echo "found $s" >> $RESULTSLOG
      else
        echo "$s not found." >> $RESULTSLOG
        RETVAL=1
      fi
    done
    if [ -n "$IS_MAC" ]; then  
      # Currently no shared libraries for Fortran are produced on the mac-intel machines
      SHARED_LIBFILES="libhdf5_cpp.9.dylib libhdf5_hl_cpp.9.dylib libhdf5_hl.9.dylib libhdf5.9.dylib libsz.a"
    elif [ -n "$IS_PPC64" ]; then
      SHARED_LIBFILES="libhdf5_cpp.so.9.0.0 libhdf5_fortran.so.9.0.0 libhdf5_hl_cpp.so.9.0.0 libhdf5hl_fortran.so.9.0.0 libhdf5_hl.so.9.0.0 libhdf5.so.9.0.0"
    else
      SHARED_LIBFILES="libhdf5_cpp.so.9.0.0 libhdf5_fortran.so.9.0.0 libhdf5_hl_cpp.so.9.0.0 libhdf5hl_fortran.so.9.0.0 libhdf5_hl.so.9.0.0 libhdf5.so.9.0.0 libsz.so.2.0.0 libz.so.1.2.5"
    fi
    if [ -n "${SHAREDLIBS}" ]; then
      for s in $SHARED_LIBFILES ; do
        if test -f $s; then
          echo "found $s" >> $RESULTSLOG
        else
          echo "$s not found." >> $RESULTSLOG
          RETVAL=1
        fi
      done
      if [ "$RETVAL" = "0" ]; then
        #echo "All shared and static library files are included: PASSED"
        echo "All shared and static library files are included: PASSED" >> $RESULTSLOG
      else
        #echo "Expected library files are missing: FAILED"
        STEP3RESULT=1
        echo "Expected library files are missing: FAILED" >> $RESULTSLOG
      fi
    else
      EXTLIBS="libsz.a libz.a"
      for l in $EXTLIBS ; do
        if test -f $l; then
          echo "found $l" >> $RESULTSLOG
        else
          echo "$l not found." >> $RESULTSLOG
          RETVAL=1
        fi
      done
      if [ "$RETVAL" = "0" ]; then
        # check to see if any shared library files are in the directory
        # 'ls' returns 0 if it finds a match, non-zero if no match is found.
        ls *.so* >& /dev/null
        RETVAL=$?
        if [ "$RETVAL" = "0" ]; then
          #echo "Unexpected shared library files are present: FAILED"
          STEP3RESULT=1
          echo "Unexpected shared library files are present: FAILED" >> $RESULTSLOG
        else
          #echo "Static library files are included and no shared libraries are present: PASSED"
          echo "Static library files are included and no shared libraries are present: PASSED" >> $RESULTSLOG
        fi
      else
        #echo "Expected library files are missing: FAILED"
        STEP3RESULT=1
        echo "Expected library files are missing: FAILED" >> $RESULTSLOG
      fi
    fi
    if [ "$STEP3RESULT" = "0" ]; then
      echo "PASSED"
      echo "Step 3 PASSED" >> $SUMMARYLOG
    else
      echo "**** FAILED ****"
      echo "**** Step 3 FAILED ****" >> $RESULTSLOG
      echo "**** Step 3 FAILED ****" >> $SUMMARYLOG
    fi
    # change to the $EXTRACTED/bin directory.  
    # For utilities testing we should already be in the utilities directory
    cd ../bin
  fi
  
  # Step 4
  echo "Executing step 4:  Verify binary platform type."
  STEP4RESULT=0
 
  # This step is included to check that the binaries built with --enable-static-exec
  # aren't dynamically linked to any libraries.  --enable-static-exec doesn't work on 
  # solaris, and our macs and ppc64 only have dynamic zlib libraries, so we skip this step for 
  # those machines.
  if [ -z "$IS_SOLARIS" ] && [ -z "$IS_MAC" ] && [ -z "$IS_PPC64" ]; then 
    RETVAL=0
    EXEC_BINFILES="gif2h5 h52gif h5copy h5debug h5diff h5dump h5import h5jam h5ls h5mkgrp h5perf_serial h5repack h5repart h5stat h5unjam"
    for e in $EXEC_BINFILES ; do
      if [ -n "$IS_MAC" ]; then
        OUTPUT=`otool -L $e`
      else
        OUTPUT=`ldd $e`
      fi
      match_number=`expr "$OUTPUT" : '\s*not a dynamic executable'`
  
      if [ "$match_number" = "25" ]; then
        #echo "$e $OUTPUT       PASSED"
        echo "$e $OUTPUT       PASSED" >> $RESULTSLOG
      else
        #echo "$e $OUTPUT       Please report whether this output is acceptable"
        STEP4RESULT=1
        echo "$e $OUTPUT       Please report whether this output is acceptable" >> $RESULTSLOG
      fi
    done
    if [ "$STEP4RESULT" = "0" ]; then
      #echo "Executable files ran successfully on this platform" 
      echo "PASSED"
      echo "Step 4 PASSED" >> $SUMMARYLOG
    else
      #echo "Not all executable files ran successfully on this platform"
      echo "**** FAILED ****"
      echo "**** Step 4 FAILED ****" >> $RESULTSLOG
      echo "**** Step 4 FAILED.  See $RESULTSLOG for details ****" >> $SUMMARYLOG 
    fi
  fi
   
  if [ -z "$UTILTEST" ]; then
    # Step 5
    echo "Executing step 5:  Compile and test examples in hdf5-examples repository with the tar file's h5cc, h5fc and h5c++."
  
    #return to the top level test directory (the starting directory)
    cd $THIS_DIR
    STEP5RESULT=0
   
    # For the macs, we need DYLD_LIBRARY_PATH set to the lib directory of the hdf5 to be tested.
    if [ -n "${IS_MAC}" ] && [ "${SHAREDLIBS}" = "yes" ]; then
        DYLD_LIBRARY_PATH=$THIS_DIR/$EXTRACTED/lib
        export DYLD_LIBRARY_PATH
    fi

    if test -d hdf5-examples ; then
      rm -rf hdf5-examples
    fi
    svn co https://svn.hdfgroup.uiuc.edu/hdf5-examples/trunk/ hdf5-examples > checkout.log
    CC=$THIS_DIR/$EXTRACTED/bin/h5cc
    export CC
    FC=$THIS_DIR/$EXTRACTED/bin/h5fc
    export FC
    # hdf5-examples/1_8/FORTRAN fails with -O2 or higher
    if [ -n "$IS_MAC" ]; then
      FCFLAGS=-O1
      export FCFLAGS
    fi
    CXX=$THIS_DIR/$EXTRACTED/bin/h5c++
    export CXX 
    H5EX_16=yes
    export H5EX_16
    H5EX_18=yes
    export H5EX_18
    cd hdf5-examples
    ./configure > ${EXAMPLE1LOG} 
    if [ "${FORTRAN}" = "yes" ]; then
      make check > ${EXAMPLE1LOG}
    else 
      cd 1_6
      make check > ${EXAMPLE1LOG}
      cd ../1_8/C
      make check >> ${EXAMPLE1LOG}
      cd ../..  
    fi
    RETVAL=`echo $?`
    NUMFAILED=`grep -c FAILED ${EXAMPLE1LOG}`
    NUMFAILED0=`grep -c "# FAIL:  0" ${EXAMPLE1LOG}`
    NUMPASSED=`grep -c "# PASS:  1" ${EXAMPLE1LOG}`
    NUMTOTAL=`grep -c "# TOTAL: 1" ${EXAMPLE1LOG}`
    NUMERR=`grep -i error ${EXAMPLE1LOG} | grep -v "# ERROR: 0" | grep -ic error`
    NUMERR0=`grep -c "# ERROR: 0" ${EXAMPLE1LOG}`
#    NUMPASSREQ=120
#    if [ "${FORTRAN}" = "no" ]; then
#      NUMPASSREQ=102
#    fi
    
    if test  "$RETVAL" = "0" -a "$NUMFAILED" = "0" ; then
      if test  "$NUMERR" = "0" -a "$NUMPASSED" = "$NUMTOTAL" -a "$NUMERR0" = "$NUMTOTAL" -a "$NUMFAILED0" = "$NUMTOTAL"  ; then
        echo "PASSED"
        echo "Step 5 PASSED:  $NUMPASSED tests passed in hdf5-examples with no failures or errors" >> ${RESULTSLOG}
        echo "Step 5 PASSED:  $NUMPASSED tests passed in hdf5-examples with no failures or errors" >> ${SUMMARYLOG}
      else
        STEP5RESULT=1
        if test $NUMERR -gt 0 -o $NUMFAILED -gt 0 ; then
          echo "**** FAILED ****"
          echo "**** Step 5 FAILED:  Some examples failed.  There were $NUMERR errors.  See details in ${EXAMPLE1LOG} ****" >> ${RESULTSLOG}
          echo "**** Step 5 FAILED:  Some examples failed.  There were $NUMERR errors.  See details in ${EXAMPLE1LOG} ****" >> ${SUMMARYLOG}
        else
          echo "**** Step 5 FAILED:  $NUMPASSED examples passed;  there should have been at least $NUMTOTAL.  See details in ${EXAMPLE1LOG} ****" >> ${RESULTSLOG} 
          echo "**** Step 5 FAILED:  $NUMPASSED examples passed;  there should have been at least $NUMTOTAL.  See details in ${EXAMPLE1LOG} ****" >> ${SUMMARYLOG} 
        fi
      fi
    else
      STEP5RESULT=1
      echo "**** FAILED ****"
      echo "**** Step 5 FAILED:  Some examples failed.  There were $NUMERR errors.  See details in ${EXAMPLE1LOG} ****" >> ${RESULTSLOG}
      echo "**** Step 5 FAILED:  Some examples failed.  There were $NUMERR errors.  See details in ${EXAMPLE1LOG} ****" >> ${SUMMARYLOG}
    fi
  
    cd $THIS_DIR

    # Step 6 NA for utilities  

    # Step 6
    echo "Executing step 6:  Use run-all-ex.sh to compile and run installed examples."
    STEP6RESULT=0
    
    cd $EXTRACTED/share/hdf5_examples
    if [ "${FORTRAN}" = "no" ]; then
      echo "Run c examples"
      if ((cd c; sh ./run-c-ex.sh) && \
         (if test -d c++; then
            echo "Run c++ examples"
            cd c++; sh ./run-c++-ex.sh
          fi) && \
         (if test -d hl; then
            echo "Run hl examples."
            cd hl && \
            (cd c; sh ./run-hlc-ex.sh) && \
            (if test -d c++; then
               echo "Run hl c++ examples"
               cd c++; sh ./run-hlc++-ex.sh
             fi)
          fi)) > $EXAMPLE2LOG; then
        echo "Done"
        RETVAL=0
      else
        RETVAL=1
      fi
    else
      sh ./run-all-ex.sh > $EXAMPLE2LOG
      RETVAL=$?
    fi

    
    if [ "$RETVAL" = "0" ]; then
      echo "PASSED"
      echo "Step 6 PASSED:  The examples in .../share/hdf5_examples were successfully compiled and ran without errors." >> ${RESULTSLOG}
      echo "Step 6 PASSED:  The examples in .../share/hdf5_examples were successfully compiled and ran without errors." >> ${SUMMARYLOG}
    else
      echo "**** FAILED ****"
      STEP6RESULT=1
      "**** Step 6 FAILED:  The examples in .../share/hdf5_examples failed to compile or run.  See details in $EXAMPLE2LOG ****" >> ${RESULTSLOG}
      "**** Step 6 FAILED:  The examples in .../share/hdf5_examples failed to compile or run.  See details in $EXAMPLE2LOG ****" >> ${SUMMARYLOG}
    fi
  fi
  

  # Step 7
  echo "Executing step 7:  Run command -V with executables to be sure that they will run."
  
  STEP7PASS=0
  STEP7FAIL=0
  STEP7RESULT=0
  # h5debug and h5perf_serial don't return 0 with -V, so they aren't in this list.  -h would be another
  # option, but even more bin files don't return 0. h5redeploy is tested as part of install, and doesn't 
  # support -V.
  EXEC_BINFILES="gif2h5 h52gif h5copy h5diff h5dump h5import h5jam h5ls h5mkgrp h5repack h5repart h5stat h5unjam"
  for e in $EXEC_BINFILES ; do
    OUTPUT=`$THIS_DIR/$EXTRACTED/bin/$e -V`
    RETVAL=$?
  
    if [ "$RETVAL" = "0" ]; then
      echo "$e  $OUTPUT        PASSED" >> $RESULTSLOG
      STEP7PASS=`expr $STEP7PASS + 1`
    else
      echo "$e  $OUTPUT        FAILED" >> $RESULTSLOG
      STEP7FAIL=`expr $STEP7FAIL + 1`
    fi
  done
  if [ $STEP7PASS -gt 12 ] && [ $STEP7FAIL -eq 0 ]; then
    echo "PASSED"
    echo "Step 7 PASSED: $STEP7PASS executable files in bin ran with -V on this machine." >> $SUMMARYLOG
  else
    STEP7RESULT=1
    echo "**** FAILED ****"
    echo "**** Step 7 FAILED: $STEP7FAIL executable files in bin failed to run with -V on this machine. ****" >> $RESULTSLOG
    echo "**** Step 7 FAILED: $STEP7FAIL executable files in bin failed to run with -V on this machine. ****" >> $SUMMARYLOG
  fi
  
#  echo ""
#  echo "SUMMARY:"
#  echo ""
#  echo "Uname information:"
#  echo "    Machine: $MACH_UNAME"
#  echo "    Binary:  $BIN_UNAME"
#  echo ""
  
  
#  if [ "$C_COMP_INFO" = "NO MATCH" ]; then
#    echo "$C_COMPILER not found in RELEASE.txt"
#  else
#    echo "C Compiler:  $C_COMPILER - $C_COMP_INFO"
#  fi
#  if [ "$F_COMP_INFO" = "NO MATCH" ]; then
#    echo "$F_COMPILER not found in RELEASE.txt"
#  else
#    echo "Fortran Compiler:  $F_COMPILER - $F_COMP_INFO"
#  fi
#  if [ "$CXX_COMP_INFO" = "NO MATCH" ]; then
#    echo "$CXX_COMPILER not found in RELEASE.txt"
#  else
#    echo "C++ Compiler:  $CXX_COMPILER - $CXX_COMP_INFO"
#  fi
#  echo ""
  
 


  if [ "$STEP0RESULT" != "0" ]; then
    THIS_TEST_RESULT=1
    echo "Step 0 failed"
    STEP0RESULT=0
  fi
 
  if [ "$STEP1RESULT" != "0" ]; then
    THIS_TEST_RESULT=1
    echo "Step 1 failed"
    STEP1RESULT=0
  fi
 
  if [ "$STEP2RESULT" != "0" ]; then
    THIS_TEST_RESULT=1
    echo "Step 2 failed"
    STEP2RESULT=0
  fi
 
  if [ "$STEP3RESULT" != "0" ]; then
    THIS_TEST_RESULT=1
    echo "Step 3 failed"
    STEP3RESULT=0
  fi

  if [ "$STEP4RESULT" != "0" ]; then
    THIS_TEST_RESULT=1
    echo "Step 4 failed"
    STEP4RESULT=0
  fi
  
  if [ "$STEP5RESULT" != "0" ]; then
    THIS_TEST_RESULT=1
    echo "Step 5 failed"
    STEP5RESULT=0
  fi
  
  if [ "$STEP6RESULT" != "0" ]; then
    THIS_TEST_RESULT=1
    echo "Step 6 failed"
    STEP6RESULT=0
  fi
  
  if [ "$STEP7RESULT" != "0" ]; then
    THIS_TEST_RESULT=1
    echo "Step 7 failed"
    STEP7RESULT=0
  fi
  
  echo "Done testing $f"

  if [ "$THIS_TEST_RESULT" != "0" ]; then
    TEST_RESULT=1
    FAILED_TEST_LOGS="$FAILED_TEST_LOGS $RESULTSLOG"
  fi

done


echo "" 
if [ "$TEST_RESULT" = "0" ]; then
  echo "**** FINAL RESULT: All tests PASSED ****"
else
  echo "**** FINAL RESULT: TESTS FAILED! See FAILED details in $FAILED_TEST_LOGS ****"
fi
    echo ""
    echo "For this version we need to perform the two following checks manually.  We hope that for future versions these can also be automated, and any suggestions for accomplishing that are most welcome."
    echo ""
    echo "The output of \"uname -a\" for this machine is:"
    echo ""
    echo "    $MACH_UNAME"
    echo ""
    echo "The \"Uname information\" line from the libhdf5.settings in this binary is:"
    echo ""
    echo "    $BIN_UNAME"
    echo ""
    echo "Please verify that the above information for this machine is compatible with that from the binary."
    echo ""
    echo $C_COMPILER
    echo ""
    echo $F_COMPILER
    echo ""
    echo $CXX_COMPILER
    echo ""
    echo "Please check that these compiler versions are listed for this platform in the \"Platforms Tested\" section of the binary's RELEASE.txt file."

#cat $THIS_DIR/*Summary

