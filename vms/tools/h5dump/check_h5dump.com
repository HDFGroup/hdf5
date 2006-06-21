$ !
$ ! This command file tests h5dump utility. The command file has to
$ ! run in the [hdf5-top.tools.testfiles] directory.
$ !
$ !
$ ! Define h5dump symbol
$ !
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-10, current_dir)
$ h5dump_dir = temp + "H5DUMP]"
$ h5dump :== $sys$disk:'h5dump_dir'h5dump.exe
$ !
$ ! Define output for diff command that compares expected and actual
$ ! outputs of h5dump
$ !
$ create h5dump.log
$ !
$ ! h5dump tests
$ !
$ CALL TOOLTEST  tgroup-1.ddl "tgroup.h5"
$ ! CALL TOOLTEST  tgroup-x.ddl "tgroup.h5"
$
$
$TOOLTEST: SUBROUTINE
$
$ len =  F$LENGTH(P1)
$ base = F$EXTRACT(0,len-3,P1)
$ actual = base + "out"
$
$ begin = "Testing h5dump "
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ define sys$output 'actual'
$ write  sys$output "#############################"
$ write  sys$output "Expected output for 'h5dump ''P2''"
$ write  sys$output "#############################"
$ h5dump 'P2
$ deassign sys$output
$ !
$ ! Compare the results
$ !
$ diff/output=h5dump_temp 'actual' 'P1'
$ open/read temp_out h5dump_temp.dif
$ read temp_out record1
$ close temp_out
$ !
$ ! Extract error code and format output line
$ !
$ len = F$LENGTH(record1)
$ err_code = F$EXTRACT(len-1,1,record1)
$ if err_code .eqs. "0" 
$  then
$    result = "PASSED"
$    line = F$FAO("!15AS !50AS !70AS", begin, P1, result) 
$  else
$    result = "*FAILED*"
$    line = F$FAO("!15AS !49AS !69AS", begin, P1, result) 
$ endif
$ !
$ ! Print test result
$ ! 
$  write sys$output line
$ ! 
$ ! Append the result to the log file 
$ !
$ append h5dump_temp.dif h5dump.log
$ !
$ ! Delete temporary files
$ !
$ del *.out;*
$ !
$ENDSUBROUTINE

