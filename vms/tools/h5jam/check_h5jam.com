$ !
$ ! This command file tests h5jam and h5unjam utilities. The command file has to
$ ! run in the [hdf5-top.tools.testfiles] directory.
$ !
$ !
$ ! Define h5jam, h5unjam and h5dump symbols
$ !
$! set message/notext/nofacility/noidentification/noseverity
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-10, current_dir)
$ h5dump_dir = temp + "H5DUMP]"
$ h5dump :== $sys$disk:'h5dump_dir'h5dump.exe
$ h5jam_dir = temp + "H5JAM]"
$ h5jam :== $sys$disk:'h5jam_dir'h5jam.exe
$ !
$ ! Define output for diff command that compares expected and actual
$ ! outputs of h5dump
$ !
$ create h5dump.log
$ !
$ ! h5jam and h5unjam tests
$ !
$
$ CALL H5JAMTEST "-u u10.txt -i" tall.h5 ta2.h5
$! CALL H5JAMTEST "-u u511.txt -i" tall.h5 ta3.h5
$! CALL H5JAMTEST "-u u512.txt -i" tall.h5 ta4.h5
$! CALL H5JAMTEST "-u u513.txt -i" tall.h5 ta5.h5
$
$ 
$H5JAMTEST: SUBROUTINE
$
$ len_org  = F$LENGTH(P2)
$ base_org = F$EXTRACT(0,len_org-2,P2)
$ len      = F$LENGTH(P3)
$ base     = F$EXTRACT(0,len-2,P3)
$ actual         = base + "out"
$ actual_org     = base_org + "org"
$ actual_err     = base + "err"
$ actual_err_org = base_org + "err"
$
$ ON ERROR THEN CONTINUE
$ begin = "Testing h5jam"
$ !
$ ! Run the test 
$ !
$ h5jam 'P1 'P2 -o 'P3
$ !
$ ! Dump the original and result file
$ !
$ define/nolog sys$output 'actual'
$ define/nolog sys$error  'actual_err'
$ h5dump 'P3
$ deassign sys$output
$ deassign sys$error
$ if F$SEARCH(actual_err) .NES. ""
$ then
$ set message/notext/nofacility/noidentification/noseverity
$    append 'actual_err' 'actual'
$ set message/ntext/facility/identification/severity
$ endif
$ define/nolog sys$output 'actual_org'
$ define/nolog sys$error  'actual_err_org'
$ h5dump 'P2
$ deassign sys$output
$ deassign sys$error
$ if F$SEARCH(actual_err_org) .NES. ""
$ then
$ set message/notext/nofacility/noidentification/noseverity
$    append 'actual_err_org' 'actual_org'
$ set message/ntext/facility/identification/severity
$ endif
$ !
$ ! Compare the results
$ !
$ diff/output=h5dump_temp/ignore=(spacing,trailing_spaces,blank_lines) 'actual_org' -
                                                                       'actual'
$ !
$ ! Delete output file 'P3
$ !
$ del 'P3;*
$ open/read temp_out h5dump_temp.dif
$READ_DATA:
$ read/end_of_file=end_read temp_out record1
$ !
$ ! Skip blank lines
$ !
$ if record1 .EQS. "" then goto READ_DATA
$ !
$ ! Find record with "Number" and exit the loop
$ !
$ len = F$LENGTH(record1)
$ pos = F$LOCATE("Number", record1)
$ !
$ if pos .EQ. 0  
$ then 
$     err_code = F$EXTRACT(len-1,1,record1)
$     goto END_READ
$ endif
$ !
$ goto READ_DATA
$
$ !
$END_READ: 
$ close temp_out
$ 
$ !
$ ! Extract error code and format output line
$ !
$ len = F$LENGTH(record1)
$ err_code = F$EXTRACT(len-1,1,record1)
$ if err_code .eqs. "1" 
$  then
$    result = "PASSED"
$    line = F$FAO("!14AS !14AS !7AS !2AS !25AS !100AS", begin, P1, P2, -
                                                       "-o", P3, result) 
$  else
$    result = "*FAILED*"
$    line = F$FAO("!14AS !14AS !7AS !2AS !25AS !99AS", begin, P1, P2, -
                                                       "-o", P3, result) 
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
$! del *.out;*
$! del *.org;*
$! del *.dif;*
$ !
$ENDSUBROUTINE

