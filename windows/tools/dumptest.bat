@REM Copyright by the Board of Trustees of the University of Illinois.
@REM All rights reserved.
@REM
@REM This file is part of HDF5.  The full HDF5 copyright notice, including
@REM terms governing use, modification, and redistribution, is contained in
@REM the files COPYING and Copyright.html.  COPYING can be found at the root
@REM of the source code distribution tree; Copyright.html can be found at the
@REM root level of an installed copy of the electronic HDF5 document set and
@REM is linked from the top-level documents page.  It can also be found at
@REM http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
@REM access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.

@ ECHO OFF

REM TOOLTEST tfamily.ddl --filedriver=family tfamily%05d.h5
REM TOOLTEST thlink.h5.xml --xml thlink.h5
REM TOOLTEST tmany.h5.xml --xml tmany.h5
REM were turned off due to unsolved failures.

mkdir temptest

cd h5dump%2\%1

h5dump%2 ..\..\testfiles\tgroup.h5 > ..\..\temptest\tgroup-1.results 2>..\..\temptest\tgroup-1_error.results
more ..\..\temptest\tgroup-1_error.results >> ..\..\temptest\tgroup-1.results
del ..\..\temptest\tgroup-1_error.results

h5dump%2 -g /g2 -g / -g /y ..\..\testfiles\tgroup.h5 > ..\..\temptest\tgroup-2.results 2>..\..\temptest\tgroup-2_error.results
more ..\..\temptest\tgroup-2_error.results >> ..\..\temptest\tgroup-2.results
del ..\..\temptest\tgroup-2_error.results

h5dump%2 ..\..\testfiles\tdset.h5 > ..\..\temptest\tdset-1.results 2>..\..\temptest\tdset-1_error.results
more ..\..\temptest\tdset-1_error.results >> ..\..\temptest\tdset-1.results
del ..\..\temptest\tdset-1_error.results

h5dump%2 -H -d dset1 -d /dset2 --dataset=dset3 ..\..\testfiles\tdset.h5 > ..\..\temptest\tdset-2.results 2>..\..\temptest\tdset-2_error.results
more ..\..\temptest\tdset-2_error.results >> ..\..\temptest\tdset-2.results
del ..\..\temptest\tdset-2_error.results

h5dump%2 ..\..\testfiles\tattr.h5 > ..\..\temptest\tattr-1.results 2>..\..\temptest\tattr-1_error.results
more ..\..\temptest\tattr-1_error.results >> ..\..\temptest\tattr-1.results
del ..\..\temptest\tattr-1_error.results

h5dump%2 -a /attr1 --attribute /attr4 --attribute=/attr5 ..\..\testfiles\tattr.h5 > ..\..\temptest\tattr-2.results 2>..\..\temptest\tattr-2_error.results
more ..\..\temptest\tattr-2_error.results >> ..\..\temptest\tattr-2.results
del ..\..\temptest\tattr-2_error.results

h5dump%2 --header -a /attr2 --attribute=/attr ..\..\testfiles\tattr.h5 > ..\..\temptest\tattr-3.results 2>..\..\temptest\tattr-3_error.results
more ..\..\temptest\tattr-3_error.results >> ..\..\temptest\tattr-3.results
del ..\..\temptest\tattr-3_error.results

h5dump%2 -A ..\..\testfiles\tnamed_dtype_attr.h5 > ..\..\temptest\tnamed_dtype_attr.results 2>..\..\temptest\tnamed_dtype_attr_error.results
more ..\..\temptest\tnamed_dtype_attr_error.results >> ..\..\temptest\tnamed_dtype_attr.results
del ..\..\temptest\tnamed_dtype_attr_error.results

h5dump%2 ..\..\testfiles\tslink.h5 > ..\..\temptest\tslink-1.results 2>..\..\temptest\tslink-1_error.results
more ..\..\temptest\tslink-1_error.results >> ..\..\temptest\tslink-1.results
del ..\..\temptest\tslink-1_error.results

h5dump%2 -l slink2 ..\..\testfiles\tslink.h5 > ..\..\temptest\tslink-2.results 2>..\..\temptest\tslink-2_error.results
more ..\..\temptest\tslink-2_error.results >> ..\..\temptest\tslink-2.results
del ..\..\temptest\tslink-2_error.results

h5dump%2 ..\..\testfiles\thlink.h5 > ..\..\temptest\thlink-1.results 2>..\..\temptest\thlink-1_error.results
more ..\..\temptest\thlink-1_error.results >> ..\..\temptest\thlink-1.results
del ..\..\temptest\thlink-1_error.results

h5dump%2 -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 ..\..\testfiles\thlink.h5 > ..\..\temptest\thlink-2.results 2>..\..\temptest\thlink-2_error.results
more ..\..\temptest\thlink-2_error.results >> ..\..\temptest\thlink-2.results
del ..\..\temptest\thlink-2_error.results

h5dump%2 -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 ..\..\testfiles\thlink.h5 > ..\..\temptest\thlink-3.results 2>..\..\temptest\thlink-3_error.results
more ..\..\temptest\thlink-3_error.results >> ..\..\temptest\thlink-3.results
del ..\..\temptest\thlink-3_error.results

h5dump%2 -g /g1 ..\..\testfiles\thlink.h5 > ..\..\temptest\thlink-4.results 2>..\..\temptest\thlink-4_error.results
more ..\..\temptest\thlink-4_error.results >> ..\..\temptest\thlink-4.results
del ..\..\temptest\thlink-4_error.results

h5dump%2 -d /dset1 -g /g2 -d /g1/dset2 ..\..\testfiles\thlink.h5 > ..\..\temptest\thlink-5.results 2>..\..\temptest\thlink-5_error.results
more ..\..\temptest\thlink-5_error.results >> ..\..\temptest\thlink-5.results
del ..\..\temptest\thlink-5_error.results

h5dump%2 ..\..\testfiles\tcompound.h5 > ..\..\temptest\tcomp-1.results 2>..\..\temptest\tcomp-1_error.results
more ..\..\temptest\tcomp-1_error.results >> ..\..\temptest\tcomp-1.results
del ..\..\temptest\tcomp-1_error.results

h5dump%2 -t /type1 --datatype /type2 --datatype=/group1/type3 ..\..\testfiles\tcompound.h5 > ..\..\temptest\tcomp-2.results 2>..\..\temptest\tcomp-2_error.results
more ..\..\temptest\tcomp-2_error.results >> ..\..\temptest\tcomp-2.results
del ..\..\temptest\tcomp-2_error.results

h5dump%2  -t /#6632:0 -g /group2 ..\..\testfiles\tcompound.h5 > ..\..\temptest\tcomp-3.results 2>..\..\temptest\tcomp-3_error.results
more ..\..\temptest\tcomp-3_error.results >> ..\..\temptest\tcomp-3.results
del ..\..\temptest\tcomp-3_error.results

h5dump%2 ..\..\testfiles\tcompound_complex.h5 > ..\..\temptest\tcompound_complex.results 2>..\..\temptest\tcompound_complex_error.results
more ..\..\temptest\tcompound_complex_error.results >> ..\..\temptest\tcompound_complex.results
del ..\..\temptest\tcompound_complex_error.results

h5dump%2 ..\..\testfiles\tnestedcomp.h5 > ..\..\temptest\tnestcomp-1.results 2>..\..\temptest\tnestcomp-1_error.results
more ..\..\temptest\tnestcomp-1_error.results >> ..\..\temptest\tnestcomp-1.results
del ..\..\temptest\tnestcomp-1_error.results

h5dump%2 ..\..\testfiles\tall.h5 > ..\..\temptest\tall-1.results 2>..\..\temptest\tall-1_error.results
more ..\..\temptest\tall-1_error.results >> ..\..\temptest\tall-1.results
del ..\..\temptest\tall-1_error.results

h5dump%2 --header -g /g1/g1.1 -a attr2 ..\..\testfiles\tall.h5 > ..\..\temptest\tall-2.results 2>..\..\temptest\tall-2_error.results
more ..\..\temptest\tall-2_error.results >> ..\..\temptest\tall-2.results
del ..\..\temptest\tall-2_error.results

h5dump%2 -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink ..\..\testfiles\tall.h5 > ..\..\temptest\tall-3.results 2>..\..\temptest\tall-3_error.results
more ..\..\temptest\tall-3_error.results >> ..\..\temptest\tall-3.results
del ..\..\temptest\tall-3_error.results

h5dump%2 ..\..\testfiles\tloop.h5 > ..\..\temptest\tloop-1.results 2>..\..\temptest\tloop-1_error.results
more ..\..\temptest\tloop-1_error.results >> ..\..\temptest\tloop-1-1.results
del ..\..\temptest\tloop-1_error.results

h5dump%2 ..\..\testfiles\tstr.h5 > ..\..\temptest\tstr-1.results 2>..\..\temptest\tstr-1_error.results
more ..\..\temptest\tstr-1_error.results >> ..\..\temptest\tstr-1.results
del ..\..\temptest\tstr-1_error.results

h5dump%2 ..\..\testfiles\tstr2.h5 > ..\..\temptest\tstr-2.results 2>..\..\temptest\tstr-2_error.results
more ..\..\temptest\tstr-2_error.results >> ..\..\temptest\tstr-2.results
del ..\..\temptest\tstr-2_error.results

h5dump%2 ..\..\testfiles\tsaf.h5 > ..\..\temptest\tsaf.results 2>..\..\temptest\tsaf_error.results
more ..\..\temptest\tsaf_error.results >> ..\..\temptest\tsaf.results
del ..\..\temptest\tsaf_error.results

h5dump%2 ..\..\testfiles\tvldtypes1.h5 > ..\..\temptest\tvldtypes1.results 2>..\..\temptest\tvldtypes1_error.results
more ..\..\temptest\tvldtypes1_error.results >> ..\..\temptest\tvldtypes1.results
del ..\..\temptest\tvldtypes1_error.results

h5dump%2 ..\..\testfiles\tvldtypes2.h5 > ..\..\temptest\tvldtypes2.results 2>..\..\temptest\tvldtypes2_error.results
more ..\..\temptest\tvldtypes2_error.results >> ..\..\temptest\tvldtypes2.results
del ..\..\temptest\tvldtypes2_error.results

h5dump%2 ..\..\testfiles\tvldtypes3.h5 > ..\..\temptest\tvldtypes3.results 2>..\..\temptest\tvldtypes3_error.results
more ..\..\temptest\tvldtypes3_error.results >> ..\..\temptest\tvldtypes3.results
del ..\..\temptest\tvldtypes3_error.results

h5dump%2 ..\..\testfiles\tvldtypes4.h5 > ..\..\temptest\tvldtypes4.results 2>..\..\temptest\tvldtypes4_error.results
more ..\..\temptest\tvldtypes4_error.results >> ..\..\temptest\tvldtypes4.results
del ..\..\temptest\tvldtypes4_error.results

h5dump%2 ..\..\testfiles\tvldtypes5.h5 > ..\..\temptest\tvldtypes5.results 2>..\..\temptest\tvldtypes5_error.results
more ..\..\temptest\tvldtypes5_error.results >> ..\..\temptest\tvldtypes5.results
del ..\..\temptest\tvldtypes5_error.results

h5dump%2 ..\..\testfiles\tvlstr.h5 > ..\..\temptest\tvlstr.results 2>..\..\temptest\tvlstr_error.results
more ..\..\temptest\tvlstr_error.results >> ..\..\temptest\tvlstr.results
del ..\..\temptest\tvlstr_error.results

h5dump%2 ..\..\testfiles\tarray1.h5 > ..\..\temptest\tarray1.results 2>..\..\temptest\tarray1_error.results
more ..\..\temptest\tarray1_error.results >> ..\..\temptest\tarray1.results
del ..\..\temptest\tarray1_error.results

h5dump%2 ..\..\testfiles\tarray2.h5 > ..\..\temptest\tarray2.results 2>..\..\temptest\tarray2_error.results
more ..\..\temptest\tarray2_error.results >> ..\..\temptest\tarray2.results
del ..\..\temptest\tarray2_error.results

h5dump%2 ..\..\testfiles\tarray3.h5 > ..\..\temptest\tarray3.results 2>..\..\temptest\tarray3_error.results
more ..\..\temptest\tarray3_error.results >> ..\..\temptest\tarray3.results
del ..\..\temptest\tarray3_error.results

h5dump%2 ..\..\testfiles\tarray4.h5 > ..\..\temptest\tarray4.results 2>..\..\temptest\tarray4_error.results
more ..\..\temptest\tarray4_error.results >> ..\..\temptest\tarray4.results
del ..\..\temptest\tarray4_error.results

h5dump%2 ..\..\testfiles\tarray5.h5 > ..\..\temptest\tarray5.results 2>..\..\temptest\tarray5_error.results
more ..\..\temptest\tarray5_error.results >> ..\..\temptest\tarray5.results
del ..\..\temptest\tarray5_error.results

h5dump%2 ..\..\testfiles\tarray6.h5 > ..\..\temptest\tarray6.results 2>..\..\temptest\tarray6_error.results
more ..\..\temptest\tarray6_error.results >> ..\..\temptest\tarray6.results
del ..\..\temptest\tarray6_error.results

h5dump%2 ..\..\testfiles\tarray7.h5 > ..\..\temptest\tarray7.results 2>..\..\temptest\tarray7_error.results
more ..\..\temptest\tarray7_error.results >> ..\..\temptest\tarray7.results
del ..\..\temptest\tarray7_error.results

h5dump%2 ..\..\testfiles\tempty.h5 > ..\..\temptest\tempty.results 2>..\..\temptest\tempty_error.results
more ..\..\temptest\tempty_error.results >> ..\..\temptest\tempty.results
del ..\..\temptest\tempty_error.results

h5dump%2 ..\..\testfiles\tgrp_comments.h5 > ..\..\temptest\tgrp_comments.results 2>..\..\temptest\tgrp_comments_error.results
more ..\..\temptest\tgrp_comments_error.results >> ..\..\temptest\tgrp_comments.results
del ..\..\temptest\tgrp_comments_error.results

h5dump%2 --filedriver=split ..\..\testfiles\tsplit_file > ..\..\temptest\tsplit_file.results 2>..\..\temptest\tsplit_file_error.results
more ..\..\temptest\tsplit_file_error.results >> ..\..\temptest\tsplit_file.results
del ..\..\temptest\tsplit_file_error.results

REM h5dump%2 --filedriver=family  ..\..\testfiles\tfamily%05d.h5 > ..\..\temptest\tfamily.results 2>..\..\temptest\thlink-5_error.results
REM more ..\..\temptest\tdset-1_error.results >> ..\..\temptest\tdset-1.results
REM del ..\..\temptest\tdset-1_error.results

h5dump%2 --filedriver=multi ..\..\testfiles\tmulti > ..\..\temptest\tmulti.results 2>..\..\temptest\tmulti_error.results
more ..\..\temptest\tmulti_error.results >> ..\..\temptest\tmulti.results
del ..\..\temptest\tmulti_error.results
 
h5dump%2 -w157 ..\..\testfiles\tlarge_objname.h5 > ..\..\temptest\tlarge_objname.results 2>..\..\temptest\tlarge_objname_error.results
more ..\..\temptest\tlarge_objname_error.results >> ..\..\temptest\tlarge_objname.results
del ..\..\temptest\tlarge_objname_error.results

h5dump%2 -A ..\..\testfiles\tall.h5 > ..\..\temptest\tall-2A.results 2>..\..\temptest\tall-2A_error.results
more ..\..\temptest\tall-2A_error.results >> ..\..\temptest\tall-2A.results
del ..\..\temptest\tall-2A_error.results
  
h5dump%2 --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 ..\..\testfiles\tall.h5 > ..\..\temptest\tall-4s.results 2>..\..\temptest\tall-4s_error.results
more ..\..\temptest\tall-4s_error.results >> ..\..\temptest\tall-4s.results
del ..\..\temptest\tall-4s_error.results

h5dump%2 -d "/g1/g1.1/dset1.1.2[0;2;10;]" ..\..\testfiles\tall.h5 > ..\..\temptest\tall-5s.results 2>..\..\temptest\tall-5s_error.results
more ..\..\temptest\tall-5s_error.results >> ..\..\temptest\tall-5s.results
del ..\..\temptest\tall-5s_error.results

h5dump%2 -d "/dset1[1,1;;;]" ..\..\testfiles\tdset.h5 > ..\..\temptest\tdset-3s.results 2>..\..\temptest\tdset-3s_error.results
more ..\..\temptest\tdset-3s_error.results >> ..\..\temptest\tdset-3s.results
del ..\..\temptest\tdset-3s_error.results
 
h5dump%2 -d "/dset1[;3,2;4,4;1,4]" ..\..\testfiles\tdset2.h5 > ..\..\temptest\tdset2-1s.results 2>..\..\temptest\tdset2-1s_error.results
more ..\..\temptest\tdset2-1s_error.results >> ..\..\temptest\tdset2-1s.results
del ..\..\temptest\tdset2-1s_error.results
 
h5dump%2 -r ..\..\testfiles\tchar.h5 > ..\..\temptest\tchar1.results 2>..\..\temptest\tchar1_error.results
more ..\..\temptest\tchar1_error.results >> ..\..\temptest\tchar1.results
del ..\..\temptest\tchar1_error.results

h5dump%2 1> ..\..\temptest\tnofilename.results 2> ..\..\temptest\tnofilename_error.results
more ..\..\temptest\tnofilename_error.results >> ..\..\temptest\tnofilename.results
del ..\..\temptest\tnofilename_error.results

h5dump%2 -H -B -d dset ..\..\testfiles\tfcontents1.h5 > ..\..\temptest\tboot1.results 2>..\..\temptest\tboot1_error.results
more ..\..\temptest\tboot1_error.results >> ..\..\temptest\tboot1.results
del ..\..\temptest\tboot1_error.results

h5dump%2 -B ..\..\testfiles\tfcontents2.h5 > ..\..\temptest\tboot2.results 2>..\..\temptest\tboot2_error.results
more ..\..\temptest\tboot2_error.results >> ..\..\temptest\tboot2.results
del ..\..\temptest\tboot2_error.results

h5dump%2 -p -d bogus ..\..\testfiles\tfcontents1.h5 > ..\..\temptest\tperror.results 2>..\..\temptest\tperror_error.results
more ..\..\temptest\tperror_error.results >> ..\..\temptest\tperror.results
del ..\..\temptest\tperror_error.results

h5dump%2 -n ..\..\testfiles\tfcontents1.h5 > ..\..\temptest\tcontents.results 2>..\..\temptest\tcontents_error.results
more ..\..\temptest\tcontents_error.results >> ..\..\temptest\tcontents.results
del ..\..\temptest\tcontents_error.results

h5dump%2 -H -p -d compact ..\..\testfiles\tfilters.h5 > ..\..\temptest\tcompact.results 2>..\..\temptest\tcompact_error.results
more ..\..\temptest\tcompact_error.results >> ..\..\temptest\tcompact.results
del ..\..\temptest\tcompact_error.results

h5dump%2 -H -p -d contiguous ..\..\testfiles\tfilters.h5 > ..\..\temptest\tcontiguos.results 2>..\..\temptest\tcontiguos_error.results
more ..\..\temptest\tcontiguos_error.results >> ..\..\temptest\tcontiguos.results
del ..\..\temptest\tcontiguos_error.results

h5dump%2 -H -p -d chunked ..\..\testfiles\tfilters.h5 > ..\..\temptest\tchunked.results 2>..\..\temptest\tchunked_error.results
more ..\..\temptest\tchunked_error.results >> ..\..\temptest\tchunked.results
del ..\..\temptest\tchunked_error.results

h5dump%2 -H -p -d external ..\..\testfiles\tfilters.h5 > ..\..\temptest\texternal.results 2>..\..\temptest\texternal_error.results
more ..\..\temptest\texternal_error.results >> ..\..\temptest\texternal.results
del ..\..\temptest\texternal_error.results

h5dump%2 -p ..\..\testfiles\tfvalues.h5 > ..\..\temptest\tfill.results 2>..\..\temptest\tfill_error.results
more ..\..\temptest\tfill_error.results >> ..\..\temptest\tfill.results
del ..\..\temptest\tfill_error.results

h5dump%2 ..\..\testfiles\tattr2.h5 > ..\..\temptest\treference.results 2>..\..\temptest\treference_error.results
more ..\..\temptest\treference_error.results >> ..\..\temptest\treference.results
del ..\..\temptest\treference_error.results

h5dump%2 -e ..\..\testfiles\tstr3.h5 > ..\..\temptest\tstringe.results 2>..\..\temptest\tstringe_error.results
more ..\..\temptest\tstringe_error.results >> ..\..\temptest\tstringe.results
del ..\..\temptest\tstringe_error.results

h5dump%2 ..\..\testfiles\tstr3.h5 > ..\..\temptest\tstring.results 2>..\..\temptest\tstring_error.results
more ..\..\temptest\tstring_error.results >> ..\..\temptest\tstring.results
del ..\..\temptest\tstring_error.results

h5dump%2 -r -d str4 ..\..\testfiles\tstr3.h5 > ..\..\temptest\tstring2.results 2>..\..\temptest\tstring2_error.results
more ..\..\temptest\tstring2_error.results >> ..\..\temptest\tstring2.results
del ..\..\temptest\tstring2_error.results

h5dump%2 ..\..\testfiles\taindices.h5 > ..\..\temptest\tindicesyes.results 2>..\..\temptest\tindicesyes_error.results
more ..\..\temptest\tindicesyes_error.results >> ..\..\temptest\tindicesyes.results
del ..\..\temptest\tindicesyes_error.results

h5dump%2 -y ..\..\testfiles\taindices.h5 > ..\..\temptest\tindicesno.results 2>..\..\temptest\tindicesno_error.results
more ..\..\temptest\tindicesno_error.results >> ..\..\temptest\tindicesno.results
del ..\..\temptest\tindicesno_error.results

if "%HDF5_EXT_SZIP%"==""  GOTO NEXTSTEP1
if %HDF5_EXT_SZIP%==szlib.lib (
   h5dump%2 -H -p -d szip ..\..\testfiles\tfilters.h5 > ..\..\temptest\tszip.results 2>..\..\temptest\tszip_error.results
   more ..\..\temptest\tszip_error.results >> ..\..\temptest\tszip.results
   del ..\..\temptest\tszip_error.results
)

:NEXTSTEP1

if "%HDF5_EXT_ZLIB%"=="" GOTO NEXTSTEP2
if %HDF5_EXT_ZLIB%==zlib.lib (
   h5dump%2 -H -p -d deflate ..\..\testfiles\tfilters.h5 > ..\..\temptest\tdeflate.results 2>..\..\temptest\tdeflate_error.results
   more ..\..\temptest\tdeflate_error.results >> ..\..\temptest\tdeflate.results
   del ..\..\temptest\tdeflate_error.results
)

:NEXTSTEP2

h5dump%2 -H -p -d shuffle ..\..\testfiles\tfilters.h5 > ..\..\temptest\tshuffle.results 2>..\..\temptest\tshuffle_error.results
more ..\..\temptest\tshuffle_error.results >> ..\..\temptest\tshuffle.results
del ..\..\temptest\tshuffle_error.results

h5dump%2 -H -p -d fletcher32 ..\..\testfiles\tfilters.h5 > ..\..\temptest\tfletcher32.results 2>..\..\temptest\tfletcher32_error.results
more ..\..\temptest\tfletcher32_error.results >> ..\..\temptest\tfletcher32.results
del ..\..\temptest\tfletcher32_error.results

if "%HDF5_EXT_ZLIB%"=="" GOTO NEXTSTEP3
if "%HDF5_EXT_SZIP%"=="" GOTO NEXTSTEP3
if %HDF5_EXT_ZLIB%==zlib.lib (
   if %HDF5_EXT_SZIP%==szlib.lib (
      h5dump%2 -H -p -d all ..\..\testfiles\tfilters.h5 > ..\..\temptest\tallfilters.results 2>..\..\temptest\tallfilters_error.results
      more ..\..\temptest\tallfilters_error.results >> ..\..\temptest\tallfilters.results
      del ..\..\temptest\tallfilters_error.results
   )
)

:NEXTSTEP3

h5dump%2 -H  -p -d myfilter ..\..\testfiles\tfilters.h5 > ..\..\temptest\tuserfilter.results 2>..\..\temptest\tuserfilter_error.results
more ..\..\temptest\tuserfilter_error.results >> ..\..\temptest\tuserfilter.results
del ..\..\temptest\tuserfilter_error.results

h5dump%2 ..\..\testfiles\tnullspace.h5 > ..\..\temptest\tnullspace.results 2>..\..\temptest\tnullspace_error.results
more ..\..\temptest\tnullspace_error.results >> ..\..\temptest\tnullspace.results
del ..\..\temptest\tnullspace_error.results

h5dump%2 --xml ..\..\testfiles\tall.h5 > ..\..\temptest\tall.h5.xml.results 2>..\..\temptest\tall.h5.xml_error.results
more ..\..\temptest\tall.h5.xml_error.results >> ..\..\temptest\tall.h5.xml.results
del ..\..\temptest\tall.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tarray1.h5 > ..\..\temptest\tarray1.h5.xml.results 2>..\..\temptest\tarray1.h5.xml_error.results
more ..\..\temptest\tarray1.h5.xml_error.results >> ..\..\temptest\tarray1.h5.xml.results
del ..\..\temptest\tarray1.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tarray2.h5 > ..\..\temptest\tarray2.h5.xml.results 2>..\..\temptest\tarray2.h5.xml_error.results
more ..\..\temptest\tarray2.h5.xml_error.results >> ..\..\temptest\tarray2.h5.xml.results
del ..\..\temptest\tarray2.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tarray3.h5 > ..\..\temptest\tarray3.h5.xml.results 2>..\..\temptest\tarray3.h5.xml_error.results
more ..\..\temptest\tarray3.h5.xml_error.results >> ..\..\temptest\tarray3.h5.xml.results
del ..\..\temptest\tarray3.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tarray6.h5 > ..\..\temptest\tarray6.h5.xml.results 2>..\..\temptest\tarray6.h5.xml_error.results
more ..\..\temptest\tarray6.h5.xml_error.results >> ..\..\temptest\tarray6.h5.xml.results
del ..\..\temptest\tarray6.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tarray7.h5 > ..\..\temptest\tarray7.h5.xml.results 2>..\..\temptest\tarray7.h5.xml_error.results
more ..\..\temptest\tarray7.h5.xml_error.results >> ..\..\temptest\tarray7.h5.xml.results
del ..\..\temptest\tarray7.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tattr.h5 > ..\..\temptest\tattr.h5.xml.results 2>..\..\temptest\tattr.h5.xml_error.results
more ..\..\temptest\tattr.h5.xml_error.results >> ..\..\temptest\tattr.h5.xml.results
del ..\..\temptest\tattr.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tbitfields.h5 > ..\..\temptest\tbitfields.h5.xml.results 2>..\..\temptest\tbitfields.h5.xml_error.results
more ..\..\temptest\tbitfields.h5.xml_error.results >> ..\..\temptest\tbitfields.h5.xml.results
del ..\..\temptest\tbitfields.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tcompound.h5 > ..\..\temptest\tcompound.h5.xml.results 2>..\..\temptest\tcompound.h5.xml_error.results
more ..\..\temptest\tcompound.h5.xml_error.results >> ..\..\temptest\tcompound.h5.xml-1.results
del ..\..\temptest\tcompound.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tcompound2.h5 > ..\..\temptest\tcompound2.h5.xml.results 2>..\..\temptest\tcompound2.h5.xml_error.results
more ..\..\temptest\tcompound2.h5.xml_error.results >> ..\..\temptest\tcompound2.h5.xml.results
del ..\..\temptest\tcompound2.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tdatareg.h5 > ..\..\temptest\tdatareg.h5.xml.results 2>..\..\temptest\tdatareg.h5.xml_error.results
more ..\..\temptest\tdatareg.h5.xml_error.results >> ..\..\temptest\tdatareg.h5.xml.results
del ..\..\temptest\tdatareg.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tdset.h5 > ..\..\temptest\tdset.h5.xml.results 2>..\..\temptest\tdset.h5.xml_error.results
more ..\..\temptest\tdset.h5.xml_error.results >> ..\..\temptest\tdset.h5.xml.results
del ..\..\temptest\tdset.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tdset2.h5 > ..\..\temptest\tdset2.h5.xml.results 2>..\..\temptest\tdset2.h5.xml_error.results
more ..\..\temptest\tdset2.h5.xml_error.results >> ..\..\temptest\tdset2.h5.xml.results
del ..\..\temptest\tdset2.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tempty.h5 > ..\..\temptest\tempty.h5.xml.results 2>..\..\temptest\tempty.h5.xm_error.results
more ..\..\temptest\tempty.h5.xm_error.results >> ..\..\temptest\tempty.h5.xm.results
del ..\..\temptest\tempty.h5.xm_error.results

h5dump%2 --xml ..\..\testfiles\tenum.h5 > ..\..\temptest\tenum.h5.xml.results 2>..\..\temptest\tenum.h5.xml_error.results
more ..\..\temptest\tenum.h5.xml_error.results >> ..\..\temptest\tenum.h5.xml.results
del ..\..\temptest\tenum.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tgroup.h5 > ..\..\temptest\tgroup.h5.xml.results 2>..\..\temptest\tgroup.h5.xml_error.results
more ..\..\temptest\tgroup.h5.xml_error.results >> ..\..\temptest\tgroup.h5.xml.results
del ..\..\temptest\tgroup.h5.xml_error.results

REM h5dump%2 --xml ..\..\testfiles\thlink.h5 > ..\..\temptest\thlink.h5.xml.results 2>..\..\temptest\thlink.h5.xml_error.results
REM more ..\..\temptest\thlink.h5.xml_error.results >> ..\..\temptest\thlink.h5.xml.results
REM del ..\..\temptest\thlink.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tloop.h5 > ..\..\temptest\tloop.h5.xml.results 2>..\..\temptest\tloop.h5.xml_error.results
more ..\..\temptest\tloop.h5.xml_error.results >> ..\..\temptest\tloop.h5.xml.results
del ..\..\temptest\tloop.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tloop2.h5 > ..\..\temptest\tloop2.h5.xml.results 2>..\..\temptest\tloop2.h5.xml_error.results
more ..\..\temptest\tloop2.h5.xml_error.results >> ..\..\temptest\tloop2.h5.xml.results
del ..\..\temptest\tloop2.h5.xml_error.results

REM h5dump%2 --xml ..\..\testfiles\tmany.h5 > ..\..\temptest\tmany.h5.xml.results 2>..\..\temptest\tmany.h5.xml_error.results
REM more ..\..\temptest\tmany.h5.xml_error.results >> ..\..\temptest\tmany.h5.xml.results
REM del ..\..\temptest\tmany.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tname-amp.h5 > ..\..\temptest\tname-amp.h5.xml.results 2>..\..\temptest\tname-amp.h5.xml_error.results
more ..\..\temptest\tname-amp.h5.xml_error.results >> ..\..\temptest\tname-amp.h5.xml.results
del ..\..\temptest\tname-amp.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tname-apos.h5 > ..\..\temptest\tname-apos.h5.xml.results 2>..\..\temptest\tname-apos.h5.xml_error.results
more ..\..\temptest\tname-apos.h5.xml_error.results >> ..\..\temptest\tname-apos.h5.xml.results
del ..\..\temptest\tname-apos.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tname-gt.h5 > ..\..\temptest\tname-gt.h5.xml.results 2>..\..\temptest\tname-gt.h5.xml_error.results
more ..\..\temptest\tname-gt.h5.xml_error.results >> ..\..\temptest\tname-gt.h5.xml.results
del ..\..\temptest\tname-gt.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tname-lt.h5 > ..\..\temptest\tname-lt.h5.xml.results 2>..\..\temptest\tname-lt.h5.xml_error.results
more ..\..\temptest\tname-lt.h5.xml_error.results >> ..\..\temptest\tname-lt.h5.xml.results
del ..\..\temptest\tname-lt.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tname-quot.h5 > ..\..\temptest\tname-quot.h5.xml.results 2>..\..\temptest\tname-quot.h5.xml_error.results
more ..\..\temptest\tname-quot.h5.xml_error.results >> ..\..\temptest\tname-quot.h5.xml.results
del ..\..\temptest\tname-quot.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tname-sp.h5 > ..\..\temptest\tname-sp.h5.xml.results 2>..\..\temptest\tname-sp.h5.xml_error.results
more ..\..\temptest\tname-sp.h5.xml_error.results >> ..\..\temptest\tname-sp.h5.xml.results
del ..\..\temptest\tname-sp.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tnestedcomp.h5 > ..\..\temptest\tnestedcomp.h5.xml.results 2>..\..\temptest\tnestedcomp.h5.xml_error.results
more ..\..\temptest\tnestedcomp.h5.xml_error.results >> ..\..\temptest\tnestedcomp.h5.xml.results
del ..\..\temptest\tnestedcomp.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tcompound_complex.h5 > ..\..\temptest\tcompound_complex.h5.xml.results 2>..\..\temptest\tcompound_complex.h5.xml_error.results
more ..\..\temptest\tcompound_complex.h5.xml_error.results >> ..\..\temptest\tcompound_complex.h5.xml.results
del ..\..\temptest\tcompound_complex.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tnodata.h5 > ..\..\temptest\tnodata.h5.xml.results 2>..\..\temptest\tnodata.h5.xml_error.results
more ..\..\temptest\tnodata.h5.xml_error.results >> ..\..\temptest\tnodata.h5.xml.results
del ..\..\temptest\tnodata.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tobjref.h5 > ..\..\temptest\tobjref.h5.xml.results 2>..\..\temptest\tobjref.h5.xml_error.results
more ..\..\temptest\tobjref.h5.xml_error.results >> ..\..\temptest\tobjref.h5.xml.results
del ..\..\temptest\tobjref.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\topaque.h5 > ..\..\temptest\topaque.h5.xml.results 2>..\..\temptest\topaque.h5.xml_error.results
more ..\..\temptest\topaque.h5.xml_error.results >> ..\..\temptest\topaque.h5.xml.results
del ..\..\temptest\topaque.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tref-escapes-at.h5 > ..\..\temptest\tref-escapes-at.h5.xml.results 2>..\..\temptest\tref-escapes-at.h5.xml_error.results
more ..\..\temptest\tref-escapes-at.h5.xml_error.results >> ..\..\temptest\tref-escapes-at.h5.xml.results
del ..\..\temptest\tref-escapes-at.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tref-escapes.h5 > ..\..\temptest\tref-escapes.h5.xml.results 2>..\..\temptest\tref-escapes.h5.xml_error.results
more ..\..\temptest\tref-escapes.h5.xml_error.results >> ..\..\temptest\tref-escapes.h5.xml.results
del ..\..\temptest\tref-escapes.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tref.h5 > ..\..\temptest\tref.h5.xml.results 2>..\..\temptest\tref.h5.xml_error.results
more ..\..\temptest\tref.h5.xml_error.results >> ..\..\temptest\tref.h5.xml.results
del ..\..\temptest\tref.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tsaf.h5 > ..\..\temptest\tsaf.h5.xml.results 2>..\..\temptest\tsaf.h5.xml_error.results
more ..\..\temptest\tsaf.h5.xml_error.results >> ..\..\temptest\tsaf.h5.xml.results
del ..\..\temptest\tsaf.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tslink.h5 > ..\..\temptest\tslink.h5.xml.results 2>..\..\temptest\tslink.h5.xml_error.results
more ..\..\temptest\tslink.h5.xml_error.results >> ..\..\temptest\tslink.h5.xml.results
del ..\..\temptest\tslink.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tstr.h5 > ..\..\temptest\tstr.h5.xml.results 2>..\..\temptest\tstr.h5.xml_error.results
more ..\..\temptest\tstr.h5.xml_error.results >> ..\..\temptest\tstr.h5.xml.results
del ..\..\temptest\tstr.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tstr2.h5 > ..\..\temptest\tstr2.h5.xml.results 2>..\..\temptest\tstr2.h5.xml_error.results
more ..\..\temptest\tstr2.h5.xml_error.results >> ..\..\temptest\tstr2.h5.xml.results
del ..\..\temptest\tstr2.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tstring-at.h5 > ..\..\temptest\tstring-at.h5.xml.results 2>..\..\temptest\tstring-at.h5.xml_error.results
more ..\..\temptest\tstring-at.h5.xml_error.results >> ..\..\temptest\tstring-at.h5.xml.results
del ..\..\temptest\tstring-at.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tstring.h5 > ..\..\temptest\tstring.h5.xml.results 2>..\..\temptest\tstring.h5.xml_error.results
more ..\..\temptest\tstring.h5.xml_error.results >> ..\..\temptest\tstring.h5.xml.results
del ..\..\temptest\tstring.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tvldtypes1.h5 > ..\..\temptest\tvldtypes1.h5.xml.results 2>..\..\temptest\tvldtypes1.h5.xml_error.results
more ..\..\temptest\tvldtypes1.h5.xml_error.results >> ..\..\temptest\tvldtypes1.h5.xml.results
del ..\..\temptest\tvldtypes1.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tvldtypes2.h5 > ..\..\temptest\tvldtypes2.h5.xml.results 2>..\..\temptest\tvldtypes2.h5.xml_error.results
more ..\..\temptest\tvldtypes2.h5.xml_error.results >> ..\..\temptest\tvldtypes2.h5.xml.results
del ..\..\temptest\tvldtypes2.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tvldtypes3.h5 > ..\..\temptest\tvldtypes3.h5.xml.results 2>..\..\temptest\tvldtypes3.h5.xml_error.results
more ..\..\temptest\tvldtypes3.h5.xml_error.results >> ..\..\temptest\tvldtypes3.h5.xml.results
del ..\..\temptest\tvldtypes3.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tvldtypes4.h5 > ..\..\temptest\tvldtypes4.h5.xml.results 2>..\..\temptest\tvldtypes4.h5.xml_error.results
more ..\..\temptest\tvldtypes4.h5.xml_error.results >> ..\..\temptest\tvldtypes4.h5.xml.results
del ..\..\temptest\tvldtypes4.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tvldtypes5.h5 > ..\..\temptest\tvldtypes5.h5.xml.results 2>..\..\temptest\tvldtypes5.h5.xml_error.results
more ..\..\temptest\tvldtypes5.h5.xml_error.results >> ..\..\temptest\tvldtypes5.h5.xml.results
del ..\..\temptest\tvldtypes5.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tvlstr.h5 > ..\..\temptest\tvlstr.h5.xml.results 2>..\..\temptest\tvlstr.h5.xml_error.results
more ..\..\temptest\tvlstr.h5.xml_error.results >> ..\..\temptest\tvlstr.h5.xml.results
del ..\..\temptest\tvlstr.h5.xml_error.results

h5dump%2 --xml ..\..\testfiles\tnamed_dtype_attr.h5 > ..\..\temptest\tnamed_dtype_attr.h5.xml.results 2>..\..\temptest\tnamed_dtype_attr.h5.xml_error.results
more ..\..\temptest\tnamed_dtype_attr.h5.xml_error.results >> ..\..\temptest\tnamed_dtype_attr.h5.xml.results
del ..\..\temptest\tnamed_dtype_attr.h5.xml_error.results

h5dump%2 --xml --use-dtd ..\..\testfiles\tempty.h5 > ..\..\temptest\tempty-dtd.h5.xml.results 2>..\..\temptest\tempty-dtd.h5.xml_error.results
more ..\..\temptest\tempty-dtd.h5.xml_error.results >> ..\..\temptest\tempty-dtd.h5.xml.results
del ..\..\temptest\tempty-dtd.h5.xml_error.results

h5dump%2 --xml -u ..\..\testfiles\tempty.h5 > ..\..\temptest\tempty-dtd-2.h5.xml.results 2>..\..\temptest\tempty-dtd-2.h5.xml_error.results
more ..\..\temptest\tempty-dtd-2.h5.xml_error.results >> ..\..\temptest\tempty-dtd-2.h5.xml.results
del ..\..\temptest\tempty-dtd-2.h5.xml_error.results

h5dump%2 --xml -X ":" ..\..\testfiles\tempty.h5 > ..\..\temptest\tempty-nons.h5.xml.results 2>..\..\temptest\tempty-nons.h5.xml_error.results
more ..\..\temptest\tempty-nons.h5.xml_error.results >> ..\..\temptest\tempty-nons.h5.xml.results
del ..\..\temptest\tempty-nons.h5.xml_error.results

h5dump%2 --xml --xml-ns=":" ..\..\testfiles\tempty.h5 > ..\..\temptest\tempty-nons-2.h5.xml.results 2>..\..\temptest\tempty-nons-2.h5.xml_error.results
more ..\..\temptest\tempty-nons-2.h5.xml_error.results >> ..\..\temptest\tempty-nons-2.h5.xml.results
del ..\..\temptest\tempty-nons-2.h5.xml_error.results

h5dump%2 --xml -X "thing:" ..\..\testfiles\tempty.h5 > ..\..\temptest\tempty-ns.h5.xml.results 2>..\..\temptest\tempty-ns.h5.xml_error.results
more ..\..\temptest\tempty-ns.h5.xml_error.results >> ..\..\temptest\tempty-ns.h5.xml.results
del ..\..\temptest\tempty-ns.h5.xml_error.results

h5dump%2 --xml --xml-ns="thing:" ..\..\testfiles\tempty.h5 > ..\..\temptest\tempty-ns-2.h5.xml.results 2>..\..\temptest\tempty-ns-2.h5.xml_error.results
more ..\..\temptest\tempty-ns-2.h5.xml_error.results >> ..\..\temptest\tempty-ns-2.h5.xml.results
del ..\..\temptest\tempty-ns-2.h5.xml_error.results

h5dump%2 --xml --xml-ns=":" --xml-dtd="http://somewhere.net" ..\..\testfiles\tempty.h5 > ..\..\temptest\tempty-nons-uri.h5.xml.results 2>..\..\temptest\tempty-nons-uri.h5.xml_error.results
more ..\..\temptest\tempty-nons-uri.h5.xml_error.results >> ..\..\temptest\tempty-nons-uri.h5.xml.results
del ..\..\temptest\tempty-nons-uri.h5.xml_error.results

h5dump%2 --xml --use-dtd --xml-dtd="http://somewhere.net" ..\..\testfiles\tempty.h5 > ..\..\temptest\tempty-dtd-uri.h5.xml.results 2>..\..\temptest\tempty-dtd-uri.h5.xml_error.results
more ..\..\temptest\tempty-dtd-uri.h5.xml_error.results >> ..\..\temptest\tempty-dtd-uri.h5.xml.results
del ..\..\temptest\tempty-dtd-uri.h5.xml_error.results

h5dump%2 --xml -A ..\..\testfiles\tall.h5 > ..\..\temptest\tall-2A.h5.xml.results 2>..\..\temptest\tall-2A.h5.xml_error.results
more ..\..\temptest\tall-2A.h5.xml_error.results >> ..\..\temptest\tall-2A.h5.xml.results
del ..\..\temptest\tall-2A.h5.xml_error.results

cd ..\..\temptest
mkdir fctemp

REM ##############################################################################
REM ##############################################################################
REM ###			                    T H E   T E S T S                             ###
REM ##############################################################################
REM ##############################################################################

type nul > ..\dumptest%2_%1.txt
echo ========================================== >> ..\dumptest%2_%1.txt
echo Testing dumptest.bat >> ..\dumptest%2_%1.txt
echo ========================================== >> ..\dumptest%2_%1.txt

REM test for displaying groups
call ..\deleteline tgroup-1.results 1 
cd ..\testfiles
call ..\deleteline tgroup-1.ddl 4
cd ..\temptest\fctemp
fc tgroup-1.ddl tgroup-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tgroup.h5                                               PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tgroup.h5                                               FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for displaying the selected groups
call ..\deleteline tgroup-2.results 1 
cd ..\testfiles
call ..\deleteline tgroup-2.ddl 4
cd ..\temptest\fctemp
fc tgroup-2.ddl tgroup-2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -g /g2 -g / -g /y tgroup.h5                             PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -g /g2 -g / -g /y tgroup.h5                             FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for displaying simple space datasets
call ..\deleteline tdset-1.results 1 
cd ..\testfiles
call ..\deleteline tdset-1.ddl 4
cd ..\temptest\fctemp
fc tdset-1.ddl tdset-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tdset.h5                                                PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tdset.h5                                                FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for displaying selected datasets
call ..\deleteline tdset-2.results 1 
cd ..\testfiles
call ..\deleteline tdset-2.ddl 4
cd ..\temptest\fctemp
fc tdset-2.ddl tdset-2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -H -d dset1 -d /dset2 --dataset=dset3                   PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -H -d dset1 -d /dset2 --dataset=dset3                   FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for displaying attributes
call ..\deleteline tattr-1.results 1 
cd ..\testfiles
call ..\deleteline tattr-1.ddl 4
cd ..\temptest\fctemp
fc tattr-1.ddl tattr-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tattr.h5                                                PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tattr.h5                                                FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for displaying the selected attributes of string type and scalar space
call ..\deleteline tattr-2.results 1 
cd ..\testfiles
call ..\deleteline tattr-2.ddl 4
cd ..\temptest\fctemp
fc tattr-2.ddl tattr-2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -a /attr1 --attribute /attr4 --attribute=/attr5 tattr.  PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -a /attr1 --attribute /attr4 --attribute=/attr5 tattr.  FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for header and error messages
call ..\deleteline tattr-3.results 1 
cd ..\testfiles
call ..\deleteline tattr-3.ddl 4
cd ..\temptest\fctemp
fc tattr-3.ddl tattr-3.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --header -a /attr2 --attribute=/attr tattr.h5           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --header -a /attr2 --attribute=/attr tattr.h5           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for displaying shared datatype in attribute
call ..\deleteline tnamed_dtype_attr.results 1 
cd ..\testfiles
call ..\deleteline tnamed_dtype_attr.ddl 4
cd ..\temptest\fctemp
fc tnamed_dtype_attr.ddl tnamed_dtype_attr.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -A tnamed_dtype_attr.h5 tnamed_dtype_attr.results       PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -A tnamed_dtype_attr.h5 tnamed_dtype_attr.results       FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for displaying soft links
call ..\deleteline tslink-1.results 1 
cd ..\testfiles
call ..\deleteline tslink-1.ddl 4
cd ..\temptest\fctemp
fc tslink-1.ddl tslink-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tslink.h5                                               PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tslink.h5                                               FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for displaying the selected link
call ..\deleteline tslink-2.results 1 
cd ..\testfiles
call ..\deleteline tslink-2.ddl 4
cd ..\temptest\fctemp
fc tslink-2.ddl tslink-2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -l slink2 tslink.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -l slink2 tslink.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM tests for hard links
call ..\deleteline thlink-1.results 1 
cd ..\testfiles
call ..\deleteline thlink-1.ddl 4
cd ..\temptest\fctemp
fc thlink-1.ddl thlink-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump thlink.h5                                               PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump thlink.h5                                               FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline thlink-2.results 1 
cd ..\testfiles
call ..\deleteline thlink-2.ddl 4
cd ..\temptest\fctemp
fc thlink-2.ddl thlink-2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3  PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3  FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline thlink-3.results 1 
cd ..\testfiles
call ..\deleteline thlink-3.ddl 4
cd ..\temptest\fctemp
fc thlink-3.ddl thlink-3.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1  PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1  FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline thlink-4.results 1 
cd ..\testfiles
call ..\deleteline thlink-4.ddl 4
cd ..\temptest\fctemp
fc thlink-4.ddl thlink-4.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -g /g1 thlink.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -g /g1 thlink.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline thlink-5.results 1 
cd ..\testfiles
call ..\deleteline thlink-5.ddl 4
cd ..\temptest\fctemp
fc thlink-5.ddl thlink-5.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -d /dset1 -g /g2 -d /g1/dset2 thlink.h5                 PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -d /dset1 -g /g2 -d /g1/dset2 thlink.h5                 FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM tests for compound data types
call ..\deleteline tcomp-1.results 1 
cd ..\testfiles
call ..\deleteline tcomp-1.ddl 4
cd ..\temptest\fctemp
fc tcomp-1.ddl tcomp-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tcompound.h5                                            PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tcompound.h5                                            FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for named data types
call ..\deleteline tcomp-2.results 1 
cd ..\testfiles
call ..\deleteline tcomp-2.ddl 4
cd ..\temptest\fctemp
fc tcomp-2.ddl tcomp-2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -t /type1 --datatype /type2 --datatype=/group1/type3    PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -t /type1 --datatype /type2 --datatype=/group1/type3    FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for unamed type 
call ..\deleteline tcomp-3.results 1 
cd ..\testfiles
call ..\deleteline tcomp-3.ddl 4
cd ..\temptest\fctemp
fc tcomp-3.ddl tcomp-3.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -t /#6632:0 -g /group2 tcompound.h5                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -t /#6632:0 -g /group2 tcompound.h5                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test complicated compound datatype
call ..\deleteline tcompound_complex.results 1 
cd ..\testfiles
call ..\deleteline tcomp-4.ddl 4
cd ..\temptest\fctemp
fc tcomp-4.ddl tcompound_complex.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tcompound_complex.h5                                    PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tcompound_complex.h5                                    FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for the nested compound type
call ..\deleteline tnestcomp-1.results 1 
cd ..\testfiles
call ..\deleteline tnestcomp-1.ddl 4
cd ..\temptest\fctemp
fc tnestcomp-1.ddl tnestcomp-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tnestedcomp.h5                                          PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tnestedcomp.h5                                          FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for options
call ..\deleteline tall-1.results 1 
cd ..\testfiles
call ..\deleteline tall-1.ddl 4
cd ..\temptest\fctemp
fc tall-1.ddl tall-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tall.h5                                                 PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tall.h5                                                 FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tall-2.results 1 
cd ..\testfiles
call ..\deleteline tall-2.ddl 4
cd ..\temptest\fctemp
fc tall-2.ddl tall-2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --header -g /g1/g1.1 -a attr2 tall.h5                   PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --header -g /g1/g1.1 -a attr2 tall.h5                   FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tall-3.results 1 
cd ..\testfiles
call ..\deleteline tall-3.ddl 4
cd ..\temptest\fctemp
fc tall-3.ddl tall-3.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5         PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5         FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for loop detection
call ..\deleteline tloop-1.results 1 
cd ..\testfiles
call ..\deleteline tloop-1.ddl 4
cd ..\temptest\fctemp
fc tloop-1.ddl tloop-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tloop.h5                                                PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tloop.h5                                                FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for string 
call ..\deleteline tstr-1.results 1 
cd ..\testfiles
call ..\deleteline tstr-1.ddl 4
cd ..\temptest\fctemp
fc tstr-1.ddl tstr-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tstr.h5                                                 PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tstr.h5                                                 FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tstr-2.results 1 
cd ..\testfiles
call ..\deleteline tstr-2.ddl 4
cd ..\temptest\fctemp
fc tstr-2.ddl tstr-2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tstr2.h5                                                PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tstr2.h5                                                FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for file created by Lib SAF team
call ..\deleteline tsaf.results 1 
cd ..\testfiles
call ..\deleteline tsaf.ddl 4
cd ..\temptest\fctemp
fc tsaf.ddl tsaf.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tsaf.h5                                                 PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tsaf.h5                                                 FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for file with variable length data
call ..\deleteline tvldtypes1.results 1 
cd ..\testfiles
call ..\deleteline tvldtypes1.ddl 4
cd ..\temptest\fctemp
fc tvldtypes1.ddl tvldtypes1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tvldtypes1.h5                                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tvldtypes1.h5                                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tvldtypes2.results 1 
cd ..\testfiles
call ..\deleteline tvldtypes2.ddl 4
cd ..\temptest\fctemp
fc tvldtypes2.ddl tvldtypes2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tvldtypes2.h5                                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tvldtypes2.h5                                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tvldtypes3.results 1 
cd ..\testfiles
call ..\deleteline tvldtypes3.ddl 4
cd ..\temptest\fctemp
fc tvldtypes3.ddl tvldtypes3.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tvldtypes3.h5                                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tvldtypes3.h5                                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tvldtypes4.results 1 
cd ..\testfiles
call ..\deleteline tvldtypes4.ddl 4
cd ..\temptest\fctemp
fc tvldtypes4.ddl tvldtypes4.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tvldtypes4.h5                                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tvldtypes4.h5                                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tvldtypes5.results 1 
cd ..\testfiles
call ..\deleteline tvldtypes5.ddl 4
cd ..\temptest\fctemp
fc tvldtypes5.ddl tvldtypes5.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tvldtypes5.h5                                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tvldtypes5.h5                                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for file with variable length string data
call ..\deleteline tvlstr.results 1 
cd ..\testfiles
call ..\deleteline tvlstr.ddl 4
cd ..\temptest\fctemp
fc tvlstr.ddl tvlstr.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tvlstr.h5                                               PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tvlstr.h5                                               FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for files with array data
call ..\deleteline tarray1.results 1 
cd ..\testfiles
call ..\deleteline tarray1.ddl 4
cd ..\temptest\fctemp
fc tarray1.ddl tarray1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tarray1.h5                                              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tarray1.h5                                              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tarray2.results 1 
cd ..\testfiles
call ..\deleteline tarray2.ddl 4
cd ..\temptest\fctemp
fc tarray2.ddl tarray2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tarray2.h5                                              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tarray2.h5                                              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tarray3.results 1 
cd ..\testfiles
call ..\deleteline tarray3.ddl 4
cd ..\temptest\fctemp
fc tarray3.ddl tarray3.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tarray3.h5                                              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tarray3.h5                                              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tarray4.results 1 
cd ..\testfiles
call ..\deleteline tarray4.ddl 4
cd ..\temptest\fctemp
fc tarray4.ddl tarray4.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tarray4.h5                                              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tarray4.h5                                              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tarray5.results 1 
cd ..\testfiles
call ..\deleteline tarray5.ddl 4
cd ..\temptest\fctemp
fc tarray5.ddl tarray5.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tarray5.h5                                              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tarray5.h5                                              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tarray6.results 1 
cd ..\testfiles
call ..\deleteline tarray6.ddl 4
cd ..\temptest\fctemp
fc tarray6.ddl tarray6.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tarray6.h5                                              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tarray6.h5                                              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tarray7.results 1 
cd ..\testfiles
call ..\deleteline tarray7.ddl 4
cd ..\temptest\fctemp
fc tarray7.ddl tarray7.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tarray7.h5                                              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tarray7.h5                                              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for files with empty data
call ..\deleteline tempty.results 1 
cd ..\testfiles
call ..\deleteline tempty.ddl 4
cd ..\temptest\fctemp
fc tempty.ddl tempty.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tempty.h5                                               PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tempty.h5                                               FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for files with groups that have comments
call ..\deleteline tgrp_comments.results 1 
cd ..\testfiles
call ..\deleteline tgrp_comments.ddl 4
cd ..\temptest\fctemp
fc tgrp_comments.ddl tgrp_comments.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tgrp_comments.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tgrp_comments.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test the --filedriver flag
call ..\deleteline tsplit_file.results 1 
cd ..\testfiles
call ..\deleteline tsplit_file.ddl 4
cd ..\temptest\fctemp
fc tsplit_file.ddl tsplit_file.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --filedriver=split tsplit_file                          PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --filedriver=split tsplit_file                          FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tfamily.ddl --filedriver=family tfamily%05d.h5
REM call ..\deleteline tfamily.results 1 
REM cd ..\testfiles
REM call ..\deleteline tfamily.ddl 4
REM cd ..\temptest\fctemp
REM fc tfamily.ddl tfamily.results >temp.txt
REM cd ..

call ..\deleteline tmulti.results 1 
cd ..\testfiles
call ..\deleteline tmulti.ddl 4
cd ..\temptest\fctemp
fc tmulti.ddl tmulti.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tmulti                                                  PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tmulti                                                  FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for files with group names which reach > 1024 bytes in size
call ..\deleteline tlarge_objname.results 1 
cd ..\testfiles
call ..\deleteline tlarge_objname.ddl 4
cd ..\temptest\fctemp
fc tlarge_objname.ddl tlarge_objname.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -w157 tlarge_objname.h5                                 PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -w157 tlarge_objname.h5                                 FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test '-A' to suppress data but print attr's
call ..\deleteline tall-2A.results 1 
cd ..\testfiles
call ..\deleteline tall-2A.ddl 4
cd ..\temptest\fctemp
fc tall-2A.ddl tall-2A.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -A tall.h5                                              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -A tall.h5                                              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test Subsetting
call ..\deleteline tall-4s.results 1 
cd ..\testfiles
call ..\deleteline tall-4s.ddl 4
cd ..\temptest\fctemp
fc tall-4s.ddl tall-4s.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3   PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3   FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tall-5s.results 1 
cd ..\testfiles
call ..\deleteline tall-5s.ddl 4
cd ..\temptest\fctemp
fc tall-5s.ddl tall-5s.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5                PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5                FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tdset-3s.results 1 
cd ..\testfiles
call ..\deleteline tdset-3s.ddl 4
cd ..\temptest\fctemp
fc tdset-3s.ddl tdset-3s.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -d "/dset1[1,1;;;]" tdset.h5                            PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -d "/dset1[1,1;;;]" tdset.h5                            FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tdset2-1s.results 1 
cd ..\testfiles
call ..\deleteline tdset2-1s.ddl 4
cd ..\temptest\fctemp
fc tdset2-1s.ddl tdset2-1s.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -d "/dset1[;3,2;4,4;1,4]" tdset2.h5                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -d "/dset1[;3,2;4,4;1,4]" tdset2.h5                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test printing characters in ASCII instead of decimal
call ..\deleteline tchar1.results 1 
cd ..\testfiles
call ..\deleteline tchar1.ddl 4
cd ..\temptest\fctemp
fc tchar1.ddl tchar1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -r tchar.h5                                             PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -r tchar.h5                                             FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test failure handling
REM Missing file name
call ..\deleteline tnofilename.results 0 
cd ..\testfiles
call ..\deleteline tnofilename.ddl 3
cd ..\temptest\fctemp
fc tnofilename.ddl tnofilename.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump                                                         PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump                                                         FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM rev. 2004

REM tests for super block
REM TOOLTEST tboot1.ddl -H -B -d dset tfcontents1.h5
call ..\deleteline tboot1.results 1 
cd ..\testfiles
call ..\deleteline tboot1.ddl 4
cd ..\temptest\fctemp
fc tboot1.ddl tboot1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -H -B -d dset tfcontents1.h5                            PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -H -B -d dset tfcontents1.h5                            FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tboot2.ddl -B tfcontents2.h5
call ..\deleteline tboot2.results 1 
cd ..\testfiles
call ..\deleteline tboot2.ddl 4
cd ..\temptest\fctemp
fc tboot2.ddl tboot2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -B tfcontents2.h5                                       PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -B tfcontents2.h5                                       FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test -p with a non existing dataset
REM TOOLTEST tperror.ddl -p -d bogus tfcontents1.h5
call ..\deleteline tperror.results 1 
cd ..\testfiles
call ..\deleteline tperror.ddl 4
cd ..\temptest\fctemp
fc tperror.ddl tperror.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -p -d bogus tfcontents1.h5                              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -p -d bogus tfcontents1.h5                              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test for file contents
REM TOOLTEST tcontents.ddl -n tfcontents1.h5
call ..\deleteline tcontents.results 1 
cd ..\testfiles
call ..\deleteline tcontents.ddl 4
cd ..\temptest\fctemp
fc tcontents.ddl tcontents.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -n tfcontents1.h5                                       PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -n tfcontents1.h5                                       FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM tests for storage layout
REM compact
REM TOOLTEST tcompact.ddl -H -p -d compact tfilters.h5
call ..\deleteline tcompact.results 1 
cd ..\testfiles
call ..\deleteline tcompact.ddl 4
cd ..\temptest\fctemp
fc tcompact.ddl tcompact.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -H -p -d compact tfilters.h5                            PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -H -p -d compact tfilters.h5                            FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM contiguous
REM TOOLTEST tcontiguos.ddl -H -p -d contiguous tfilters.h5
call ..\deleteline tcontiguos.results 1 
cd ..\testfiles
call ..\deleteline tcontiguos.ddl 4
cd ..\temptest\fctemp
fc tcontiguos.ddl tcontiguos.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -H -p -d contiguous tfilters.h5                         PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -H -p -d contiguous tfilters.h5                         FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM chunked
REM TOOLTEST tchunked.ddl -H -p -d chunked tfilters.h5
call ..\deleteline tchunked.results 1 
cd ..\testfiles
call ..\deleteline tchunked.ddl 4
cd ..\temptest\fctemp
fc tchunked.ddl tchunked.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -H -p -d chunked tfilters.h5                            PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -H -p -d chunked tfilters.h5                            FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM external 
REM TOOLTEST texternal.ddl -H -p -d external tfilters.h5
call ..\deleteline texternal.results 1 
cd ..\testfiles
call ..\deleteline texternal.ddl 4
cd ..\temptest\fctemp
fc texternal.ddl texternal.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -H -p -d external tfilters.h5                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -H -p -d external tfilters.h5                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM fill values
REM TOOLTEST tfill.ddl -p tfvalues.h5
call ..\deleteline tfill.results 1 
cd ..\testfiles
call ..\deleteline tfill.ddl 4
cd ..\temptest\fctemp
fc tfill.ddl tfill.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -p tfvalues.h5                                          PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -p tfvalues.h5                                          FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM several datatype, with references , print path
REM TOOLTEST treference.ddl  tattr2.h5
call ..\deleteline treference.results 1 
cd ..\testfiles
call ..\deleteline treference.ddl 4
cd ..\temptest\fctemp
fc treference.ddl treference.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tattr2.h5                                               PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tattr2.h5                                               FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM escape/not escape non printable characters
REM TOOLTEST tstringe.ddl -e tstr3.h5
call ..\deleteline tstringe.results 1 
cd ..\testfiles
call ..\deleteline tstringe.ddl 4
cd ..\temptest\fctemp
fc tstringe.ddl tstringe.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -e tstr3.h5                                             PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -e tstr3.h5                                             FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tstring.ddl tstr3.h5
call ..\deleteline tstring.results 1 
cd ..\testfiles
call ..\deleteline tstring.ddl 4
cd ..\temptest\fctemp
fc tstring.ddl tstring.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tstr3.h5                                                PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tstr3.h5                                                FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM char data as ASCII with non escape
REM TOOLTEST tstring2.ddl -r -d str4 tstr3.h5
call ..\deleteline tstring2.results 1 
cd ..\testfiles
call ..\deleteline tstring2.ddl 4
cd ..\temptest\fctemp
fc tstring2.ddl tstring2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -r -d str4 tstr3.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -r -d str4 tstr3.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM array indices print/not print
REM TOOLTEST tindicesyes.ddl taindices.h5
call ..\deleteline tindicesyes.results 1 
cd ..\testfiles
call ..\deleteline tindicesyes.ddl 4
cd ..\temptest\fctemp
fc tindicesyes.ddl tindicesyes.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump taindices.h5                                            PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump taindices.h5                                            FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tindicesno.ddl -y taindices.h5
call ..\deleteline tindicesno.results 1 
cd ..\testfiles
call ..\deleteline tindicesno.ddl 4
cd ..\temptest\fctemp
fc tindicesno.ddl tindicesno.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -y taindices.h5                                         PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -y taindices.h5                                         FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM tests for filters
REM SZIP
REM TOOLTEST tszip.ddl -H -p -d szip tfilters.h5
if "%HDF5_EXT_SZIP%"=="" GOTO NEXTSTEP4
if %HDF5_EXT_SZIP%==szlib.lib (
      call ..\deleteline tszip.results 1 
      cd ..\testfiles
      call ..\deleteline tszip.ddl 4
      cd ..\temptest\fctemp
      fc tszip.ddl tszip.results >temp.txt
      if %ERRORLEVEL%==0 (
         echo Testing h5dump -H -p -d szip tfilters.h5                               PASSED >> ..\..\dumptest%2_%1.txt
      ) else (
         echo Testing h5dump -H -p -d szip tfilters.h5                               FAILED >> ..\..\dumptest%2_%1.txt
         more temp.txt >> ..\..\dumptest%2_%1.txt
      )
      del temp.txt
      cd ..
      GOTO NEXTSTEP5
) else (
   echo Testing h5dump -H -p -d szip tfilters.h5                               -SKIP- >> ..\..\dumptest%2_%1.txt
   GOTO NEXTSTEP5
)

:NEXTSTEP4
echo Testing h5dump -H -p -d szip tfilters.h5                               -SKIP- >> ..\dumptest%2_%1.txt

:NEXTSTEP5


REM deflate
REM TOOLTEST tdeflate.ddl -H -p -d deflate tfilters.h5
if "%HDF5_EXT_ZLIB%"=="" GOTO NEXTSTEP6
if %HDF5_EXT_ZLIB%==zlib.lib (
   call ..\deleteline tdeflate.results 1 
   cd ..\testfiles
   call ..\deleteline tdeflate.ddl 4
   cd ..\temptest\fctemp
   fc tdeflate.ddl tdeflate.results >temp.txt
   if %ERRORLEVEL%==0 (
      echo Testing h5dump -H -p -d deflate tfilters.h5                            PASSED >> ..\..\dumptest%2_%1.txt
   ) else (
      echo Testing h5dump -H -p -d deflate tfilters.h5                            FAILED >> ..\..\dumptest%2_%1.txt
      more temp.txt >> ..\..\dumptest%2_%1.txt
   )
   del temp.txt
   cd ..
   GOTO NEXTSTEP7
) else (
   echo Testing h5dump -H -p -d deflate tfilters.h5                            -SKIP- >> ..\..\dumptest%2_%1.txt
   GOTO NEXTSTEP7
)

:NEXTSTEP6
echo Testing h5dump -H -p -d deflate tfilters.h5                            -SKIP- >> ..\dumptest%2_%1.txt

:NEXTSTEP7

REM shuffle
REM TOOLTEST tshuffle.ddl -H -p -d shuffle tfilters.h5
call ..\deleteline tshuffle.results 1 
cd ..\testfiles
call ..\deleteline tshuffle.ddl 4
cd ..\temptest\fctemp
fc tshuffle.ddl tshuffle.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -H -p -d shuffle tfilters.h5                            PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -H -p -d shuffle tfilters.h5                            FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM fletcher32
REM TOOLTEST tfletcher32.ddl -H -p -d fletcher32  tfilters.h5
call ..\deleteline tfletcher32.results 1 
cd ..\testfiles
call ..\deleteline tfletcher32.ddl 4
cd ..\temptest\fctemp
fc tfletcher32.ddl tfletcher32.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -H -p -d fletcher32  tfilters.h5                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -H -p -d fletcher32  tfilters.h5                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM all
REM TOOLTEST tallfilters.ddl -H -p -d all  tfilters.h5
if "%HDF5_EXT_ZLIB%"=="" GOTO NEXTSTEP8
if "%HDF5_EXT_SZIP%"=="" GOTO NEXTSTEP8
if %HDF5_EXT_ZLIB%==zlib.lib (
   if %HDF5_EXT_SZIP%==szlib.lib (
      call ..\deleteline tallfilters.results 1 
      cd ..\testfiles
      call ..\deleteline tallfilters.ddl 4
      cd ..\temptest\fctemp
      fc tallfilters.ddl tallfilters.results >temp.txt
      if %ERRORLEVEL%==0 (
         echo Testing h5dump -H -p -d all tfilters.h5                                PASSED >> ..\..\dumptest%2_%1.txt
      ) else (
         echo Testing h5dump -H -p -d all tfilters.h5                                FAILED >> ..\..\dumptest%2_%1.txt
         more temp.txt >> ..\..\dumptest%2_%1.txt
      )
      del temp.txt
      cd ..
      GOTO NEXTSTEP9
   ) else (
      echo Testing h5dump -H -p -d all tfilters.h5                                -SKIP- >> ..\..\dumptest%2_%1.txt
      GOTO NEXTSTEP9
   )
) else (
   echo Testing h5dump -H -p -d all tfilters.h5                                -SKIP- >> ..\..\dumptest%2_%1.txt
   GOTO NEXTSTEP9
)


:NEXTSTEP8
echo Testing h5dump -H -p -d all tfilters.h5                                -SKIP- >> ..\dumptest%2_%1.txt

:NEXTSTEP9

REM user defined
REM TOOLTEST tuserfilter.ddl -H  -p -d myfilter  tfilters.h5
call ..\deleteline tuserfilter.results 1 
cd ..\testfiles
call ..\deleteline tuserfilter.ddl 4
cd ..\temptest\fctemp
fc tuserfilter.ddl tuserfilter.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump -H -p -d myfilter tfilters.h5                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump -H -p -d myfilter tfilters.h5                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd .. 

REM test for displaying dataset and attribute of null space
REM TOOLTEST tnullspace.ddl tnullspace.h5
call ..\deleteline tnullspace.results 1 
cd ..\testfiles
call ..\deleteline tnullspace.ddl 4
cd ..\temptest\fctemp
fc tnullspace.ddl tnullspace.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump tnullspace.h5                                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump tnullspace.h5                                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM test XML
REM TOOLTEST tall.h5.xml --xml tall.h5
call ..\deleteline tall.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tall.h5.xml 3
cd ..\temptest\fctemp
fc tall.h5.xml tall.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tall.h5                                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tall.h5                                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tattr.h5.xml --xml tattr.h5
call ..\deleteline tattr.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tattr.h5.xml 3
cd ..\temptest\fctemp
fc tattr.h5.xml tattr.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tattr.h5                                          PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tattr.h5                                          FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tbitfields.h5.xml --xml tbitfields.h5
call ..\deleteline tbitfields.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tbitfields.h5.xml 3
cd ..\temptest\fctemp
fc tbitfields.h5.xml tbitfields.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tbitfields.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tbitfields.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tcompound.h5.xml --xml tcompound.h5
call ..\deleteline tcompound.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tcompound.h5.xml 3
cd ..\temptest\fctemp
fc tcompound.h5.xml tcompound.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tcompound.h5                                      PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tcompound.h5                                      FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tcompound2.h5.xml --xml tcompound2.h5
call ..\deleteline tcompound2.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tcompound2.h5.xml 3
cd ..\temptest\fctemp
fc tcompound2.h5.xml tcompound2.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tcompound2.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tcompound2.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tdatareg.h5.xml --xml tdatareg.h5
call ..\deleteline tdatareg.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tdatareg.h5.xml 3
cd ..\temptest\fctemp
fc tdatareg.h5.xml tdatareg.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tdatareg.h5                                       PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tdatareg.h5                                       FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tdset.h5.xml --xml tdset.h5
call ..\deleteline tdset.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tdset.h5.xml 3
cd ..\temptest\fctemp
fc tdset.h5.xml tdset.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tdset.h5                                          PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tdset.h5                                          FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tdset2.h5.xml --xml tdset2.h5
call ..\deleteline tdset2.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tdset2.h5.xml 3
cd ..\temptest\fctemp
fc tdset2.h5.xml tdset2.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tdset2.h5                                         PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tdset2.h5                                         FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tenum.h5.xml --xml tenum.h5
call ..\deleteline tenum.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tenum.h5.xml 3
cd ..\temptest\fctemp
fc tenum.h5.xml tenum.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tenum.h5                                          PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tenum.h5                                          FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tgroup.h5.xml --xml tgroup.h5
call ..\deleteline tgroup.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tgroup.h5.xml 3
cd ..\temptest\fctemp
fc tgroup.h5.xml tgroup.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tgroup.h5                                         PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tgroup.h5                                         FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST thlink.h5.xml --xml thlink.h5
REM call ..\deleteline thlink.h5.xml.results 0 
REM cd ..\testfiles
REM call ..\deleteline thlink.h5.xml 3
REM cd ..\temptest\fctemp
REM fc thlink.h5.xml thlink.h5.xml.results >temp.txt
REM if %ERRORLEVEL%==0 (
REM   echo Testing h5dump --xml thlink.h5                                      PASSED >> ..\..\dumptest%2_%1.txt
REM ) else (
REM    echo Testing h5dump --xml thlink.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
REM    more temp.txt >> ..\..\dumptest%2_%1.txt
REM )
REM del temp.txt
REM cd ..
REM TOOLTEST tloop.h5.xml --xml tloop.h5
call ..\deleteline tloop.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tloop.h5.xml 3
cd ..\temptest\fctemp
fc tloop.h5.xml tloop.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tloop.h5                                          PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tloop.h5                                          FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tloop2.h5.xml --xml tloop2.h5
call ..\deleteline tloop2.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tloop2.h5.xml 3
cd ..\temptest\fctemp
fc tloop2.h5.xml tloop2.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tloop2.h5                                         PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tloop2.h5                                         FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tmany.h5.xml --xml tmany.h5
REM call ..\deleteline tmany.h5.xml.results 0 
REM cd ..\testfiles
REM call ..\deleteline tmany.h5.xml 3
REM cd ..\temptest\fctemp
REM fc tmany.h5.xml tmany.h5.xml.results >temp.txt
REM if %ERRORLEVEL%==0 (
REM    echo Testing h5dump --xml tmany.h5                                      PASSED >> ..\..\dumptest%2_%1.txt
REM ) else (
REM    echo Testing h5dump --xml tmany.h5                                      FAILED >> ..\..\dumptest%2_%1.txt
REM    more temp.txt >> ..\..\dumptest%2_%1.txt
REM )
REM del temp.txt
REM cd ..
REM TOOLTEST tnestedcomp.h5.xml --xml tnestedcomp.h5
call ..\deleteline tnestedcomp.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tnestedcomp.h5.xml 3
cd ..\temptest\fctemp
fc tnestedcomp.h5.xml tnestedcomp.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tnestedcomp.h5                                    PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tnestedcomp.h5                                    FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tcompound_complex.h5.xml --xml tcompound_complex.h5
call ..\deleteline tcompound_complex.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tcompound_complex.h5.xml 3
cd ..\temptest\fctemp
fc tcompound_complex.h5.xml tcompound_complex.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tcompound_complex.h5                              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tcompound_complex.h5                              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tobjref.h5.xml --xml tobjref.h5
call ..\deleteline tobjref.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tobjref.h5.xml 3
cd ..\temptest\fctemp
fc tobjref.h5.xml tobjref.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tobjref.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tobjref.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST topaque.h5.xml --xml topaque.h5
call ..\deleteline topaque.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline topaque.h5.xml 3
cd ..\temptest\fctemp
fc topaque.h5.xml topaque.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml topaque.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml topaque.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tslink.h5.xml --xml tslink.h5
call ..\deleteline tslink.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tslink.h5.xml 3
cd ..\temptest\fctemp
fc tslink.h5.xml tslink.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tslink.h5                                         PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tslink.h5                                         FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tstr.h5.xml --xml tstr.h5
call ..\deleteline tstr.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tstr.h5.xml 3
cd ..\temptest\fctemp
fc tstr.h5.xml tstr.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tstr.h5                                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tstr.h5                                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tstr2.h5.xml --xml tstr2.h5
call ..\deleteline tstr2.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tstr2.h5.xml 3
cd ..\temptest\fctemp
fc tstr2.h5.xml tstr2.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tstr2.h5                                          PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tstr2.h5                                          FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tref.h5.xml --xml tref.h5
call ..\deleteline tref.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tref.h5.xml 3
cd ..\temptest\fctemp
fc tref.h5.xml tref.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tref.h5                                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tref.h5                                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tname-amp.h5.xml --xml tname-amp.h5
call ..\deleteline tname-amp.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tname-amp.h5.xml 3
cd ..\temptest\fctemp
fc tname-amp.h5.xml tname-amp.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tname-amp.h5                                      PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tname-amp.h5                                      FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tname-apos.h5.xml --xml tname-apos.h5
call ..\deleteline tname-apos.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tname-apos.h5.xml 3
cd ..\temptest\fctemp
fc tname-apos.h5.xml tname-apos.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tname-apos.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tname-apos.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tname-gt.h5.xml --xml tname-gt.h5
call ..\deleteline tname-gt.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tname-gt.h5.xml 3
cd ..\temptest\fctemp
fc tname-gt.h5.xml tname-gt.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tname-gt.h5                                       PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tname-gt.h5                                       FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tname-lt.h5.xml --xml tname-lt.h5
call ..\deleteline tname-lt.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tname-lt.h5.xml 3
cd ..\temptest\fctemp
fc tname-lt.h5.xml tname-lt.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tname-lt.h5                                       PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tname-lt.h5                                       FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tname-quot.h5.xml --xml tname-quot.h5
call ..\deleteline tname-quot.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tname-quot.h5.xml 3
cd ..\temptest\fctemp
fc tname-quot.h5.xml tname-quot.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tname-quot.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tname-quot.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tname-sp.h5.xml --xml tname-sp.h5
call ..\deleteline tname-sp.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tname-sp.h5.xml 3
cd ..\temptest\fctemp
fc tname-sp.h5.xml tname-sp.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tname-sp.h5                                       PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tname-sp.h5                                       FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tstring.h5.xml --xml tstring.h5
call ..\deleteline tstring.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tstring.h5.xml 3
cd ..\temptest\fctemp
fc tstring.h5.xml tstring.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tstring.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tstring.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tstring-at.h5.xml --xml tstring-at.h5
call ..\deleteline tstring-at.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tstring-at.h5.xml 3
cd ..\temptest\fctemp
fc tstring-at.h5.xml tstring-at.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tstring-at.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tstring-at.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tref-escapes.h5.xml --xml tref-escapes.h5
call ..\deleteline tref-escapes.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tref-escapes.h5.xml 3
cd ..\temptest\fctemp
fc tref-escapes.h5.xml tref-escapes.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tref-escapes.h5                                   PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tref-escapes.h5                                   FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tref-escapes-at.h5.xml --xml tref-escapes-at.h5
call ..\deleteline tref-escapes-at.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tref-escapes-at.h5.xml 3
cd ..\temptest\fctemp
fc tref-escapes-at.h5.xml tref-escapes-at.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tref-escapes-at.h5                                PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tref-escapes-at.h5                                FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tnodata.h5.xml --xml tnodata.h5
call ..\deleteline tnodata.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tnodata.h5.xml 3
cd ..\temptest\fctemp
fc tnodata.h5.xml tnodata.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tnodata.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tnodata.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tarray1.h5.xml --xml tarray1.h5
call ..\deleteline tarray1.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tarray1.h5.xml 3
cd ..\temptest\fctemp
fc tarray1.h5.xml tarray1.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tarray1.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tarray1.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tarray2.h5.xml --xml tarray2.h5
call ..\deleteline tarray2.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tarray2.h5.xml 3
cd ..\temptest\fctemp
fc tarray2.h5.xml tarray2.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tarray2.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tarray2.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tarray3.h5.xml --xml tarray3.h5
call ..\deleteline tarray3.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tarray3.h5.xml 3
cd ..\temptest\fctemp
fc tarray3.h5.xml tarray3.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tarray3.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tarray3.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tarray6.h5.xml --xml tarray6.h5
call ..\deleteline tarray6.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tarray6.h5.xml 3
cd ..\temptest\fctemp
fc tarray6.h5.xml tarray6.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tarray6.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tarray6.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tarray7.h5.xml --xml tarray7.h5
call ..\deleteline tarray7.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tarray7.h5.xml 3
cd ..\temptest\fctemp
fc tarray7.h5.xml tarray7.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tarray7.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tarray7.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tvldtypes1.h5.xml --xml tvldtypes1.h5
call ..\deleteline tvldtypes1.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tvldtypes1.h5.xml 3
cd ..\temptest\fctemp
fc tvldtypes1.h5.xml tvldtypes1.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tvldtypes1.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tvldtypes1.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tvldtypes2.h5.xml --xml tvldtypes2.h5
call ..\deleteline tvldtypes2.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tvldtypes2.h5.xml 3
cd ..\temptest\fctemp
fc tvldtypes2.h5.xml tvldtypes2.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tvldtypes2.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tvldtypes2.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tvldtypes3.h5.xml --xml tvldtypes3.h5
call ..\deleteline tvldtypes3.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tvldtypes3.h5.xml 3
cd ..\temptest\fctemp
fc tvldtypes3.h5.xml tvldtypes3.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tvldtypes3.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tvldtypes3.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tvldtypes4.h5.xml --xml tvldtypes4.h5
call ..\deleteline tvldtypes4.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tvldtypes4.h5.xml 3
cd ..\temptest\fctemp
fc tvldtypes4.h5.xml tvldtypes4.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tvldtypes4.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tvldtypes4.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tvldtypes5.h5.xml --xml tvldtypes5.h5
call ..\deleteline tvldtypes5.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tvldtypes5.h5.xml 3
cd ..\temptest\fctemp
fc tvldtypes5.h5.xml tvldtypes5.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tvldtypes5.h5                                     PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tvldtypes5.h5                                     FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tvlstr.h5.xml --xml tvlstr.h5
call ..\deleteline tvlstr.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tvlstr.h5.xml 3
cd ..\temptest\fctemp
fc tvlstr.h5.xml tvlstr.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tvlstr.h5                                         PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tvlstr.h5                                         FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tsaf.h5.xml --xml tsaf.h5
call ..\deleteline tsaf.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tsaf.h5.xml 3
cd ..\temptest\fctemp
fc tsaf.h5.xml tsaf.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tsaf.h5                                           PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tsaf.h5                                           FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tempty.h5.xml --xml tempty.h5
call ..\deleteline tempty.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tempty.h5.xml 3
cd ..\temptest\fctemp
fc tempty.h5.xml tempty.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tempty.h5                                         PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tempty.h5                                         FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tnamed_dtype_attr.h5.xml --xml tnamed_dtype_attr.h5
call ..\deleteline tnamed_dtype_attr.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tnamed_dtype_attr.h5.xml 3
cd ..\temptest\fctemp
fc tnamed_dtype_attr.h5.xml tnamed_dtype_attr.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml tnamed_dtype_attr.h5                              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml tnamed_dtype_attr.h5                              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM other options for xml

REM TOOLTEST tempty-dtd.h5.xml --xml --use-dtd tempty.h5
call ..\deleteline tempty-dtd.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tempty-dtd.h5.xml 3
cd ..\temptest\fctemp
fc tempty-dtd.h5.xml tempty-dtd.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml --use-dtd tempty.h5                               PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml --use-dtd tempty.h5                               FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tempty-dtd-2.h5.xml --xml -u tempty.h5
call ..\deleteline tempty-dtd-2.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tempty-dtd-2.h5.xml 3
cd ..\temptest\fctemp
fc tempty-dtd-2.h5.xml tempty-dtd-2.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml -u tempty.h5                                      PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml -u tempty.h5                                      FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tempty-nons.h5.xml --xml -X ":" tempty.h5
call ..\deleteline tempty-nons.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tempty-nons.h5.xml 3
cd ..\temptest\fctemp
fc tempty-nons.h5.xml tempty-nons.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml -X ":" tempty.h5                                  PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml -X ":" tempty.h5                                  FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tempty-nons-2.h5.xml --xml --xml-ns=":" tempty.h5
call ..\deleteline tempty-nons-2.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tempty-nons-2.h5.xml 3
cd ..\temptest\fctemp
fc tempty-nons-2.h5.xml tempty-nons-2.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml --xml-ns=":" tempty.h5                            PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml --xml-ns=":" tempty.h5                            FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM Some of these combinations are syntactically correct but
REM the URLs are dummies 
REM TOOLTEST tempty-ns.h5.xml --xml -X "thing:" tempty.h5
call ..\deleteline tempty-ns.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tempty-ns.h5.xml 3
cd ..\temptest\fctemp
fc tempty-ns.h5.xml tempty-ns.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml -X "thing:" tempty.h5                             PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml -X "thing:" tempty.h5                             FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tempty-ns-2.h5.xml --xml --xml-ns="thing:" tempty.h5
call ..\deleteline tempty-ns-2.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tempty-ns-2.h5.xml 3
cd ..\temptest\fctemp
fc tempty-ns-2.h5.xml tempty-ns-2.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml --xml-ns="thing:" tempty.h5                       PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml --xml-ns="thing:" tempty.h5                       FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tempty-nons-uri.h5.xml --xml --xml-ns=":" --xml-dtd="http://somewhere.net" tempty.h5
call ..\deleteline tempty-nons-uri.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tempty-nons-uri.h5.xml 3
cd ..\temptest\fctemp
fc tempty-nons-uri.h5.xml tempty-nons-uri.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml-dtd="http://somewhere.net" tempty.h5              PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml-dtd="http://somewhere.net" tempty.h5              FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..
REM TOOLTEST tempty-dtd-uri.h5.xml --xml --use-dtd --xml-dtd="http://somewhere.net" tempty.h5
call ..\deleteline tempty-dtd-uri.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tempty-dtd-uri.h5.xml 3
cd ..\temptest\fctemp
fc tempty-dtd-uri.h5.xml tempty-dtd-uri.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml --use-dtd --xml-dtd="http://somewhere.net" temp   PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml --use-dtd --xml-dtd="http://somewhere.net" temp   FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

REM TOOLTEST tall-2A.h5.xml --xml -A tall.h5
call ..\deleteline tall-2A.h5.xml.results 0 
cd ..\testfiles
call ..\deleteline tall-2A.h5.xml 3
cd ..\temptest\fctemp
fc tall-2A.h5.xml tall-2A.h5.xml.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5dump --xml -A tall.h5                                        PASSED >> ..\..\dumptest%2_%1.txt
) else (
   echo Testing h5dump --xml -A tall.h5                                        FAILED >> ..\..\dumptest%2_%1.txt
   more temp.txt >> ..\..\dumptest%2_%1.txt
)
del temp.txt
cd ..

cd ..

rmdir /s/q temptest