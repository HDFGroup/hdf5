SET (CTEST_CUSTOM_MAXIMUM_NUMBER_OF_WARNINGS 1500)
 
SET (CTEST_CUSTOM_WARNING_EXCEPTION
    ${CTEST_CUSTOM_WARNING_EXCEPTION}
    "H5detect.c.[0-9]+.[ \t]*:[ \t]*warning C4090"
    "H5detect.c.[0-9]+.[ \t]*:[ \t]*warning:[ \t]*passing argument"
    "H5detect.c[0-9 \t:]*warning:[ \t]*passing argument"
    "H5Tconv.c[0-9 \t:]*warning:[ \t]*comparison is always false due to limited range of data type"
    "testhdf5.h.[0-9]+.[ \t]*:[ \t]*warning C4005"
    "H5Ztrans.c.[0-9]+.[ \t]*:[ \t]*warning C4244"
    "SZIP.src.*:[ \t]*warning"
    "POSIX name for this item is deprecated"
    "disabling jobserver mode"
)
 
SET (CTEST_CUSTOM_MEMCHECK_IGNORE
    ${CTEST_CUSTOM_MEMCHECK_IGNORE}
    flush1           #designed to fail
    flush2           #designed to need flush1
    error_test       #uses runTest.cmake
    err_compat       #uses runTest.cmake
    links_env        #uses runTest.cmake
    h5test-clear-objects
    h5perform-clear-objects
    hl_test-clear-objects
    hl_fortran_test-clear-objects
    ######### tools/h5copy #########
    H5COPY-clearall-objects
    H5COPY-H5LS_h5copytst-basic       #uses runTest.cmake
    H5COPY-clear-refs
    H5COPY-region_ref                 #needs clear-refs
    H5COPY-H5LS_h5copy_ref-refs       #uses runTest.cmake
    H5COPY-clear-ext-links
    H5COPY-ext_link                   #needs clear-ext-links
    H5COPY-ext_link_f                 #needs clear-ext-links
    H5COPY-ext_dangle_noobj           #needs clear-ext-links
    H5COPY-ext_dangle_noobj_f         #needs clear-ext-links
    H5COPY-ext_dangle_nofile          #needs clear-ext-links
    H5COPY-ext_dangle_nofile_f        #needs clear-ext-links
    H5COPY-ext_link_group             #needs clear-ext-links
    H5COPY-ext_link_group_f           #needs clear-ext-links
    H5COPY-H5LS_h5copy_extlinks_src-links    #uses runTest.cmake
    H5COPY-clear-misc
    H5COPY-CMP-h5copy_misc1           #uses runTest.cmake
    ######### tools/h5diff #########
    H5DIFF-clearall-objects
    H5DIFF-h5diff_10      #uses runTest.cmake
    H5DIFF-h5diff_11      #uses runTest.cmake
    H5DIFF-h5diff_12      #uses runTest.cmake
    H5DIFF-h5diff_13      #uses runTest.cmake
    H5DIFF-h5diff_14      #uses runTest.cmake
    H5DIFF-h5diff_15      #uses runTest.cmake
    H5DIFF-h5diff_16_1    #uses runTest.cmake
    H5DIFF-h5diff_16_2    #uses runTest.cmake
    H5DIFF-h5diff_16_3    #uses runTest.cmake
    H5DIFF-h5diff_17      #uses runTest.cmake
    H5DIFF-h5diff_171     #uses runTest.cmake
    H5DIFF-h5diff_172     #uses runTest.cmake
    H5DIFF-h5diff_18      #uses runTest.cmake
    H5DIFF-h5diff_18_1    #uses runTest.cmake
    H5DIFF-h5diff_20      #uses runTest.cmake
    H5DIFF-h5diff_21      #uses runTest.cmake
    H5DIFF-h5diff_22      #uses runTest.cmake
    H5DIFF-h5diff_23      #uses runTest.cmake
    H5DIFF-h5diff_24      #uses runTest.cmake
    H5DIFF-h5diff_25      #uses runTest.cmake
    H5DIFF-h5diff_26      #uses runTest.cmake
    H5DIFF-h5diff_27      #uses runTest.cmake
    H5DIFF-h5diff_28      #uses runTest.cmake
    H5DIFF-h5diff_50      #uses runTest.cmake
    H5DIFF-h5diff_51      #uses runTest.cmake
    H5DIFF-h5diff_52      #uses runTest.cmake
    H5DIFF-h5diff_53      #uses runTest.cmake
    H5DIFF-h5diff_54      #uses runTest.cmake
    H5DIFF-h5diff_55      #uses runTest.cmake
    H5DIFF-h5diff_56      #uses runTest.cmake
    H5DIFF-h5diff_57      #uses runTest.cmake
    H5DIFF-h5diff_58      #uses runTest.cmake
    H5DIFF-h5diff_600     #uses runTest.cmake
    H5DIFF-h5diff_601     #uses runTest.cmake
    H5DIFF-h5diff_603     #uses runTest.cmake
    H5DIFF-h5diff_604     #uses runTest.cmake
    H5DIFF-h5diff_605     #uses runTest.cmake
    H5DIFF-h5diff_606     #uses runTest.cmake
    H5DIFF-h5diff_607     #uses runTest.cmake
    H5DIFF-h5diff_608     #uses runTest.cmake
    H5DIFF-h5diff_609     #uses runTest.cmake
    H5DIFF-h5diff_610     #uses runTest.cmake
    H5DIFF-h5diff_612     #uses runTest.cmake
    H5DIFF-h5diff_613     #uses runTest.cmake
    H5DIFF-h5diff_614     #uses runTest.cmake
    H5DIFF-h5diff_615     #uses runTest.cmake
    H5DIFF-h5diff_616     #uses runTest.cmake
    H5DIFF-h5diff_617     #uses runTest.cmake
    H5DIFF-h5diff_618     #uses runTest.cmake
    H5DIFF-h5diff_619     #uses runTest.cmake
    H5DIFF-h5diff_621     #uses runTest.cmake
    H5DIFF-h5diff_622     #uses runTest.cmake
    H5DIFF-h5diff_623     #uses runTest.cmake
    H5DIFF-h5diff_624     #uses runTest.cmake
    H5DIFF-h5diff_625     #uses runTest.cmake
    H5DIFF-h5diff_626     #uses runTest.cmake
    H5DIFF-h5diff_627     #uses runTest.cmake
    H5DIFF-h5diff_628     #uses runTest.cmake
    H5DIFF-h5diff_70      #uses runTest.cmake
    H5DIFF-h5diff_80      #uses runTest.cmake
    H5DIFF-h5diff_90      #uses runTest.cmake
    H5DIFF-h5diff_101     #uses runTest.cmake
    H5DIFF-h5diff_102     #uses runTest.cmake
    H5DIFF-h5diff_200     #uses runTest.cmake
    H5DIFF-h5diff_201     #uses runTest.cmake
    H5DIFF-h5diff_202     #uses runTest.cmake
    H5DIFF-h5diff_203     #uses runTest.cmake
    H5DIFF-h5diff_204     #uses runTest.cmake
    H5DIFF-h5diff_205     #uses runTest.cmake
    H5DIFF-h5diff_206     #uses runTest.cmake
    H5DIFF-h5diff_207     #uses runTest.cmake
    H5DIFF-h5diff_300     #uses runTest.cmake
    H5DIFF-h5diff_400     #uses runTest.cmake
    H5DIFF-h5diff_401     #uses runTest.cmake
    H5DIFF-h5diff_402     #uses runTest.cmake
    H5DIFF-h5diff_403     #uses runTest.cmake
    H5DIFF-h5diff_404     #uses runTest.cmake
    H5DIFF-h5diff_405     #uses runTest.cmake
    H5DIFF-h5diff_406     #uses runTest.cmake
    H5DIFF-h5diff_407     #uses runTest.cmake
    H5DIFF-h5diff_408     #uses runTest.cmake
    H5DIFF-h5diff_409     #uses runTest.cmake
    H5DIFF-h5diff_410     #uses runTest.cmake
    H5DIFF-h5diff_411     #uses runTest.cmake
    H5DIFF-h5diff_412     #uses runTest.cmake
    H5DIFF-h5diff_413     #uses runTest.cmake
    H5DIFF-h5diff_414     #uses runTest.cmake
    H5DIFF-h5diff_415     #uses runTest.cmake
    H5DIFF-h5diff_416     #uses runTest.cmake
    H5DIFF-h5diff_417     #uses runTest.cmake
    H5DIFF-h5diff_418     #uses runTest.cmake
    H5DIFF-h5diff_419     #uses runTest.cmake
    H5DIFF-h5diff_420     #uses runTest.cmake
    H5DIFF-h5diff_421     #uses runTest.cmake
    H5DIFF-h5diff_422     #uses runTest.cmake
    H5DIFF-h5diff_423     #uses runTest.cmake
    H5DIFF-h5diff_424     #uses runTest.cmake
    H5DIFF-h5diff_425     #uses runTest.cmake
    H5DIFF-h5diff_450     #uses runTest.cmake
    H5DIFF-h5diff_451     #uses runTest.cmake
    H5DIFF-h5diff_452     #uses runTest.cmake
    H5DIFF-h5diff_453     #uses runTest.cmake
    H5DIFF-h5diff_454     #uses runTest.cmake
    H5DIFF-h5diff_455     #uses runTest.cmake
    H5DIFF-h5diff_456     #uses runTest.cmake
    H5DIFF-h5diff_457     #uses runTest.cmake
    H5DIFF-h5diff_458     #uses runTest.cmake
    H5DIFF-h5diff_459     #uses runTest.cmake
    H5DIFF-h5diff_500     #uses runTest.cmake
    H5DIFF-h5diff_501     #uses runTest.cmake
    H5DIFF-h5diff_502     #uses runTest.cmake
    H5DIFF-h5diff_503     #uses runTest.cmake
    H5DIFF-h5diff_504     #uses runTest.cmake
    H5DIFF-h5diff_505     #uses runTest.cmake
    H5DIFF-h5diff_506     #uses runTest.cmake
    H5DIFF-h5diff_507     #uses runTest.cmake
    H5DIFF-h5diff_508     #uses runTest.cmake
    H5DIFF-h5diff_509     #uses runTest.cmake
    H5DIFF-h5diff_510     #uses runTest.cmake
    H5DIFF-h5diff_511     #uses runTest.cmake
    H5DIFF-h5diff_512     #uses runTest.cmake
    H5DIFF-h5diff_513     #uses runTest.cmake
    H5DIFF-h5diff_514     #uses runTest.cmake
    H5DIFF-h5diff_515     #uses runTest.cmake
    H5DIFF-h5diff_516     #uses runTest.cmake
    H5DIFF-h5diff_517     #uses runTest.cmake
    H5DIFF-h5diff_518     #uses runTest.cmake
    H5DIFF-h5diff_480     #uses runTest.cmake
    H5DIFF-h5diff_481     #uses runTest.cmake
    H5DIFF-h5diff_482     #uses runTest.cmake
    H5DIFF-h5diff_483     #uses runTest.cmake
    H5DIFF-h5diff_484     #uses runTest.cmake
    H5DIFF-h5diff_530     #uses runTest.cmake
    ######### tools/h5dump #########
    H5DUMP-clearall-objects
    H5DUMP-packedbits      #uses runTest.cmake
    H5DUMP-tgroup-1        #uses runTest.cmake
    H5DUMP-tgroup-2        #uses runTest.cmake
    H5DUMP-tdset-1         #uses runTest.cmake
    H5DUMP-tdset-2         #uses runTest.cmake
    H5DUMP-tattr-1         #uses runTest.cmake
    H5DUMP-tattr-2         #uses runTest.cmake
    H5DUMP-tattr-3         #uses runTest.cmake
    H5DUMP-tnamed_dtype_attr     #uses runTest.cmake
    H5DUMP-tslink-1        #uses runTest.cmake
    H5DUMP-tudlink-1       #uses runTest.cmake
    H5DUMP-tslink-2        #uses runTest.cmake
    H5DUMP-tudlink-2       #uses runTest.cmake
    H5DUMP-thlink-1        #uses runTest.cmake
    H5DUMP-thlink-2        #uses runTest.cmake
    H5DUMP-thlink-3        #uses runTest.cmake
    H5DUMP-thlink-4        #uses runTest.cmake
    H5DUMP-thlink-5        #uses runTest.cmake
    H5DUMP-tcomp-1         #uses runTest.cmake
    H5DUMP-tcomp-2         #uses runTest.cmake
    H5DUMP-tcomp-4         #uses runTest.cmake
    H5DUMP-tnestcomp-1     #uses runTest.cmake
    H5DUMP-tall-1          #uses runTest.cmake
    H5DUMP-tall-2          #uses runTest.cmake
    H5DUMP-tall-3          #uses runTest.cmake
    H5DUMP-tloop-1         #uses runTest.cmake
    H5DUMP-tstr-1          #uses runTest.cmake
    H5DUMP-tstr-2          #uses runTest.cmake
    H5DUMP-tsaf            #uses runTest.cmake
    H5DUMP-tvldtypes1      #uses runTest.cmake
    H5DUMP-tvldtypes2      #uses runTest.cmake
    H5DUMP-tvldtypes3      #uses runTest.cmake
    H5DUMP-tvldtypes4      #uses runTest.cmake
    H5DUMP-tvldtypes5      #uses runTest.cmake
    H5DUMP-tvlstr          #uses runTest.cmake
    H5DUMP-tarray1         #uses runTest.cmake
    H5DUMP-tarray2         #uses runTest.cmake
    H5DUMP-tarray3         #uses runTest.cmake
    H5DUMP-tarray4         #uses runTest.cmake
    H5DUMP-tarray5         #uses runTest.cmake
    H5DUMP-tarray6         #uses runTest.cmake
    H5DUMP-tarray7         #uses runTest.cmake
    H5DUMP-tarray8         #uses runTest.cmake
    H5DUMP-tempty          #uses runTest.cmake
    H5DUMP-tgrp_comments   #uses runTest.cmake
    H5DUMP-tsplit_file     #uses runTest.cmake
    H5DUMP-tfamily         #uses runTest.cmake
    H5DUMP-tmulti          #uses runTest.cmake
    H5DUMP-tlarge_objname  #uses runTest.cmake
    H5DUMP-tall-2A         #uses runTest.cmake
    H5DUMP-tall-2B         #uses runTest.cmake
    H5DUMP-tall-4s         #uses runTest.cmake
    H5DUMP-tall-5s         #uses runTest.cmake
    H5DUMP-tdset-3s        #uses runTest.cmake
    H5DUMP-tchar1          #uses runTest.cmake
    H5DUMP-tchar1          #uses runTest.cmake
    H5DUMP-tnofilename     #uses runTest.cmake
    H5DUMP-tboot1          #uses runTest.cmake
    H5DUMP-tboot2          #uses runTest.cmake
    H5DUMP-tperror         #uses runTest.cmake
    H5DUMP-tcontents       #uses runTest.cmake
    H5DUMP-tcompact        #uses runTest.cmake
    H5DUMP-tcontiguos      #uses runTest.cmake
    H5DUMP-tchunked        #uses runTest.cmake
    H5DUMP-texternal       #uses runTest.cmake
    H5DUMP-tfill           #uses runTest.cmake
    H5DUMP-packedbits      #uses runTest.cmake
    H5DUMP-tpbitsSignedWhole              #uses runTest.cmake
    H5DUMP-tpbitsSignedIntWhole           #uses runTest.cmake
    H5DUMP-tpbitsSignedLongWhole          #uses runTest.cmake
    H5DUMP-tpbitsSignedLongLongWhole      #uses runTest.cmake
    H5DUMP-tpbitsSignedLongLongWhole63    #uses runTest.cmake
    H5DUMP-tpbitsSignedLongLongWhole1     #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedWhole              #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedIntWhole           #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedLongWhole          #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedLongLongWhole      #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedLongLongWhole63    #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedLongLongWhole1     #uses runTest.cmake
    H5DUMP-tpbitsSigned2                     #uses runTest.cmake
    H5DUMP-tpbitsSigned4                     #uses runTest.cmake
    H5DUMP-tpbitsSignedInt                   #uses runTest.cmake
    H5DUMP-tpbitsSignedInt4                  #uses runTest.cmake
    H5DUMP-tpbitsSignedInt8                  #uses runTest.cmake
    H5DUMP-tpbitsSignedLong                  #uses runTest.cmake
    H5DUMP-tpbitsSignedLong8                 #uses runTest.cmake
    H5DUMP-tpbitsSignedLong16                #uses runTest.cmake
    H5DUMP-tpbitsSignedLongLong              #uses runTest.cmake
    H5DUMP-tpbitsSignedLongLong16            #uses runTest.cmake
    H5DUMP-tpbitsSignedLongLong32            #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSigned2             #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSigned4             #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedInt           #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedInt4          #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedInt8          #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedLong          #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedLong8         #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedLong16        #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedLongLong      #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedLongLong16    #uses runTest.cmake
    H5DUMP-tpbitsUnsignedSignedLongLong32    #uses runTest.cmake
    H5DUMP-tpbitsCharOffsetExceeded          #uses runTest.cmake
    H5DUMP-tpbitsIntOffsetExceeded           #uses runTest.cmake
    H5DUMP-tpbitsLongOffsetExceeded          #uses runTest.cmake
    H5DUMP-tpbitsCharLengthExceeded          #uses runTest.cmake
    H5DUMP-tpbitsIntLengthExceeded           #uses runTest.cmake
    H5DUMP-tpbitsLongLengthExceeded          #uses runTest.cmake
    H5DUMP-treference      #uses runTest.cmake
    H5DUMP-tstringe        #uses runTest.cmake
    H5DUMP-tstring         #uses runTest.cmake
    H5DUMP-tstring2        #uses runTest.cmake
    H5DUMP-tindicesyes     #uses runTest.cmake
    H5DUMP-tindicesno      #uses runTest.cmake
    H5DUMP-tindicessub1    #uses runTest.cmake
    H5DUMP-tindicessub2    #uses runTest.cmake
    H5DUMP-tindicessub3    #uses runTest.cmake
    H5DUMP-tindicessub4    #uses runTest.cmake
    H5DUMP-tszip           #uses runTest.cmake
    H5DUMP-tdeflate        #uses runTest.cmake
    H5DUMP-tshuffle        #uses runTest.cmake
    H5DUMP-tfletcher32     #uses runTest.cmake
    H5DUMP-tnbit           #uses runTest.cmake
    H5DUMP-tscaleoffset    #uses runTest.cmake
    H5DUMP-tallfilters     #uses runTest.cmake
    H5DUMP-tuserfilter     #uses runTest.cmake
    H5DUMP-tlonglinks      #uses runTest.cmake
    H5DUMP-tbigdims        #uses runTest.cmake
    H5DUMP-thyperslab      #uses runTest.cmake
    H5DUMP-tnullspace      #uses runTest.cmake
    H5DUMP-tvms            #uses runTest.cmake
    H5DUMP-tbin1LE         #uses runTest.cmake
    H5DUMP-tbin1           #uses runTest.cmake
    H5DUMP-clear-out1
    H5DUMP-h5import-out1
    H5DUMP-h5diff-out1
    H5DUMP-tbin2                     #uses runTest.cmake
    H5DUMP-tbin3                     #uses runTest.cmake
    H5DUMP-clear-out3
    H5DUMP-h5import-out3
    H5DUMP-h5diff-out3
    H5DUMP-clear-objects
    H5DUMP-tbin4                     #uses runTest.cmake
    H5DUMP-tdatareg                  #uses runTest.cmake
    H5DUMP-tdataregR                 #uses runTest.cmake
    H5DUMP-tattrreg                  #uses runTest.cmake
    H5DUMP-tattrregR                 #uses runTest.cmake
    H5DUMP-output-tbinregR           #uses runTest.cmake
    H5DUMP-output-cmp-tbinregR       #uses runTest.cmake
    H5DUMP-tordergr1                 #uses runTest.cmake
    H5DUMP-tordergr2                 #uses runTest.cmake
    H5DUMP-tordergr3                 #uses runTest.cmake
    H5DUMP-tordergr4                 #uses runTest.cmake
    H5DUMP-tordergr5                 #uses runTest.cmake
    H5DUMP-torderattr1               #uses runTest.cmake
    H5DUMP-torderattr2               #uses runTest.cmake
    H5DUMP-torderattr3               #uses runTest.cmake
    H5DUMP-torderattr4               #uses runTest.cmake
    H5DUMP-tfpformat                 #uses runTest.cmake
    H5DUMP-textlinksrc               #uses runTest.cmake
    H5DUMP-textlinkfar               #uses runTest.cmake
    H5DUMP-textlink                  #uses runTest.cmake
    H5DUMP_PACKED_BITS-clearall-objects
    H5DUMP-tpackedbits               #uses runTest.cmake
    H5DUMP-tpackedbits2              #uses runTest.cmake
    H5DUMP-tnofilename-with-packed-bits     #uses runTest.cmake
    H5DUMP-tpbitsSigned              #uses runTest.cmake
    H5DUMP-tpbitsUnsigned            #uses runTest.cmake
    H5DUMP-tpbitsOverlapped          #uses runTest.cmake
    H5DUMP-tpbitsMax                 #uses runTest.cmake
    H5DUMP-tpbitsCompound            #uses runTest.cmake
    H5DUMP-tpbitsArray               #uses runTest.cmake
    H5DUMP-tpbitsMaxExceeded         #uses runTest.cmake
    H5DUMP-tpbitsOffsetExceeded      #uses runTest.cmake
    H5DUMP-tpbitsOffsetNegative      #uses runTest.cmake
    H5DUMP-tpbitsLengthPositive      #uses runTest.cmake
    H5DUMP-tpbitsLengthExceeded      #uses runTest.cmake
    H5DUMP-tpbitsIncomplete          #uses runTest.cmake
    H5DUMP-XML-clearall-objects
    H5DUMP-XML-tall.h5               #uses runTest.cmake
    H5DUMP-XML-tattr.h5              #uses runTest.cmake
    H5DUMP-XML-tbitfields.h5         #uses runTest.cmake
    H5DUMP-XML-tcompound.h5          #uses runTest.cmake
    H5DUMP-XML-tcompound2.h5         #uses runTest.cmake
    H5DUMP-XML-tdatareg.h5           #uses runTest.cmake
    H5DUMP-XML-tdset.h5              #uses runTest.cmake
    H5DUMP-XML-tdset2.h5             #uses runTest.cmake
    H5DUMP-XML-tenum.h5              #uses runTest.cmake
    H5DUMP-XML-tgroup.h5             #uses runTest.cmake
    H5DUMP-XML-thlink.h5             #uses runTest.cmake
    H5DUMP-XML-tloop.h5              #uses runTest.cmake
    H5DUMP-XML-tloop2.h5             #uses runTest.cmake
    H5DUMP-XML-tmany.h5              #uses runTest.cmake
    H5DUMP-XML-tnestedcomp.h5        #uses runTest.cmake
    H5DUMP-XML-tcompound_complex.h5  #uses runTest.cmake
    H5DUMP-XML-tobjref.h5            #uses runTest.cmake
    H5DUMP-XML-topaque.h5            #uses runTest.cmake
    H5DUMP-XML-tslink.h5             #uses runTest.cmake
    H5DUMP-XML-tudlink.h5            #uses runTest.cmake
    H5DUMP-XML-textlink.h5           #uses runTest.cmake
    H5DUMP-XML-tstr.h5               #uses runTest.cmake
    H5DUMP-XML-tstr2.h5              #uses runTest.cmake
    H5DUMP-XML-tref.h5               #uses runTest.cmake
    H5DUMP-XML-tname-amp.h5          #uses runTest.cmake
    H5DUMP-XML-tname-apos.h5         #uses runTest.cmake
    H5DUMP-XML-tname-gt.h5           #uses runTest.cmake
    H5DUMP-XML-tname-lt.h5           #uses runTest.cmake
    H5DUMP-XML-tname-quot.h5         #uses runTest.cmake
    H5DUMP-XML-tname-sp.h5           #uses runTest.cmake
    H5DUMP-XML-tstring.h5            #uses runTest.cmake
    H5DUMP-XML-tstring-at.h5         #uses runTest.cmake
    H5DUMP-XML-tref-escapes.h5       #uses runTest.cmake
    H5DUMP-XML-tref-escapes-at.h5    #uses runTest.cmake
    H5DUMP-XML-tnodata.h5            #uses runTest.cmake
    H5DUMP-XML-tarray1.h5            #uses runTest.cmake
    H5DUMP-XML-tarray2.h5            #uses runTest.cmake
    H5DUMP-XML-tarray3.h5            #uses runTest.cmake
    H5DUMP-XML-tarray6.h5            #uses runTest.cmake
    H5DUMP-XML-tarray7.h5            #uses runTest.cmake
    H5DUMP-XML-tvldtypes1.h5         #uses runTest.cmake
    H5DUMP-XML-tvldtypes2.h5         #uses runTest.cmake
    H5DUMP-XML-tvldtypes3.h5         #uses runTest.cmake
    H5DUMP-XML-tvldtypes4.h5         #uses runTest.cmake
    H5DUMP-XML-tvldtypes5.h5         #uses runTest.cmake
    H5DUMP-XML-tvlstr.h5             #uses runTest.cmake
    H5DUMP-XML-tsaf.h5               #uses runTest.cmake
    H5DUMP-XML-tempty.h5             #uses runTest.cmake
    H5DUMP-XML-tnamed_dtype_attr.h5  #uses runTest.cmake
    H5DUMP-XML-tempty-dtd.h5         #uses runTest.cmake
    H5DUMP-XML-tempty-dtd-2.h5       #uses runTest.cmake
    H5DUMP-XML-tempty-nons.h5        #uses runTest.cmake
    H5DUMP-XML-tempty-nons-2.h5      #uses runTest.cmake
    H5DUMP-XML-tempty-ns.h5          #uses runTest.cmake
    H5DUMP-XML-tempty-ns-2.h5        #uses runTest.cmake
    H5DUMP-XML-tempty-nons-uri.h5    #uses runTest.cmake
    H5DUMP-XML-tempty-dtd-uri.h5     #uses runTest.cmake
    H5DUMP-XML-tall-2A.h5            #uses runTest.cmake
    H5DUMP-XML-torderattr1.h5        #uses runTest.cmake
    H5DUMP-XML-torderattr2.h5        #uses runTest.cmake
    H5DUMP-XML-torderattr3.h5        #uses runTest.cmake
    H5DUMP-XML-torderattr4.h5        #uses runTest.cmake
    H5DUMP-XML-tfpformat.h5          #uses runTest.cmake
    ######### tools/h5import #########
    H5IMPORT-clear-objects
    ######### tools/h5ls #########
    H5LS-clearall-objects
    H5LS-help-1                     #uses runTest.cmake
    H5LS-help-2                     #uses runTest.cmake
    H5LS-help-3                     #uses runTest.cmake
    H5LS-tall-1                     #uses runTest.cmake
    H5LS-tall-2                     #uses runTest.cmake
    H5LS-tgroup                     #uses runTest.cmake
    H5LS-tgroup-3                   #uses runTest.cmake
    H5LS-tgroup-1                   #uses runTest.cmake
    H5LS-tgroup-2                   #uses runTest.cmake
    H5LS-tdset-1                    #uses runTest.cmake
    H5LS-tslink-1                   #uses runTest.cmake
    H5LS-tsoftlinks-1               #uses runTest.cmake
    H5LS-tsoftlinks-2               #uses runTest.cmake
    H5LS-tsoftlinks-3               #uses runTest.cmake
    H5LS-tsoftlinks-4               #uses runTest.cmake
    H5LS-tsoftlinks-5               #uses runTest.cmake
    H5LS-textlink-1                 #uses runTest.cmake
    H5LS-textlinksrc-1              #uses runTest.cmake
    H5LS-textlinksrc-2              #uses runTest.cmake
    H5LS-textlinksrc-3              #uses runTest.cmake
    H5LS-textlinksrc-4              #uses runTest.cmake
    H5LS-textlinksrc-5              #uses runTest.cmake
    H5LS-textlinksrc-6              #uses runTest.cmake
    H5LS-textlinksrc-7              #uses runTest.cmake
    H5LS-tudlink-1                  #uses runTest.cmake
    H5LS-textlinksrc-1-old          #uses runTest.cmake
    H5LS-textlinksrc-2-old          #uses runTest.cmake
    H5LS-textlinksrc-3-old          #uses runTest.cmake
    H5LS-textlinksrc-6-old          #uses runTest.cmake
    H5LS-textlinksrc-7-old          #uses runTest.cmake
    H5LS-textlinksrc-nodangle-1     #uses runTest.cmake
    H5LS-textlinksrc-nodangle-2     #uses runTest.cmake
    H5LS-tsoftlinks-nodangle-1      #uses runTest.cmake
    H5LS-thlinks-nodangle-1         #uses runTest.cmake
    H5LS-thlink-1                   #uses runTest.cmake
    H5LS-tcomp-1                    #uses runTest.cmake
    H5LS-tnestcomp-1                #uses runTest.cmake
    H5LS-tnestcomp-2                #uses runTest.cmake
    H5LS-tnestcomp-3                #uses runTest.cmake
    H5LS-tnestcomp-4                #uses runTest.cmake
    H5LS-tloop-1                    #uses runTest.cmake
    H5LS-tstr-1                     #uses runTest.cmake
    H5LS-tsaf                       #uses runTest.cmake
    H5LS-tvldtypes1                 #uses runTest.cmake
    H5LS-tarray1                    #uses runTest.cmake
    H5LS-tempty                     #uses runTest.cmake
    H5LS-tattr2                     #uses runTest.cmake
    H5LS-nosuchfile                 #uses runTest.cmake
    H5LS-tvldtypes2le               #uses runTest.cmake
    H5LS-tdataregle                 #uses runTest.cmake
    ######### tools/h5repack #########
    H5REPACK-clearall-objects
    H5REPACK-gzip_verbose_filters                 #uses runTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_chunk_20x10            #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-chunk_20x10              #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_conti                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-conti                    #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_compa                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-compa                    #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_conti             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_chunk             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_compa             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_compa             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_chunk             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_conti             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_compa                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_conti                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_18x13                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-contig_small_compa           #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-contig_small_fixed_compa     #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-layout_long_switches     #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-layout_short_switches    #uses grepTest.cmake
    ######### tools/h5stat #########
    H5STAT-clearall-objects
    H5STAT-h5stat_help1            #uses runTest.cmake
    H5STAT-h5stat_help2            #uses runTest.cmake
    H5STAT-h5stat_filters          #uses runTest.cmake
    H5STAT-h5stat_filters-file     #uses runTest.cmake
    H5STAT-h5stat_filters-F        #uses runTest.cmake
    H5STAT-h5stat_filters-d        #uses runTest.cmake
    H5STAT-h5stat_filters-g        #uses runTest.cmake
    H5STAT-h5stat_filters-dT       #uses runTest.cmake
    H5STAT-h5stat_filters-UD       #uses runTest.cmake
    H5STAT-h5stat_filters-UT       #uses runTest.cmake
    H5STAT-h5stat_tsohm            #uses runTest.cmake
    H5STAT-h5stat_newgrat          #uses runTest.cmake
    H5STAT-h5stat_newgrat-UG       #uses runTest.cmake
    H5STAT-h5stat_newgrat-UA       #uses runTest.cmake
    ######### tools/misc #########
    h5repart_20K-clear-objects
    h5repart_5K-clear-objects
    h5repart_sec2-clear-objects
    H5MKGRP-h5mkgrp_help                                #uses runTest.cmake
    H5MKGRP-h5mkgrp_version                             #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_single
    H5MKGRP-h5mkgrp_single                              #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single                         #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_single-v
    H5MKGRP-h5mkgrp_single-v                            #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single-v                       #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_single-p
    H5MKGRP-h5mkgrp_single-p                            #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single-p                       #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_single_latest-l
    H5MKGRP-h5mkgrp_single_latest-l                     #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single_latest-l                #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several
    H5MKGRP-h5mkgrp_several                             #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several                        #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several-v
    H5MKGRP-h5mkgrp_several-v                           #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several-v                      #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several-p
    H5MKGRP-h5mkgrp_several-p                           #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several-p                      #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several_latest-l
    H5MKGRP-h5mkgrp_several_latest-l                    #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several_latest-l               #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested-p
    H5MKGRP-h5mkgrp_nested-p                            #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested-p                       #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested_latest-lp
    H5MKGRP-h5mkgrp_nested_latest-lp                    #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested_latest-lp               #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested_mult-p
    H5MKGRP-h5mkgrp_nested_mult-p                       #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested_mult-p                  #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested_mult_latest-lp
    H5MKGRP-h5mkgrp_nested_mult_latest-lp               #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested_mult_latest-lp          #uses runTest.cmake
)
