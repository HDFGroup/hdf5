#!rbin/sh 
# HDF Utilities Test script
# Usage: h5importtestutil.sh [machine-type]

# initialize errors variable
errors=0

TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

TOOLTEST()
{
err=0
./h5import $*
../h5dump/h5dump $5 >log2

cd testfiles
../../h5dump/h5dump $5 >log1
cd ..

cmp -s testfiles/log1 log2 || err=1
rm -f log2 testfiles/log1
if [ $err -eq 1 ]; then
errors="` expr $errors + 1 `";
  echo "*FAILED*"
else
  echo " PASSED"
fi
}

echo "" 
echo "=============================="
echo "H5IMPORT tests started"
echo "=============================="

if [ -f h5import -a -f h5importtest ]; then
#echo "** Testing h5import  ***"

rm -f  output.h5 log1 testfiles/tx* testfiles/b* *.dat

./h5importtest

TESTING "ASCII I32 rank 3 - Output BE " ;
TOOLTEST testfiles/txtin32 -c testfiles/textin32 -o test1.h5

TESTING "ASCII I16 rank 3 - Output LE - CHUNKED - extended" 
TOOLTEST testfiles/txtin16 -c testfiles/textin16 -o test2.h5

TESTING "ASCII I8 - rank 3 - Output I16 LE-Chunked+Extended+Compressed " 
TOOLTEST testfiles/txtin16 -c testfiles/textin8  -o test3.h5

TESTING "ASCII UI32 - rank 3 - Output BE" 
TOOLTEST testfiles/in1 -c testfiles/textuin32 -o test4.h5

TESTING "ASCII UI16 - rank 2 - Output LE+Chunked+Compressed " 
TOOLTEST testfiles/in1 -c testfiles/textuin16 -o test5.h5

TESTING "ASCII F32 - rank 3 - Output LE " 
TOOLTEST testfiles/fp1 -c testfiles/textfp32 -o test6.h5

TESTING "ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed " 
TOOLTEST testfiles/fp2 -c testfiles/textfp64 -o test7.h5

TESTING "BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed " 
TOOLTEST testfiles/bfp64 -c testfiles/conbfp64 -o test8.h5

TESTING "BINARY I16 - rank 3 - Output order LE + CHUNKED + extended " 
TOOLTEST testfiles/bin16 -c testfiles/conbin16 -o test9.h5

TESTING "BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed " 
TOOLTEST testfiles/bin8 -c testfiles/conbin8  -o test10.h5

TESTING "BINARY I32 - rank 3 - Output BE + CHUNKED " 
TOOLTEST testfiles/bin32 -c testfiles/conbin32 -o test11.h5

TESTING "BINARY UI16 - rank 3 - Output byte BE + CHUNKED " 
TOOLTEST testfiles/buin16 -c testfiles/conbuin16 -o test12.h5

TESTING "BINARY UI32 - rank 3 - Output LE + CHUNKED " 
TOOLTEST testfiles/buin32 -c testfiles/conbuin32 -o test13.h5

rm -f  testfiles/tx* testfiles/b* *.dat
rm -f  test*.h5 testfiles/test*.log
else
	echo "** h5import or h5importtest not available ***"
	errors="` expr $errors + 1 `";
fi

#
# Check errors result
if [ $errors -eq 0 ]; then
    echo "======================================"
    echo " H5IMPORT Utilities tests have passed."
    echo "======================================"    
else
    echo "*********************************************"
    echo " H5IMPORT Utilities tests encountered errors"
    echo "*********************************************"
fi
exit $errors
