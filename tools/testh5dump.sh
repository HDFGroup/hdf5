#! /bin/sh
# Test scripts for h5dump.
# See the USAGE function for command usage.


# Definitions of commands and variables
CMD='../h5dump'
RM='rm -f'
DIFF=diff
CMP='cmp -s'
nerrors=0		# number of errors (0)
quitonerr=0		# quit on error (not)
noclean=0		# no cleaning temp. files (yes)
only=""			# dumper sub-command to test only
except=""		# dumper sub-command to test not


# Definitions of functions/shorthands
#

# Print Usage of the command
USAGE()
{
    echo "Usage: $0 [-help] [-noclean] [-quit] [-except <command>] [-only <command>]"
    echo "    -help: display help information"
    echo "    -noclean: do not clean away temporary files"
    echo "    -quit: quit immediately if any test fails"
    echo "    -except: skip one specific command"
    echo "    -only: test one specific command"
    echo "<command> can be one of {list, dumpsds, dumprig, dumpvd, dumpvg, dumpgr}"
}

# Print message with formats according to message level ($1)
MESG()
{
    level=$1
    shift
    case $level in
	0)
	    echo '============================='
	    echo $*
	    echo '============================='
	    ;;
	3)
	    echo '-----------------------------'
	    echo $*
	    echo '-----------------------------'
	    ;;
	6)
	    echo "*** $* ***"
	    ;;
	*)
	    echo "MESG(): Unknown level ($level)"
	    exit 1
	    ;;
    esac

}


# Run the test to produce an output file which is then
# compared with the expected ($1) output.
# Note that this can be used to produce the expected
# output files by replace "$output" with "$expected"
# in the run-the-test commands.
TEST()
{
    # parse the arguments
    output=tmp.out
    expected=testfiles/$1
    shift
    # print a id banner
    MESG 6 $@
    # run the test
    ( 
	echo "#############################"
	echo "Expected output for '$CMD $@'" 
	echo "#############################"
	cd testfiles
        $CMD "$@"
    ) > $output
    $CMP $expected $output
    if [ $? -ne 0 ]
    then
	echo $DIFF $expected $output
	$DIFF $expected $output
	echo "   <<< FAILED >>>"
	nerrors=`expr $nerrors + 1`
	if [ $quitonerr -gt 0 ]; 
	then
	    FINISH
	fi
    fi
#    if [ $noclean -eq 0 ]
#    then
#	$RM $output
#    fi
}


# Report the result and exit
FINISH()
{
    if [ $nerrors -eq 0 ]
    then
	MESG 0 "All h5dump tests passed"
    else
	MESG 0 "h5dump tests failed: $nerrors"
    fi
    exit $nerrors
}


#===============
# Main Body
#===============

# parse arguments
while [ $# -gt 0 ]
do
    case "$1" in
	"-quit")
	    quitonerr=1
	    ;;
	"-noclean")
	    noclean=1
	    ;;
	"-help")
	    USAGE
	    exit 0
	    ;;
	"-only")
	    shift
	    case "$1" in
    		"h5dump")
		    only="$1"
		    ;;
		*)
		    echo "Unknown command: $1"
		    USAGE
		    exit 1
		    ;;
	    esac
	    ;;
	"-except")
	    shift
	    case "$1" in
    		"h5dump")
		    except="$1"
		    ;;
		*)
		    echo "Unknown command: $1"
		    USAGE
		    exit 1
		    ;;
	    esac
	    ;;
	* )
	    echo "Unknow option: $1"
	    USAGE
	    exit 1
	    ;;
    esac
    shift
done

# Print a beginning banner
MESG 0 "Running h5dump tests"

# Test command list
TestCmd=h5dump
TestName="Test command $TestCmd"
if [ "$except" != $TestCmd -a \( -z "$only" -o "$only" = $TestCmd \) ]
then
MESG 3 "$TestName"
TEST tgroup-1.ddl tgroup.h5
TEST tgroup-2.ddl -g / tgroup.h5
TEST tgroup-3.ddl -g /g2 /y tgroup.h5

TEST tdset-1.ddl tdset.h5
TEST tdset-2.ddl -d dset1 /dset2 tdset.h5
TEST tdset-3.ddl -d /dset1 -header tdset.h5
TEST tdset-4.ddl -d dset3 tdset.h5

TEST tattr-1.ddl tattr.h5
TEST tattr-2.ddl -a attr1 attr3 tattr.h5
TEST tattr-3.ddl -header -a attr2 tattr.h5
TEST tattr-4.ddl -a attr4 tattr.h5

TEST tslink-1.ddl tslink.h5
TEST tslink-2.ddl -l slink2 tslink.h5

TEST thlink-1.ddl thlink.h5
TEST thlink-2.ddl -d /g1/link2 /dset /g1/link1/link3 thlink.h5
TEST thlink-3.ddl -d /dset /g1/link1/link3 /g1/link2 thlink.h5
TEST thlink-4.ddl -g /g1 thlink.h5
TEST thlink-5.ddl -d /dset -g /g2 -d /g1/link2 thlink.h5

TEST tcomp-1.ddl tcompound.h5
TEST tcomp-2.ddl -t /type1 /type2 /group1/type3 tcompound.h5
TEST tcomp-3.ddl -d /group2/dset5 -g /group1 tcompound.h5
TEST tcomp-4.ddl -t /#3432:0 -g /group2 tcompound.h5

TEST tall-1.ddl tall.h5
TEST tall-2.ddl -header -g /g1/g1.1 -a attr2 tall.h5
TEST tall-3.ddl -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5

else
MESG 3 "$TestName <<<SKIPPED>>>"
fi

# End of test
FINISH
