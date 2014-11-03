#! /bin/sh
# (Use -e to abort at any unexpected error.)
# Submit daily tests of HDF5 base software
# Usauge: DailyHDFTests [-r<version>]
# Example: DailyHDFTests        # test the CVS current version
#          DailyHDFTests -r1.2  # test version 1.2
#          DailyHDFTests -r1.4  # test version 1.4

# This script assumes we are building binaries for a release in 
# a directory named $HOME/snapshots-bin-${sw}${SWVERSTR}, i.e.
# /home/hdftest/snapshots-bin-hdf5_1_8_14 for the HDF5 v1.8.14 release.

# This script should be run from the $HOME/snapshots-bin-${sw}${SWVERSTR} 
# directory with the command 
# "sh current/bin/build_and_package_hdf5_binaries.sh -r<version> -b<path to package dir>" 

# general setup
PROGRAMNAME=`basename $0`
DEBUGMODE=""            # change to -debug for debug mode

# Setup
HOSTNAME=`hostname | cut -f1 -d.`       # no domain part
TODAY=`date +%m%d%a`
SW=HDF5                 # Name of Software.
sw=hdf5                 # Name of Software in lower cases.
SWVER=                  # Software version, default to current CVS version.
SWVERSTR=               # Software version string.
TITLE="${SW} Tests on $TODAY"
errcode=0               # error code so far

# Parse command options
while [ $# -gt 0 ]; do
    case "$1" in
        -r*)
            SWVER="$1"
            SWVERSTR=_`echo $SWVER | sed -e s/-r// -e s/\\\./_/g`
            ;;
        -debug*)
            DEBUGMODE="$1"
            ;;
        -b*)
            BINARYDIR=`echo $1 | sed -e s/-b//`
            ;;
        *)
            echo "Unknown option ($1)"
            exit 1
            ;;
    esac
    shift
done

# Mail addresses for reports
# TOWHOM_PASSED--normal report
# TOWHOM_FAILED--Failure Report
# If in debug mode, email output to myself; else to the "group".
if [ -n "$DEBUGMODE" ]; then
    echo "******** DEBUGMODE is $DEBUGMODE ************"
#    TOWHOM_PASSED=hdftest
#    TOWHOM_FAILED=hdftest
    TOWHOM_PASSED=lrknox
    TOWHOM_FAILED=lrknox
else
#       TOWHOM_PASSED="hdf5repo@lists.hdfgroup.org repo-all@lists.hdfgroup.org"
#       TOWHOM_FAILED=hdf5lib@lists.hdfgroup.org
#    TOWHOM_PASSED=hdftest
#    TOWHOM_FAILED=hdftest
    TOWHOM_PASSED=lrknox
    TOWHOM_FAILED=lrknox
fi

# Setup test directories
LOGDIR=$HOME/snapshots-bin-${sw}${SWVERSTR}/log
LOGDIRLOCK=${LOGDIR}/.lock-${HOSTNAME}
LOGFILE=${LOGDIR}/${PROGRAMNAME}-${HOSTNAME}_${TODAY}
FAILEDLOG=${LOGDIR}/FAILED_LOG_${TODAY}
FAILEDDETAIL=${LOGDIR}/FAILED_DETAIL_${TODAY}
INCOMPLETELOG=${LOGDIR}/INCOMPLETE_LOG_${TODAY}
PASSEDLOG=${LOGDIR}/PASSED_LOG_${TODAY}
SKIPPEDLOG=${LOGDIR}/SKIPPED_LOG_${TODAY}
TIMELOG=${LOGDIR}/TIME_LOG_${TODAY}
TIMEKEEPERLOG=${LOGDIR}/TIMEKEEPER_LOG_${TODAY}
REPORTED=${LOGDIR}/REPORTED_${TODAY}
REPOLOG=${LOGDIR}/REPO_LOG_${TODAY}
REPOLOG_LOCK=${LOGDIR}/REPO_LOG_LOCK_${TODAY}
SNAPSHOTLOG=${LOGDIR}/SNAPSHOT_LOG_${TODAY}
RELEASE_ASAP=${LOGDIR}/../release_asap          # make a special release asap
RELEASE_ALWAYS=${LOGDIR}/../release_always      # always release
RELEASE_NOT=${LOGDIR}/../release_not            # Do not release
RELEASED=${LOGDIR}/RELEASED_${TODAY}
WATCHERFILE=${LOGDIR}/../watchers               # List of platform watchers
CMD="bin/runtest $DEBUGMODE ${SWVER}"

# Setup Mailing command and Report title
MAIL=mail
TEST_TITLE="$HOSTNAME ${SW}${SWVERSTR}_Daily_Tests_${TODAY}"

# set up auto-cleanup
trap "rm -f $LOGDIRLOCK" 0
trap "rm -f $LOGDIRLOCK" 1 2 3 4 5 6 7 8 9 10 12 13 14 15

# Clean up LOGDIR by moving all files that are not "of today" to OLD.
# Then go into OLD and move all files older than 7 days to OLDER.
# Then go into OLDer and remove all files older than 30 days.
# This procedure keeps old logfiles in two tiers.  The first tier is OLD
# which should be in AFS space so that the files are available to most hosts.
# The second tier is OLDER which could be in HDF's own NFS space which has
# much bigger capacity but not as widely available.
# Do all these in a sub-shell with -e to abort the cleaning if any error is
# encountered.
(set -e
nold=6
nolder=31
echo cleaning $LOGDIR by moving all old logfiles to OLD
cd $LOGDIR
test -d OLD/. || mkdir OLD
#find . \( -name OLD -prune \) \
#    -o -type f ! -name '*'$TODAY'*' -exec mv '{}' OLD/. \;

echo cleaning $LOGDIR/OLD by moving all files older than $nold days to OLDER
cd OLD
test -d OLDER/. || mkdir OLDER
find . \( -name OLDER -prune \) \
    -o -type f -mtime +$nold -exec mv '{}' OLDER/. \;

echo cleaning $LOGDIR/OLDER by removing all files older than $nolder days.
cd OLDER
find . -type f -mtime +$nolder -exec rm -f '{}' \;

if [ -f $LOGFILE ]; then
    if [ -n "$DEBUGMODE" ]; then
        echo "$LOGFILE exists.  No more daily tests today"
    fi
    exit 1
fi
)

#(cd $HOME/snapshots-bin-${sw}${SWVERSTR}; touch $REPOLOG; $CMD -nodiff -norepo -all) > $LOGFILE 2>&1

# Verify test script did complete by checking the last lines
#(tail -4 $LOGFILE | grep -s "^*** finished .* in $HOSTNAME ***" > /dev/null 2>&1) ||
#    (echo "****snaptest launcher FAILED to complete in $HOSTNAME****" >> $FAILEDLOG)

#CURRENT_DIR=`pwd`
#cd $HOME/snapshots-bin-${sw}${SWVERSTR}/current
#pwd
#SVN_URL=`svn info | grep URL`
#BRANCHNAME=`echo $SVN_URL | sed -e 's/URL:.*\///' | sed -e 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'`
#REVISION=`svn info | grep Revision`
#LAST_INFO=`svn info | grep "Last Changed"`
#TEST_TITLE="$HOSTNAME ${BRANCHNAME}${SWVERSTR}_Daily_Tests_${TODAY}"
#TITLE="${BRANCHNAME} Tests on $TODAY"
#cd $CURRENT_DIR

# Check result
if [ -f $FAILEDLOG ]; then
    errcode=1
elif [ -f $INCOMPLETELOG ]; then
    errcode=1
    echo "Not all builds were completed!"
fi

if [ $errcode -eq 0 ]; then
    DEPLOYDIR=`grep "deploydir \/mnt" ./snaptest.cfg | sed "s/^.*deploydir //"`
    echo "Make binary tar files from deployed files in $DEPLOYDIR in $BINARYDIR" 
    CURRENT_DIR=`pwd`
    cd current/bin;perl ./make1814TarFiles.pl $DEPLOYDIR $BINARYDIR
    cd $CURRENT_DIR
else 
    echo "errcode was $errcode;  no tar files were created."
fi

# remove lock
rm -f $LOGDIRLOCK

# test binaries on 3 linux binary test machines.


# final exit
exit $errcode

