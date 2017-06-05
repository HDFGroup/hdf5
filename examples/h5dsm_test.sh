POOL_UUID=$1
EXEC_ARGS="-x DD_MASK=\"all\" --ompi-server file:~/uri.txt"
FILE="h5dsm_test.h5"

# --------------- SETUP --------------- #
# Create file
echo h5dsm_file_create $FILE
orterun -np 1 $EXEC_ARGS ./h5dsm_file_create $POOL_UUID $FILE
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_file_create $FILE \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_file_create $POOL_UUID $FILE
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open file
echo h5dsm_file_open $FILE
orterun -np 1 $EXEC_ARGS ./h5dsm_file_open $POOL_UUID $FILE
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_file_open $FILE \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_file_open $POOL_UUID $FILE
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create group
echo h5dsm_group_create $FILE grp
orterun -np 1 $EXEC_ARGS ./h5dsm_group_create $POOL_UUID $FILE grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open group
echo h5dsm_group_open $FILE grp
orterun -np 1 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID $FILE grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_group_open $FILE grp \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID $FILE grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create dataset
echo h5dsm_dset_create $FILE dset
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_create $POOL_UUID $FILE dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create dset2
echo h5dsm_dset_create $FILE dset2 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_dset_create $POOL_UUID $FILE dset2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open dataset
echo h5dsm_dset_open $FILE /dset
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_open $POOL_UUID $FILE dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_dset_open $FILE /dset \(2 processes\)
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_open $POOL_UUID $FILE dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open dset2
echo h5dsm_dset_open $FILE dset2
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_open $POOL_UUID $FILE dset2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_dset_open $FILE dset2 \(2 processes\)
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_open $POOL_UUID $FILE dset2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- DSET IO --------------- #
# Write dataset
echo h5dsm_dset_write $FILE dset
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_write $POOL_UUID $FILE dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# "Write Partial" test
echo h5dsm_dset_wpartial $FILE dset2 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_dset_wpartial $POOL_UUID $FILE dset2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Read dataset
echo h5dsm_dset_read $FILE dset2
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_read $POOL_UUID $FILE dset2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# "Read Partial" test
echo h5dsm_dset_rpartial $FILE dset \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_dset_rpartial $POOL_UUID $FILE dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- MAPS --------------- #
echo h5dsm_map $FILE
orterun -np 1 $EXEC_ARGS ./h5dsm_map $POOL_UUID $FILE
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- LINKS --------------- #
# H5Lexists (should be FALSE)
echo h5dsm_link_exists $FILE grp2
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID $FILE grp2 | tee h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create grp2 \(2 processes\)
echo h5dsm_group_create $FILE grp2 -s
orterun -np 2 $EXEC_ARGS ./h5dsm_group_create $POOL_UUID $FILE grp2 -s
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Lexists (should be TRUE)
echo h5dsm_link_exists $FILE grp2
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID $FILE grp2 | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create slink
echo h5dsm_slink_create $FILE grp slink -s
orterun -np 1 $EXEC_ARGS ./h5dsm_slink_create $POOL_UUID $FILE grp slink -s
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open slink
echo h5dsm_group_open $FILE slink
orterun -np 1 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID $FILE slink
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_group_open $FILE slink \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID $FILE slink
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create slroot
echo h5dsm_slink_create $FILE / grp/slroot
orterun -np 1 $EXEC_ARGS ./h5dsm_slink_create $POOL_UUID $FILE / grp/slroot
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open slroot
echo h5dsm_group_open $FILE /slink/slroot/grp
orterun -np 1 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID $FILE /slink/slroot/grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_group_open $FILE /slink/slroot/grp \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID $FILE /slink/slroot/grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Lexists (should be FALSE)
echo h5dsm_link_exists $FILE /slink/slroot/slpath
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID $FILE /slink/slroot/slpath | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create slpath
echo h5dsm_slink_create $FILE /slink/slroot/dset slpath
orterun -np 1 $EXEC_ARGS ./h5dsm_slink_create $POOL_UUID $FILE /slink/slroot/dset slpath
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Lexists (should be TRUE)
echo h5dsm_link_exists $FILE /slink/slroot/slpath
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID $FILE /slink/slroot/slpath | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open slpath
echo h5dsm_dset_open $FILE slink/slroot/slpath
orterun -np 1 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID $FILE slpath
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_dset_open $FILE slink/slroot/slpath \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID $FILE slpath
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Literate root
echo h5dsm_link_iter $FILE /
orterun -np 1 $EXEC_ARGS ./h5dsm_link_iter $POOL_UUID $FILE / | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Literate grp
echo h5dsm_link_iter $FILE grp
orterun -np 1 $EXEC_ARGS ./h5dsm_link_iter $POOL_UUID $FILE grp | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- Attributes --------------- #
# Create attribute on root group
echo h5dsm_attr_create $FILE / attrroot
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_create $POOL_UUID $FILE / attrroot
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open attribute
echo h5dsm_attr_open $FILE / attrroot
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_open $POOL_UUID $FILE / attrroot
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Write attribute
echo h5dsm_attr_write $FILE / attrroot
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_write $POOL_UUID $FILE / attrroot
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Read attribute
echo h5dsm_attr_read $FILE / attrroot
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_read $POOL_UUID $FILE / attrroot
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create attribute on group
echo h5dsm_attr_create $FILE grp attrgrp
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_create $POOL_UUID $FILE grp attrgrp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open attribute
echo h5dsm_attr_open $FILE /grp attrgrp
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_open $POOL_UUID $FILE /grp attrgrp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create attribute on dataset
echo h5dsm_attr_create $FILE /dset attrdset
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_create $POOL_UUID $FILE /dset attrdset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open attribute
echo h5dsm_attr_open $FILE dset attrdset
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_open $POOL_UUID $FILE dset attrdset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create additional attribute on root group
echo h5dsm_attr_create $FILE / attrroot2
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_create $POOL_UUID $FILE / attrroot2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open attribute
echo h5dsm_attr_open $FILE / attrroot2
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_open $POOL_UUID $FILE / attrroot2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Aiterate on root group
echo h5dsm_attr_iter $FILE /
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_iter $POOL_UUID $FILE / | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Aiterate on group
echo h5dsm_attr_iter $FILE grp
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_iter $POOL_UUID $FILE grp | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Aiterate on dataset
echo h5dsm_attr_iter $FILE /dset
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_iter $POOL_UUID $FILE /dset | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- Objects --------------- #
# H5Oopen root group
echo h5dsm_obj_open $FILE /
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID $FILE /
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open $FILE / \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID $FILE /
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen group
echo h5dsm_obj_open $FILE /grp
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID $FILE /grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open $FILE /grp \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID $FILE /grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen dataset
echo h5dsm_obj_open $FILE dset
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID $FILE dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open $FILE dset \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID $FILE dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen map
echo h5dsm_obj_open $FILE MAP_VL_T
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID $FILE MAP_VL_T
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open $FILE MAP_VL_T \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID $FILE MAP_VL_T
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oget_info root group
echo h5dsm_obj_info $FILE /
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_info $POOL_UUID $FILE / | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oget_info group
echo h5dsm_obj_info $FILE grp
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_info $POOL_UUID $FILE grp | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oget_info dataset
echo h5dsm_obj_info $FILE /dset
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_info $POOL_UUID $FILE /dset | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oget_info map
echo h5dsm_obj_info $FILE /MAP_VL_T
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_info $POOL_UUID $FILE /MAP_VL_T | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen_by_addr root group
echo h5dsm_obj_open_addr $FILE 0x0000000000000001
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID $FILE 0x0000000000000001
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open_addr $FILE 0x0000000000000001 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID $FILE 0x0000000000000001
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen_by_addr group
echo h5dsm_obj_open_addr $FILE 0x0000000000000002
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID $FILE 0x0000000000000002
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open_addr $FILE 0x0000000000000002 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID $FILE 0x0000000000000002
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen_by_addr dataset
echo h5dsm_obj_open_addr $FILE 0x4000000000000003
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID $FILE 0x4000000000000003
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open_addr $FILE 0x4000000000000003 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID $FILE 0x4000000000000003
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen_by_addr dataset
echo h5dsm_obj_open_addr $FILE 0xc000000000000005
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID $FILE 0xc000000000000005
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open_addr $FILE 0xc000000000000005 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID $FILE 0xc000000000000005
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- Snapshots --------------- #
# H5Lexists before slink created (should be FALSE)
echo h5dsm_link_exists $FILE grp2 9
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID $FILE slink 9 | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Lexists after slink created (should be TRUE)
echo h5dsm_link_exists $FILE grp2 10
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID $FILE slink 10 | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- Type Conversion --------------- #
echo h5dsm_ttconv
orterun -np 1 $EXEC_ARGS ./h5dsm_ttconv $POOL_UUID -q
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- Variable Length Types --------------- #
echo h5dsm_tvlen
orterun -np 1 $EXEC_ARGS ./h5dsm_tvlen $POOL_UUID -q
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- Committed Datatypes --------------- #
echo h5dsm_tcommit
orterun -np 1 $EXEC_ARGS ./h5dsm_tcommit $POOL_UUID -q
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_tcommit \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_tcommit $POOL_UUID -q
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- Output Comparison --------------- #
sed -i -e 's/#.*//' -e 's/[ ^I]*$//' -e '/^$/ d' h5dsm_test.out
echo cmp h5dsm_test.out h5dsm_test.out.exp
cmp h5dsm_test.out h5dsm_test.out.exp
if test $? -ne 0; then
    echo h5dsm_test.out does not match h5dsm_test.out.exp
    echo FAILED
    exit 1
fi

echo PASSED
exit 0
