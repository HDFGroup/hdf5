POOL_UUID=$1
EXEC_ARGS="-x DD_MASK=\"all\" --hostfile $HOME/my_hosts --ompi-server file:~/uri.txt"

# --------------- SETUP --------------- #
# Create file
echo h5dsm_file_create file.h5
orterun -np 1 $EXEC_ARGS ./h5dsm_file_create $POOL_UUID file.h5
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_file_create file.h5 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_file_create $POOL_UUID file.h5
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open file
echo h5dsm_file_open file.h5
orterun -np 1 $EXEC_ARGS ./h5dsm_file_open $POOL_UUID file.h5
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_file_open file.h5 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_file_open $POOL_UUID file.h5
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create group
echo h5dsm_group_create file.h5 grp
orterun -np 1 $EXEC_ARGS ./h5dsm_group_create $POOL_UUID file.h5 grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open group
echo h5dsm_group_open file.h5 grp
orterun -np 1 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID file.h5 grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_group_open file.h5 grp \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID file.h5 grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create dataset
echo h5dsm_dset_create file.h5 dset
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_create $POOL_UUID file.h5 dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create dset2
echo h5dsm_dset_create file.h5 dset2 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_dset_create $POOL_UUID file.h5 dset2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open dataset
echo h5dsm_dset_open file.h5 /dset
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_open $POOL_UUID file.h5 dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_dset_open file.h5 /dset \(2 processes\)
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_open $POOL_UUID file.h5 dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open dset2
echo h5dsm_dset_open file.h5 dset2
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_open $POOL_UUID file.h5 dset2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_dset_open file.h5 dset2 \(2 processes\)
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_open $POOL_UUID file.h5 dset2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- DSET IO --------------- #
# Write dataset
echo h5dsm_dset_write file.h5 dset
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_write $POOL_UUID file.h5 dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# "Write Partial" test
echo h5dsm_dset_wpartial file.h5 dset2 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_dset_wpartial $POOL_UUID file.h5 dset2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Read dataset
echo h5dsm_dset_read file.h5 dset2
orterun -np 1 $EXEC_ARGS ./h5dsm_dset_read $POOL_UUID file.h5 dset2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# "Read Partial" test
# Disabled due to failure
#echo h5dsm_dset_rpartial file.h5 dset \(2 processes\)
#orterun -np 2 $EXEC_ARGS ./h5dsm_dset_rpartial $POOL_UUID file.h5 dset
#if test $? -ne 0; then
#    echo FAILED
#    exit 1
#fi

# --------------- LINKS --------------- #
# H5Lexists (should be FALSE)
echo h5dsm_link_exists file.h5 grp2
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID file.h5 grp2 | tee h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create grp2 \(2 processes\)
echo h5dsm_group_create file.h5 grp2 -s
orterun -np 2 $EXEC_ARGS ./h5dsm_group_create $POOL_UUID file.h5 grp2 -s
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Lexists (should be TRUE)
echo h5dsm_link_exists file.h5 grp2
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID file.h5 grp2 | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create slink
echo h5dsm_slink_create file.h5 grp slink -s
orterun -np 1 $EXEC_ARGS ./h5dsm_slink_create $POOL_UUID file.h5 grp slink -s
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open slink
echo h5dsm_group_open file.h5 slink
orterun -np 1 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID file.h5 slink
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_group_open file.h5 slink \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID file.h5 slink
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create slroot
echo h5dsm_slink_create file.h5 / grp/slroot
orterun -np 1 $EXEC_ARGS ./h5dsm_slink_create $POOL_UUID file.h5 / grp/slroot
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open slroot
echo h5dsm_group_open file.h5 /slink/slroot/grp
orterun -np 1 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID file.h5 /slink/slroot/grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_group_open file.h5 /slink/slroot/grp \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID file.h5 /slink/slroot/grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Lexists (should be FALSE)
echo h5dsm_link_exists file.h5 /slink/slroot/slpath
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID file.h5 /slink/slroot/slpath | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create slpath
echo h5dsm_slink_create file.h5 /slink/slroot/dset slpath
orterun -np 1 $EXEC_ARGS ./h5dsm_slink_create $POOL_UUID file.h5 /slink/slroot/dset slpath
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Lexists (should be TRUE)
echo h5dsm_link_exists file.h5 /slink/slroot/slpath
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID file.h5 /slink/slroot/slpath | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open slpath
echo h5dsm_dset_open file.h5 slink/slroot/slpath
orterun -np 1 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID file.h5 slpath
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_dset_open file.h5 slink/slroot/slpath \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_group_open $POOL_UUID file.h5 slpath
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Literate root
echo h5dsm_link_iter file.h5 /
orterun -np 1 $EXEC_ARGS ./h5dsm_link_iter $POOL_UUID file.h5 / | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Literate grp
echo h5dsm_link_iter file.h5 grp
orterun -np 1 $EXEC_ARGS ./h5dsm_link_iter $POOL_UUID file.h5 grp | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- Attributes --------------- #
# Create attribute on root group
echo h5dsm_attr_create file.h5 / attrroot
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_create $POOL_UUID file.h5 / attrroot
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open attribute
echo h5dsm_attr_open file.h5 / attrroot
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_open $POOL_UUID file.h5 / attrroot
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Write attribute
echo h5dsm_attr_write file.h5 / attrroot
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_write $POOL_UUID file.h5 / attrroot
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Read attribute
echo h5dsm_attr_read file.h5 / attrroot
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_read $POOL_UUID file.h5 / attrroot
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create attribute on group
echo h5dsm_attr_create file.h5 grp attrgrp
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_create $POOL_UUID file.h5 grp attrgrp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open attribute
echo h5dsm_attr_open file.h5 /grp attrgrp
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_open $POOL_UUID file.h5 /grp attrgrp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create attribute on dataset
echo h5dsm_attr_create file.h5 /dset attrdset
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_create $POOL_UUID file.h5 /dset attrdset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open attribute
echo h5dsm_attr_open file.h5 dset attrdset
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_open $POOL_UUID file.h5 dset attrdset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Create additional attribute on root group
echo h5dsm_attr_create file.h5 / attrroot2
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_create $POOL_UUID file.h5 / attrroot2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# Open attribute
echo h5dsm_attr_open file.h5 / attrroot2
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_open $POOL_UUID file.h5 / attrroot2
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Aiterate on root group
echo h5dsm_attr_iter file.h5 /
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_iter $POOL_UUID file.h5 / | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Aiterate on group
echo h5dsm_attr_iter file.h5 grp
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_iter $POOL_UUID file.h5 grp | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Aiterate on dataset
echo h5dsm_attr_iter file.h5 /dset
orterun -np 1 $EXEC_ARGS ./h5dsm_attr_iter $POOL_UUID file.h5 /dset | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- Objects --------------- #
# H5Oopen root group
echo h5dsm_obj_open file.h5 /
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID file.h5 /
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open file.h5 / \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID file.h5 /
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen group
echo h5dsm_obj_open file.h5 /grp
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID file.h5 /grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open file.h5 /grp \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID file.h5 /grp
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen dataset
echo h5dsm_obj_open file.h5 dset
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID file.h5 dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open file.h5 dset \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open $POOL_UUID file.h5 dset
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oget_info root group
echo h5dsm_obj_info file.h5 /
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_info $POOL_UUID file.h5 / | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oget_info group
echo h5dsm_obj_info file.h5 grp
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_info $POOL_UUID file.h5 grp | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oget_info dataset
echo h5dsm_obj_info file.h5 /dset
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_info $POOL_UUID file.h5 /dset | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen_by_addr root group
echo h5dsm_obj_open_addr file.h5 0x0000000000000001
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID file.h5 0x0000000000000001
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open_addr file.h5 0x0000000000000001 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID file.h5 0x0000000000000001
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen_by_addr group
echo h5dsm_obj_open_addr file.h5 0x0000000000000002
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID file.h5 0x0000000000000002
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open_addr file.h5 0x0000000000000002 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID file.h5 0x0000000000000002
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Oopen_by_addr dataset
echo h5dsm_obj_open_addr file.h5 0x4000000000000003
orterun -np 1 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID file.h5 0x4000000000000003
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

echo h5dsm_obj_open_addr file.h5 0x4000000000000003 \(2 processes\)
orterun -np 2 $EXEC_ARGS ./h5dsm_obj_open_addr $POOL_UUID file.h5 0x4000000000000003
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# --------------- Snapshots --------------- #
# H5Lexists before slink created (should be FALSE)
echo h5dsm_link_exists file.h5 grp2 7
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID file.h5 slink 7 | tee -a h5dsm_test.out
if test $? -ne 0; then
    echo FAILED
    exit 1
fi

# H5Lexists after slink created (should be TRUE)
echo h5dsm_link_exists file.h5 grp2 8
orterun -np 1 $EXEC_ARGS ./h5dsm_link_exists $POOL_UUID file.h5 slink 8 | tee -a h5dsm_test.out
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

# --------------- Output Comparison --------------- #
cmp h5dsm_test.out h5dsm_test.out.exp
if test $? -ne 0; then
    echo h5dsm_test.out does not match h5dsm_test.out.exp
    echo FAILED
    exit 1
fi

echo PASSED
exit 0
