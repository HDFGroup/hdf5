#############################
Expected output for 'h5ls ./testfiles/h5copy_extlinks_src.out.h5'
#############################
Opened "./testfiles/h5copy_extlinks_src.out.h5" with sec2 driver.
/                        Group
    Location:  1:96
    Links:     1
/copy1_dset              Dataset {6/6}
    Location:  1:800
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/copy1_group             Group
    Location:  1:4344
    Links:     1
/copy1_group/extlink_datatype External Link {h5copy_extlinks_trg.h5//datatype}
/copy1_group/extlink_dset External Link {h5copy_extlinks_trg.h5//simple}
/copy1_group/extlink_grp External Link {h5copy_extlinks_trg.h5//group}
/copy1_group/extlink_notyet1 External Link {h5copy_extlinks_trg.h5//notyet}
/copy1_group/extlink_notyet2 External Link {notyet_file.h5//notyet}
/copy2_dset              Dataset {6/6}
    Location:  1:4216
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/copy2_group             Group
    Location:  1:4712
    Links:     1
/copy2_group/extlink_datatype Type
    Location:  1:5912
    Links:     1
    Type:      shared-1:5912 32-bit little-endian integer
/copy2_group/extlink_dset Dataset {6/6}
    Location:  1:5080
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/copy2_group/extlink_grp Group
    Location:  1:5872
    Links:     1
/copy2_group/extlink_notyet1 External Link {h5copy_extlinks_trg.h5//notyet}
/copy2_group/extlink_notyet2 External Link {notyet_file.h5//notyet}
