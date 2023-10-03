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
    Location:  1:4600
    Links:     1
/copy1_group/extlink_datatype External Link {h5copy_extlinks_trg.h5//datatype}
/copy1_group/extlink_dset External Link {h5copy_extlinks_trg.h5//simple}
/copy1_group/extlink_grp External Link {h5copy_extlinks_trg.h5//group}
/copy1_group/extlink_notyet1 External Link {h5copy_extlinks_trg.h5//notyet}
/copy1_group/extlink_notyet2 External Link {notyet_file.h5//notyet}
/copy2_dset              Dataset {6/6}
    Location:  1:4120
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/copy2_group             Group
    Location:  1:5032
    Links:     1
/copy2_group/extlink_datatype Type
    Location:  1:6232
    Links:     1
    Type:      shared-1:6232 32-bit little-endian integer
/copy2_group/extlink_dset Dataset {6/6}
    Location:  1:5400
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/copy2_group/extlink_grp Group
    Location:  1:6192
    Links:     1
/copy2_group/extlink_notyet1 External Link {h5copy_extlinks_trg.h5//notyet}
/copy2_group/extlink_notyet2 External Link {notyet_file.h5//notyet}
/copy_dangle1_1          External Link {h5copy_extlinks_trg.h5//notyet}
/copy_dangle1_2          External Link {h5copy_extlinks_trg.h5//notyet}
/copy_dangle2_1          External Link {notyet_file.h5//notyet}
/copy_dangle2_2          External Link {notyet_file.h5//notyet}
