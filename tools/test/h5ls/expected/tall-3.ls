Opened "tall.h5" with sec2 driver.
/                        Group
    Attribute: attr1 {10}
        Type:      8-bit integer
        Data:
               97, 98, 99, 100, 101, 102, 103, 104, 105, 0
    Attribute: attr2 {2, 2}
        Type:      32-bit big-endian integer
        Data:
               0, 1, 2, 3
    Location:  1:96
    Links:     1
/g1                      Group
    Location:  1:800
    Links:     1
/g1/g1.1                 Group
    Location:  1:2536
    Links:     1
/g1/g1.1/dset1.1.1       Dataset {10/10, 10/10}
    Attribute: attr1 {27}
        Type:      8-bit integer
        Data:
               49, 115, 116, 32, 97, 116, 116, 114, 105, 98, 117, 116, 101,
               32, 111, 102, 32, 100, 115, 101, 116, 49, 46, 49, 46, 49, 0
    Attribute: attr2 {27}
        Type:      8-bit integer
        Data:
               50, 110, 100, 32, 97, 116, 116, 114, 105, 98, 117, 116, 101,
               32, 111, 102, 32, 100, 115, 101, 116, 49, 46, 49, 46, 49, 0
    Location:  1:5480
    Links:     1
    Storage:   400 logical bytes, 400 allocated bytes, 100.00% utilization
    Type:      32-bit big-endian integer
    Address:   6224
    Data:
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 2, 4, 6,
         8, 10, 12, 14, 16, 18, 0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 0, 4, 8, 12,
         16, 20, 24, 28, 32, 36, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 0, 6, 12,
         18, 24, 30, 36, 42, 48, 54, 0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 0, 8,
         16, 24, 32, 40, 48, 56, 64, 72, 0, 9, 18, 27, 36, 45, 54, 63, 72, 81
/g1/g1.1/dset1.1.2       Dataset {20/20}
    Location:  1:8272
    Links:     1
    Storage:   80 logical bytes, 80 allocated bytes, 100.00% utilization
    Type:      32-bit big-endian integer
    Address:   6624
    Data:
         0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19
/g1/g1.2                 Group
    Location:  1:3568
    Links:     1
/g1/g1.2/extlink         External Link {somefile//somepath}
/g1/g1.2/g1.2.1          Group
    Location:  1:4272
    Links:     1
/g1/g1.2/g1.2.1/slink    Soft Link {somevalue}
/g2                      Group
    Location:  1:1832
    Links:     1
/g2/dset2.1              Dataset {10/10}
    Location:  1:8960
    Links:     1
    Storage:   40 logical bytes, 40 allocated bytes, 100.00% utilization
    Type:      IEEE 32-bit big-endian float
    Address:   6704
    Data:
         1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9
/g2/dset2.2              Dataset {3/3, 5/5}
    Location:  1:9560
    Links:     1
    Storage:   60 logical bytes, 60 allocated bytes, 100.00% utilization
    Type:      IEEE 32-bit big-endian float
    Address:   6744
    Data:
         0, 0.1, 0.2, 0.3, 0.4, 0, 0.2, 0.4, 0.6, 0.8, 0, 0.3, 0.6, 0.9, 1.2
/g2/udlink               UD Link {cannot follow UD links}
