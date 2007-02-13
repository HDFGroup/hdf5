#############################
Expected output for 'h5ls ../testfiles/h5copytst.b.out.h5'
#############################
Opened "../testfiles/h5copytst.b.out.h5" with sec2 driver.
/chunk                   Dataset {6/6}
    Location:  1:6216
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Chunks:    {2} 8 bytes
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
/compact                 Dataset {6/6}
    Location:  1:6344
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
/compound                Dataset {2/2}
    Location:  1:8528
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   80 logical bytes, 80 allocated bytes, 100.00% utilization
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/compressed              Dataset {6/6}
    Location:  1:12888
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Chunks:    {2} 8 bytes
    Storage:   24 logical bytes, 42 allocated bytes, 57.14% utilization
    Filter-0:  deflate-1 OPT {1}
    Type:      native int
/grp_dsets               Group
    Location:  1:31584
    Links:     1
/grp_dsets/chunk         Dataset {6/6}
    Location:  1:33720
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Chunks:    {2} 8 bytes
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
/grp_dsets/compact       Dataset {6/6}
    Location:  1:34176
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
/grp_dsets/compound      Dataset {2/2}
    Location:  1:34312
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   80 logical bytes, 80 allocated bytes, 100.00% utilization
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_dsets/compressed    Dataset {6/6}
    Location:  1:34528
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Chunks:    {2} 8 bytes
    Storage:   24 logical bytes, 42 allocated bytes, 57.14% utilization
    Filter-0:  deflate-1 OPT {1}
    Type:      native int
/grp_dsets/named_vl      Dataset {2/2}
    Location:  1:34744
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   16 logical bytes, 32 allocated bytes, 50.00% utilization
    Type:      shared-1:34696 variable length of
                   native int
/grp_dsets/nested_vl     Dataset {2/2}
    Location:  1:43216
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   16 logical bytes, 32 allocated bytes, 50.00% utilization
    Type:      variable length of
                   variable length of
                       native int
/grp_dsets/simple        Dataset {6/6}
    Location:  1:43360
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
/grp_dsets/vl            Type
    Location:  1:34696
    Links:     2
    Type:      shared-1:34696 variable length of
                   native int
/grp_empty               Group
    Location:  1:30464
    Links:     1
/grp_rename              Group
    Location:  1:44240
    Links:     1
/grp_rename/chunk        Dataset {6/6}
    Location:  1:46376
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Chunks:    {2} 8 bytes
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
/grp_rename/compact      Dataset {6/6}
    Location:  1:46832
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
/grp_rename/compound     Dataset {2/2}
    Location:  1:46968
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   80 logical bytes, 80 allocated bytes, 100.00% utilization
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_rename/compressed   Dataset {6/6}
    Location:  1:47184
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Chunks:    {2} 8 bytes
    Storage:   24 logical bytes, 42 allocated bytes, 57.14% utilization
    Filter-0:  deflate-1 OPT {1}
    Type:      native int
/grp_rename/named_vl     Dataset {2/2}
    Location:  1:47400
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   16 logical bytes, 32 allocated bytes, 50.00% utilization
    Type:      shared-1:47352 variable length of
                   native int
/grp_rename/nested_vl    Dataset {2/2}
    Location:  1:55872
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   16 logical bytes, 32 allocated bytes, 50.00% utilization
    Type:      variable length of
                   variable length of
                       native int
/grp_rename/simple       Dataset {6/6}
    Location:  1:56016
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
/grp_rename/vl           Type
    Location:  1:47352
    Links:     2
    Type:      shared-1:47352 variable length of
                   native int
/named_vl                Dataset {2/2}
    Location:  1:13104
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   16 logical bytes, 32 allocated bytes, 50.00% utilization
    Type:      shared-1:13056 variable length of
                   native int
/nested_vl               Dataset {2/2}
    Location:  1:27392
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   16 logical bytes, 32 allocated bytes, 50.00% utilization
    Type:      variable length of
                   variable length of
                       native int
/rename                  Dataset {2/2}
    Location:  1:29584
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   80 logical bytes, 80 allocated bytes, 100.00% utilization
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/simple                  Dataset {6/6}
    Location:  1:808
    Links:     1
    Modified:  2007-02-13 18:36:15 CST
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
