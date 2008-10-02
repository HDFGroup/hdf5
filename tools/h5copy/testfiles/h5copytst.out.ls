#############################
Expected output for 'h5ls ../testfiles/h5copytst.out.h5'
#############################
Opened "../testfiles/h5copytst.out.h5" with sec2 driver.
/                        Group
    Location:  1:96
    Links:     1
/A                       Group
    Location:  1:89936
    Links:     1
/A/B1                    Group
    Location:  1:90640
    Links:     1
/A/B1/simple             Dataset {6/6}
    Location:  1:89808
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/A/B2                    Group
    Location:  1:94176
    Links:     1
/A/B2/simple2            Dataset {6/6}
    Location:  1:94048
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/C                       Group
    Location:  1:97384
    Links:     1
/C/D                     Group
    Location:  1:98088
    Links:     1
/C/D/simple              Dataset {6/6}
    Location:  1:97256
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E                       Group
    Location:  1:112000
    Links:     1
/E/F                     Group
    Location:  1:112704
    Links:     1
/E/F/grp_dsets           Group
    Location:  1:100200
    Links:     1
/E/F/grp_dsets/chunk     Dataset {6/6}
    Location:  1:104384
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/compact   Dataset {6/6}
    Location:  1:104840
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/compound  Dataset {2/2}
    Location:  1:104976
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/E/F/grp_dsets/compressed Dataset {6/6}
    Location:  1:107288
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/E/F/grp_dsets/named_vl  Dataset {2/2}
    Location:  1:111600
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:107456 variable length of
                   32-bit little-endian integer
/E/F/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:111728
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/E/F/grp_dsets/simple    Dataset {6/6}
    Location:  1:111872
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/vl        Type
    Location:  1:107456
    Links:     2
    Type:      shared-1:107456 variable length of
                   32-bit little-endian integer
/G                       Group
    Location:  1:127648
    Links:     1
/G/H                     Group
    Location:  1:128352
    Links:     1
/G/H/grp_nested          Group
    Location:  1:114728
    Links:     1
/G/H/grp_nested/grp_dsets Group
    Location:  1:115520
    Links:     1
/G/H/grp_nested/grp_dsets/chunk Dataset {6/6}
    Location:  1:119704
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/compact Dataset {6/6}
    Location:  1:120160
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/compound Dataset {2/2}
    Location:  1:120296
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/G/H/grp_nested/grp_dsets/compressed Dataset {6/6}
    Location:  1:122608
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/named_vl Dataset {2/2}
    Location:  1:126920
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:122776 variable length of
                   32-bit little-endian integer
/G/H/grp_nested/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:127048
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/G/H/grp_nested/grp_dsets/simple Dataset {6/6}
    Location:  1:127192
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/vl Type
    Location:  1:122776
    Links:     2
    Type:      shared-1:122776 variable length of
                   32-bit little-endian integer
/chunk                   Dataset {6/6}
    Location:  1:6216
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/compact                 Dataset {6/6}
    Location:  1:6344
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/compound                Dataset {2/2}
    Location:  1:8528
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/compressed              Dataset {6/6}
    Location:  1:12888
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_dsets               Group
    Location:  1:33760
    Links:     1
/grp_dsets/chunk         Dataset {6/6}
    Location:  1:37944
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/compact       Dataset {6/6}
    Location:  1:38400
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/compound      Dataset {2/2}
    Location:  1:38536
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_dsets/compressed    Dataset {6/6}
    Location:  1:40848
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_dsets/named_vl      Dataset {2/2}
    Location:  1:45160
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:41016 variable length of
                   32-bit little-endian integer
/grp_dsets/nested_vl     Dataset {2/2}
    Location:  1:45288
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_dsets/simple        Dataset {6/6}
    Location:  1:45432
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/simple_group  Dataset {6/6}
    Location:  1:61544
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/vl            Type
    Location:  1:41016
    Links:     2
    Type:      shared-1:41016 variable length of
                   32-bit little-endian integer
/grp_empty               Group
    Location:  1:32968
    Links:     1
/grp_nested              Group
    Location:  1:46224
    Links:     1
/grp_nested/grp_dsets    Group
    Location:  1:47016
    Links:     1
/grp_nested/grp_dsets/chunk Dataset {6/6}
    Location:  1:51200
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/compact Dataset {6/6}
    Location:  1:51656
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/compound Dataset {2/2}
    Location:  1:51792
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_nested/grp_dsets/compressed Dataset {6/6}
    Location:  1:54104
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/named_vl Dataset {2/2}
    Location:  1:58416
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:54272 variable length of
                   32-bit little-endian integer
/grp_nested/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:58544
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_nested/grp_dsets/simple Dataset {6/6}
    Location:  1:58688
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/vl Type
    Location:  1:54272
    Links:     2
    Type:      shared-1:54272 variable length of
                   32-bit little-endian integer
/grp_rename              Group
    Location:  1:62752
    Links:     1
/grp_rename/chunk        Dataset {6/6}
    Location:  1:66936
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/compact      Dataset {6/6}
    Location:  1:67392
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/compound     Dataset {2/2}
    Location:  1:67528
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_rename/compressed   Dataset {6/6}
    Location:  1:69840
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets    Group
    Location:  1:75632
    Links:     1
/grp_rename/grp_dsets/chunk Dataset {6/6}
    Location:  1:79816
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/compact Dataset {6/6}
    Location:  1:80272
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/compound Dataset {2/2}
    Location:  1:80408
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_rename/grp_dsets/compressed Dataset {6/6}
    Location:  1:82720
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/named_vl Dataset {2/2}
    Location:  1:87032
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:82888 variable length of
                   32-bit little-endian integer
/grp_rename/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:87160
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_rename/grp_dsets/simple Dataset {6/6}
    Location:  1:87304
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/vl Type
    Location:  1:82888
    Links:     2
    Type:      shared-1:82888 variable length of
                   32-bit little-endian integer
/grp_rename/named_vl     Dataset {2/2}
    Location:  1:74152
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:70008 variable length of
                   32-bit little-endian integer
/grp_rename/nested_vl    Dataset {2/2}
    Location:  1:74280
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_rename/simple       Dataset {6/6}
    Location:  1:74424
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/vl           Type
    Location:  1:70008
    Links:     2
    Type:      shared-1:70008 variable length of
                   32-bit little-endian integer
/named_vl                Dataset {2/2}
    Location:  1:13104
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:13056 variable length of
                   32-bit little-endian integer
/nested_vl               Dataset {2/2}
    Location:  1:27392
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/rename                  Dataset {2/2}
    Location:  1:31760
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/simple                  Dataset {6/6}
    Location:  1:800
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/simple_top              Dataset {6/6}
    Location:  1:29584
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
