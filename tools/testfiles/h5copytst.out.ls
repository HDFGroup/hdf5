#############################
Expected output for 'h5ls ../testfiles/h5copytst.out.h5'
#############################
Opened "../testfiles/h5copytst.out.h5" with sec2 driver.
/A                       Group
    Location:  1:90344
    Links:     1
/A/B1                    Group
    Location:  1:91056
    Links:     1
/A/B1/simple             Dataset {6/6}
    Location:  1:90216
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/A/B2                    Group
    Location:  1:94600
    Links:     1
/A/B2/simple2            Dataset {6/6}
    Location:  1:94472
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/C                       Group
    Location:  1:97816
    Links:     1
/C/D                     Group
    Location:  1:98528
    Links:     1
/C/D/simple              Dataset {6/6}
    Location:  1:97688
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E                       Group
    Location:  1:103960
    Links:     1
/E/F                     Group
    Location:  1:113216
    Links:     1
/E/F/grp_dsets           Group
    Location:  1:100648
    Links:     1
/E/F/grp_dsets/chunk     Dataset {6/6}
    Location:  1:102784
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/compact   Dataset {6/6}
    Location:  1:103240
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/compound  Dataset {2/2}
    Location:  1:103376
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/E/F/grp_dsets/compressed Dataset {6/6}
    Location:  1:103592
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/E/F/grp_dsets/named_vl  Dataset {2/2}
    Location:  1:103808
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:103760 variable length of
                   32-bit little-endian integer
/E/F/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:112280
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/E/F/grp_dsets/simple    Dataset {6/6}
    Location:  1:112424
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/vl        Type
    Location:  1:103760
    Links:     2
    Type:      shared-1:103760 variable length of
                   32-bit little-endian integer
/G                       Group
    Location:  1:128264
    Links:     1
/G/H                     Group
    Location:  1:128976
    Links:     1
/G/H/grp_nested          Group
    Location:  1:115248
    Links:     1
/G/H/grp_nested/grp_dsets Group
    Location:  1:116040
    Links:     1
/G/H/grp_nested/grp_dsets/chunk Dataset {6/6}
    Location:  1:118176
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/compact Dataset {6/6}
    Location:  1:120776
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/compound Dataset {2/2}
    Location:  1:120912
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/G/H/grp_nested/grp_dsets/compressed Dataset {6/6}
    Location:  1:123224
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/named_vl Dataset {2/2}
    Location:  1:127536
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:123392 variable length of
                   32-bit little-endian integer
/G/H/grp_nested/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:127664
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/G/H/grp_nested/grp_dsets/simple Dataset {6/6}
    Location:  1:127808
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/vl Type
    Location:  1:123392
    Links:     2
    Type:      shared-1:123392 variable length of
                   32-bit little-endian integer
/chunk                   Dataset {6/6}
    Location:  1:6216
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/compact                 Dataset {6/6}
    Location:  1:6344
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/compound                Dataset {2/2}
    Location:  1:8528
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/compressed              Dataset {6/6}
    Location:  1:12888
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_dsets               Group
    Location:  1:33760
    Links:     1
/grp_dsets/chunk         Dataset {6/6}
    Location:  1:35896
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/compact       Dataset {6/6}
    Location:  1:36352
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/compound      Dataset {2/2}
    Location:  1:36488
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_dsets/compressed    Dataset {6/6}
    Location:  1:36704
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_dsets/named_vl      Dataset {2/2}
    Location:  1:36920
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:36872 variable length of
                   32-bit little-endian integer
/grp_dsets/nested_vl     Dataset {2/2}
    Location:  1:45392
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_dsets/simple        Dataset {6/6}
    Location:  1:45536
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/simple_group  Dataset {6/6}
    Location:  1:61744
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/vl            Type
    Location:  1:36872
    Links:     2
    Type:      shared-1:36872 variable length of
                   32-bit little-endian integer
/grp_empty               Group
    Location:  1:32968
    Links:     1
/grp_nested              Group
    Location:  1:46328
    Links:     1
/grp_nested/grp_dsets    Group
    Location:  1:47120
    Links:     1
/grp_nested/grp_dsets/chunk Dataset {6/6}
    Location:  1:49256
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/compact Dataset {6/6}
    Location:  1:51856
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/compound Dataset {2/2}
    Location:  1:51992
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_nested/grp_dsets/compressed Dataset {6/6}
    Location:  1:54304
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/named_vl Dataset {2/2}
    Location:  1:58616
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:54472 variable length of
                   32-bit little-endian integer
/grp_nested/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:58744
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_nested/grp_dsets/simple Dataset {6/6}
    Location:  1:58888
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/vl Type
    Location:  1:54472
    Links:     2
    Type:      shared-1:54472 variable length of
                   32-bit little-endian integer
/grp_rename              Group
    Location:  1:62952
    Links:     1
/grp_rename/chunk        Dataset {6/6}
    Location:  1:65088
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/compact      Dataset {6/6}
    Location:  1:65544
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/compound     Dataset {2/2}
    Location:  1:65680
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_rename/compressed   Dataset {6/6}
    Location:  1:65896
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets    Group
    Location:  1:75936
    Links:     1
/grp_rename/grp_dsets/chunk Dataset {6/6}
    Location:  1:78072
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/compact Dataset {6/6}
    Location:  1:78528
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/compound Dataset {2/2}
    Location:  1:78664
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_rename/grp_dsets/compressed Dataset {6/6}
    Location:  1:78880
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/named_vl Dataset {2/2}
    Location:  1:79096
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:79048 variable length of
                   32-bit little-endian integer
/grp_rename/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:87568
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_rename/grp_dsets/simple Dataset {6/6}
    Location:  1:87712
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/vl Type
    Location:  1:79048
    Links:     2
    Type:      shared-1:79048 variable length of
                   32-bit little-endian integer
/grp_rename/named_vl     Dataset {2/2}
    Location:  1:66112
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:66064 variable length of
                   32-bit little-endian integer
/grp_rename/nested_vl    Dataset {2/2}
    Location:  1:74584
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_rename/simple       Dataset {6/6}
    Location:  1:74728
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/vl           Type
    Location:  1:66064
    Links:     2
    Type:      shared-1:66064 variable length of
                   32-bit little-endian integer
/named_vl                Dataset {2/2}
    Location:  1:13104
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:13056 variable length of
                   32-bit little-endian integer
/nested_vl               Dataset {2/2}
    Location:  1:27392
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/rename                  Dataset {2/2}
    Location:  1:31760
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/simple                  Dataset {6/6}
    Location:  1:808
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/simple_top              Dataset {6/6}
    Location:  1:29584
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
