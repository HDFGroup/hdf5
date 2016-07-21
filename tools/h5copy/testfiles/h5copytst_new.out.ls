#############################
Expected output for 'h5ls ../testfiles/h5copytst_new.out.h5'
#############################
Opened "../testfiles/h5copytst_new.out.h5" with sec2 driver.
/                        Group
    Location:  1:96
    Links:     1
/A                       Group
    Location:  1:65602
    Links:     1
/A/B1                    Group
    Location:  1:66306
    Links:     1
/A/B1/simple             Dataset {6/6}
    Location:  1:65509
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/A/B2                    Group
    Location:  1:69807
    Links:     1
/A/B2/simple2            Dataset {6/6}
    Location:  1:69714
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/C                       Group
    Location:  1:72980
    Links:     1
/C/D                     Group
    Location:  1:73684
    Links:     1
/C/D/simple              Dataset {6/6}
    Location:  1:72887
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E                       Group
    Location:  1:76217
    Links:     1
/E/F                     Group
    Location:  1:76921
    Links:     1
/E/F/grp_dsets           Group
    Location:  1:75044
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
/E/F/grp_dsets/chunk     Dataset {6/6}
    Location:  1:76014
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/compact   Dataset {6/6}
    Location:  1:75367
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/compound  Dataset {2/2}
    Location:  1:75470
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/E/F/grp_dsets/compressed Dataset {6/6}
    Location:  1:75683
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/E/F/grp_dsets/named_vl  Dataset {2/2}
    Location:  1:75853
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:75793 variable length of
                   32-bit little-endian integer
/E/F/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:76108
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/E/F/grp_dsets/simple    Dataset {6/6}
    Location:  1:75274
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/vl        Type
    Location:  1:75793
    Links:     2
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Type:      shared-1:75793 variable length of
                   32-bit little-endian integer
/G                       Group
    Location:  1:85688
    Links:     1
/G/H                     Group
    Location:  1:86392
    Links:     1
/G/H/grp_nested          Group
    Location:  1:84436
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
/G/H/grp_nested/grp_dsets Group
    Location:  1:84515
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
/G/H/grp_nested/grp_dsets/chunk Dataset {6/6}
    Location:  1:85485
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/compact Dataset {6/6}
    Location:  1:84838
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/compound Dataset {2/2}
    Location:  1:84941
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/G/H/grp_nested/grp_dsets/compressed Dataset {6/6}
    Location:  1:85154
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/named_vl Dataset {2/2}
    Location:  1:85324
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:85264 variable length of
                   32-bit little-endian integer
/G/H/grp_nested/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:85579
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/G/H/grp_nested/grp_dsets/simple Dataset {6/6}
    Location:  1:84745
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/vl Type
    Location:  1:85264
    Links:     2
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Type:      shared-1:85264 variable length of
                   32-bit little-endian integer
/chunk                   Dataset {6/6}
    Location:  1:2238
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/compact                 Dataset {6/6}
    Location:  1:4240
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/compound                Dataset {2/2}
    Location:  1:6391
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/compressed              Dataset {6/6}
    Location:  1:6604
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_dsets               Group
    Location:  1:27748
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
/grp_dsets/chunk         Dataset {6/6}
    Location:  1:28718
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/compact       Dataset {6/6}
    Location:  1:28071
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/compound      Dataset {2/2}
    Location:  1:28174
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_dsets/compressed    Dataset {6/6}
    Location:  1:28387
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_dsets/named_vl      Dataset {2/2}
    Location:  1:28557
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:28497 variable length of
                   32-bit little-endian integer
/grp_dsets/nested_vl     Dataset {2/2}
    Location:  1:28812
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_dsets/simple        Dataset {6/6}
    Location:  1:27978
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/simple_group  Dataset {6/6}
    Location:  1:46180
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/vl            Type
    Location:  1:28497
    Links:     2
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Type:      shared-1:28497 variable length of
                   32-bit little-endian integer
/grp_empty               Group
    Location:  1:27693
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
/grp_nested              Group
    Location:  1:35940
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
/grp_nested/grp_dsets    Group
    Location:  1:36019
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
/grp_nested/grp_dsets/chunk Dataset {6/6}
    Location:  1:36989
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/compact Dataset {6/6}
    Location:  1:36342
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/compound Dataset {2/2}
    Location:  1:36445
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_nested/grp_dsets/compressed Dataset {6/6}
    Location:  1:36658
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/named_vl Dataset {2/2}
    Location:  1:36828
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:36768 variable length of
                   32-bit little-endian integer
/grp_nested/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:37083
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_nested/grp_dsets/simple Dataset {6/6}
    Location:  1:36249
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/vl Type
    Location:  1:36768
    Links:     2
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Type:      shared-1:36768 variable length of
                   32-bit little-endian integer
/grp_rename              Group
    Location:  1:47077
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
/grp_rename/chunk        Dataset {6/6}
    Location:  1:48047
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/compact      Dataset {6/6}
    Location:  1:47400
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/compound     Dataset {2/2}
    Location:  1:47503
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_rename/compressed   Dataset {6/6}
    Location:  1:47716
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets    Group
    Location:  1:55269
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
/grp_rename/grp_dsets/chunk Dataset {6/6}
    Location:  1:56239
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/compact Dataset {6/6}
    Location:  1:55592
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/compound Dataset {2/2}
    Location:  1:55695
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_rename/grp_dsets/compressed Dataset {6/6}
    Location:  1:55908
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/named_vl Dataset {2/2}
    Location:  1:56078
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:56018 variable length of
                   32-bit little-endian integer
/grp_rename/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:56333
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_rename/grp_dsets/simple Dataset {6/6}
    Location:  1:55499
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/vl Type
    Location:  1:56018
    Links:     2
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Type:      shared-1:56018 variable length of
                   32-bit little-endian integer
/grp_rename/named_vl     Dataset {2/2}
    Location:  1:47886
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:47826 variable length of
                   32-bit little-endian integer
/grp_rename/nested_vl    Dataset {2/2}
    Location:  1:48141
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_rename/simple       Dataset {6/6}
    Location:  1:47307
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/vl           Type
    Location:  1:47826
    Links:     2
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Type:      shared-1:47826 variable length of
                   32-bit little-endian integer
/named_vl                Dataset {2/2}
    Location:  1:8657
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      shared-1:8606 variable length of
                   32-bit little-endian integer
/nested_vl               Dataset {2/2}
    Location:  1:22942
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/rename                  Dataset {2/2}
    Location:  1:27240
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/simple                  Dataset {6/6}
    Location:  1:800
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/simple_top              Dataset {6/6}
    Location:  1:25099
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
