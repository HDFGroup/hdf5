#############################
Expected output for 'h5ls ../testfiles/h5copytst.a.out.h5'
#############################
Opened "../testfiles/h5copytst.a.out.h5" with sec2 driver.
/chunk                   Dataset {6/6}
    Location:  1:6216
    Links:     1
    Modified:  2006-10-17 15:55:17 CDT
    Chunks:    {2} 8 bytes
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
/compact                 Dataset {6/6}
    Location:  1:6344
    Links:     1
    Modified:  2006-10-17 15:55:17 CDT
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
/compound                Dataset {2/2}
    Location:  1:8528
    Links:     1
    Modified:  2006-10-17 15:55:17 CDT
    Storage:   80 logical bytes, 80 allocated bytes, 100.00% utilization
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/compressed              Dataset {6/6}
    Location:  1:12888
    Links:     1
    Modified:  2006-10-17 15:55:17 CDT
    Chunks:    {2} 8 bytes
    Storage:   24 logical bytes, 42 allocated bytes, 57.14% utilization
    Filter-0:  deflate-1 OPT {1}
    Type:      native int
/named_vl                Dataset {2/2}
    Location:  1:13104
    Links:     1
    Modified:  2006-10-17 15:55:17 CDT
    Storage:   16 logical bytes, 32 allocated bytes, 50.00% utilization
    Type:      shared-1:13056 variable length of
                   native int
/nested_vl               Dataset {2/2}
    Location:  1:27392
    Links:     1
    Modified:  2006-10-17 15:55:17 CDT
    Storage:   16 logical bytes, 32 allocated bytes, 50.00% utilization
    Type:      variable length of
                   variable length of
                       native int
/simple                  Dataset {6/6}
    Location:  1:808
    Links:     1
    Modified:  2006-10-17 15:55:17 CDT
    Storage:   24 logical bytes, 24 allocated bytes, 100.00% utilization
    Type:      native int
