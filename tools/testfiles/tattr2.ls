#############################
 output for 'h5ls -w80 -v -S tattr2.h5'
#############################
Opened "tattr2.h5" with sec2 driver.
dset                     Dataset {2/2}
    Attribute: string    scalar
        Type:      17-byte null-terminated ASCII string
        Data:  "string attribute"
    Attribute: bitfield  {2}
        Type:      8-bit bitfield
        Data:  0x01, 0x02
    Attribute: opaque    {2}
        Type:      1-byte opaque type
               (tag = "1-byte opaque type")
        Data:  0x01, 0x02
    Attribute: compound  {2}
        Type:      struct {
                   "a"                +0    32-bit little-endian integer
                   "b"                +4    IEEE 32-bit little-endian float
               } 8 bytes
        Data:  {1, 1}, {2, 2}
    Attribute: reference {2}
        Type:      object reference
        Data:  DATASET-0:1:0:976, DATASET-0:1:0:976
    Attribute: enum      {2}
        Type:      enum 32-bit little-endian integer {
                   RED              = 0
                   GREEN            = 1
               }
        Data:  RED, RED
    Attribute: vlen      {2}
        Type:      variable length of
                   32-bit little-endian integer
        Data:  (0), (10,11)
    Attribute: array     {2}
        Type:      [3] 32-bit little-endian integer
        Data:  [1,1,1], [2,2,2]
    Attribute: integer   {2}
        Type:      32-bit little-endian integer
        Data:  0, 0
    Attribute: float     {2}
        Type:      IEEE 32-bit little-endian float
        Data:  0, 0
    Location:  0:1:0:976
    Links:     1
    Modified:  XXXX-XX-XX XX:XX:XX XXX
    Storage:   8 logical bytes, 0 allocated bytes
    Type:      32-bit little-endian integer
