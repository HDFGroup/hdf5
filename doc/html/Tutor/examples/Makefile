# Edit the following variables as needed
HDF_INSTALL = "/afs/ncsa/packages/hdf5/old/1.0.1-sol2.5"
CC          = gcc
CFLAGS      = '-ansi -O3 -finline-functions -pg'
LIB         = -lm

INCLUDE   = -I$(HDF_INSTALL)/include
LIBSHDF   = -L$(HDF_INSTALL)/lib -lhdf5 -lm -g

all: h5_crtfile \
     h5_crtdat \
     h5_rdwt \
     h5_crtatt \
     h5_crtgrp \
     h5_crtgrpar \
     h5_crtgrpd \
     h5_copy \
     h5_extend \
     h5_iterate \
     h5_mount \
     h5_read \
     h5_reference \
     h5_hyperslab
 

h5_crtfile: h5_crtfile.c
	$(CC) $(CFLAGS) -o $@ h5_crtfile.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_crtdat: h5_crtdat.c
	$(CC) $(CFLAGS) -o $@ h5_crtdat.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_rdwt: h5_rdwt.c
	$(CC) $(CFLAGS) -o $@ h5_rdwt.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_crtatt: h5_crtatt.c
	$(CC) $(CFLAGS) -o $@ h5_crtatt.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_crtgrp: h5_crtgrp.c
	$(CC) $(CFLAGS) -o $@ h5_crtgrp.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_crtgrpar: h5_crtgrpar.c
	$(CC) $(CFLAGS) -o $@ h5_crtgrpar.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_crtgrpd: h5_crtgrpd.c
	$(CC) $(CFLAGS) -o $@ h5_crtgrpd.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_copy: h5_copy.c
	$(CC) $(CFLAGS) -o $@ h5_copy.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_extend: h5_extend.c
	$(CC) $(CFLAGS) -o $@ h5_extend.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_iterate: h5_iterate.c
	$(CC) $(CFLAGS) -o $@ h5_iterate.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_mount: h5_mount.c
	$(CC) $(CFLAGS) -o $@ h5_mount.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_read: h5_read.c
	$(CC) $(CFLAGS) -o $@ h5_read.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_reference: h5_reference.c
	$(CC) $(CFLAGS) -o $@ h5_reference.c $(INCLUDE) $(LIBSHDF) $(LIB)

h5_hyperslab: h5_hyperslab.c
	$(CC) $(CFLAGS) -o $@ h5_hyperslab.c $(INCLUDE) $(LIBSHDF) $(LIB)


clean: 
	rm -f *.o h5_crtfile \
        h5_crtdat \
        h5_rdwt \
        h5_crtatt \
        h5_crtgrp \
        h5_crtgrpar \
        h5_crtgrpd \
        h5_copy \
        h5_extend \
        h5_iterate \
        h5_mount \
        h5_read \
        h5_reference \
        h5_hyperslab 

.SUFFIXES:.o.c
