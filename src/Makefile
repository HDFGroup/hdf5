### Name of target
TARGET = libhdf5.a

#
# PART 2: various choices
#

### -DDEBUG		Debugging options on
DEFS = 

#
# PART 3: hardware dependend
#

### CC entry:      name and arguments for the compiler (also for linking)
### MACHINE entry: defines used for compiling (not for linking)
### LIBS:          defines used for linking
MACHINE = 
CC=gcc -O
LIBS = 

################################################
##   no changes required below this line      ##
################################################

CFLAGS = -c $(MACHINE) $(DEFS)

INCL = hdf5.h

OBJ =	H5.o H5E.o H5A.o H5F.o H5C.o H5M.o H5AC.o H5B.o H5MM.o H5MF.o H5T.o \
    H5Gnode.o H5H.o H5G.o H5P.o H5D.o H5O.o H5Onull.o H5Ocont.o H5Ostab.o

$(TARGET): $(OBJ) 
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)
	$(RANLIB) $(TARGET)

all test: $(TARGET) debug_hdf5

debug: $(OBJ) 
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)
	$(RANLIB) $(TARGET)

clean:
	-rm -f *.o core $(TARGET) 
	-rm -f *.bak

###########################################################################

debug_hdf5: $(TARGET) debug.o
	$(CC) -o debug debug.o -L. -lhdf5

debug.o: debug.c $(INCL)
	$(CC) $(CFLAGS) debug.c

H5.o:	H5.c  $(INCL) H5private.h H5proto.h
	$(CC) $(CFLAGS) H5.c

H5E.o:	H5E.c  $(INCL) H5Eprivate.h H5Eproto.h
	$(CC) $(CFLAGS) H5E.c

H5F.o:	H5F.c  $(INCL) H5Fprivate.h H5Fproto.h
	$(CC) $(CFLAGS) H5F.c

H5C.o:	H5C.c  $(INCL) H5Cprivate.h H5Cproto.h
	$(CC) $(CFLAGS) H5C.c

H5M.o:	H5M.c  $(INCL) H5Mprivate.h H5Mproto.h
	$(CC) $(CFLAGS) H5M.c

H5A.o:	H5A.c  $(INCL) H5Aprivate.h H5Aproto.h
	$(CC) $(CFLAGS) H5A.c

H5AC.o:	H5AC.c  $(INCL) H5ACprivate.h H5ACproto.h
	$(CC) $(CFLAGS) H5AC.c

H5B.o:	H5B.c  $(INCL) H5Bprivate.h H5Bproto.h
	$(CC) $(CFLAGS) H5B.c

H5MM.o:	H5MM.c  $(INCL) H5MMprivate.h H5MMproto.h
	$(CC) $(CFLAGS) H5MM.c

H5MF.o:	H5MF.c  $(INCL) H5MFprivate.h H5MFproto.h
	$(CC) $(CFLAGS) H5MF.c

H5T.o:	H5T.c  $(INCL) H5Tprivate.h H5Tproto.h
	$(CC) $(CFLAGS) H5T.c

H5Gnode.o: H5Gnode.c $(INCL) H5Gprivate.h H5Gproto.h
	$(CC) $(CFLAGS) H5Gnode.c

H5H.o: H5H.c $(INCL) H5Hprivate.h H5Hproto.h
	$(CC) $(CFLAGS) H5H.c

H5G.o: H5G.c $(INCL) H5Gprivate.h H5Gproto.h
	$(CC) $(CFLAGS) H5G.c

H5P.o: H5P.c $(INCL) H5Pprivate.h H5Pproto.h
	$(CC) $(CFLAGS) H5P.c

H5D.o: H5D.c $(INCL) H5Dprivate.h H5Dproto.h
	$(CC) $(CFLAGS) H5D.c

H5O.o: H5O.c $(INCL) H5Oprivate.h H5Oproto.h
	$(CC) $(CFLAGS) H5O.c

H5Onull.o: H5Onull.c $(INCL) H5Oprivate.h H5Oproto.h
	$(CC) $(CFLAGS) H5Onull.c

H5Ocont.o: H5Ocont.c $(INCL) H5Oprivate.h H5Oproto.h
	$(CC) $(CFLAGS) H5Ocont.c

H5Ostab.o: H5Ostab.c $(INCL) H5Oprivate.h H5Oproto.h
	$(CC) $(CFLAGS) H5Ostab.c
