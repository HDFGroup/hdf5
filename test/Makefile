### Name of target
TARGET = testhdf5

#
# PART 2: various choices
#

### -DDEBUG		Debugging options on
DEFS = -I../src

################################################
##   no changes required below this line      ##
################################################

INCL = ../src/hdf5.h testhdf5.h

OBJ =	testhdf5.o tmeta.o tfile.o theap.o tohdr.o tstab.o

LIBS =	../src/libhdf5.a

$(TARGET): $(OBJ) $(LIBS)
	$(CC) -o $(TARGET) $(OBJ) $(LIBS)

all: $(TARGET)

test: $(TARGET)
	./$(TARGET)

debug: $(OBJ) 
	purify $(CC) -o $(TARGET) $(OBJ) $(LIBS)

clean:
	-rm -f *.o core *.core $(TARGET) 
	-rm -f *.bak *.h5

###########################################################################

testhdf5.o:	testhdf5.c  $(INCL)
	$(CC) $(CFLAGS) $(DEFS) testhdf5.c

tmeta.o:	tmeta.c  $(INCL)
	$(CC) $(CFLAGS) $(DEFS) tmeta.c

tfile.o:	tfile.c  $(INCL)
	$(CC) $(CFLAGS) $(DEFS) tfile.c

theap.o:	theap.c $(INCL)
	$(CC) $(CFLAGS) $(DEFS) theap.c

tohdr.o:	tohdr.c $(INCL)
	$(CC) $(CFLAGS) $(DEFS) tohdr.c

tstab.o:	tstab.c $(INCL)
	$(CC) $(CFLAGS) $(DEFS) tstab.c


