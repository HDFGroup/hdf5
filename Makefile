# ##################################################################
#
#
# This is the top level Makefile to build HDF5 on Unix based
#       platforms
#

# Define your sh shell
#SHELL=/bin/sh

# Define your machine type
#MACHINE=UNIX386

# Define your C compiler and flags
CC= gcc
#CFLAGS= -ansi -Wall -pedantic -O -c
CFLAGS= -ansi -Wall -pedantic -g -c

# Define your FORTRAN compiler
FC= f77
FFLAGS=

# Location where the HDF include files are to be installed
HDFINC= `pwd`/src

# Location where the HDF library is to be installed
HDFLIB= `pwd`/src

# Location to put HDF utility executables
HDFBIN= `pwd`/bin

# Name of library archiver and flags to send
AR= ar
ARFLAGS= ru

# Name of achive randomizer (use 'touch' if non-existant)
RANLIB= ranlib

# Name of remove utility
RM= /bin/rm
RMFLAGS= -f 

# ##################################################################
#
# This is the top level Makefile to build HDF5 on Unix based
#       platforms
#

#
#
# Flags to recursively send
#

HDF_FLAGS       = \
        CC="$(CC)" \
        CFLAGS="$(CFLAGS)" \
        FC="$(FC)" \
        FFLAGS="$(FFLAGS)" \
        RANLIB="$(RANLIB)" \
        AR="$(AR)" \
        ARFLAGS="$(ARFLAGS)" \
        RM="$(RM)" \
        RMFLAGS="$(RMFLAGS)" \
        MACHINE="$(MACHINE)" \
        HDFLIB="$(HDFLIB)" \
        HDFINC="$(HDFINC)" \
	HDFBIN="$(HDFBIN)"	

#
#
# General rules
#
all:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="src test" subd message 

rebuild:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@ \
          SUBDIRS="src test" subd message 

libhdf5:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=all \
          SUBDIRS="src" subd

tests:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=test \
          SUBDIRS="src test" subd

debug:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=debug \
          SUBDIRS="src test" subd message 

clean:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@  \
          SUBDIRS="src test" subd
	$(RM) $(RMFLAGS) core *.log

distclean:
	@$(MAKE) $(MFLAGS) $(HDF_FLAGS) TARG=$@  \
          SUBDIRS="src test" subd
	$(RM) $(RMFLAGS) core *.log
	$(RM) -rf bin lib include

subd:
	@for dir in $(SUBDIRS); do \
		(cd $$dir; echo Making \`$(TARG)\' in `pwd`; \
		$(MAKE) $(MFLAGS) $(HDF_FLAGS) $(TARG)); \
	done

message:
	@echo ""
	@echo "***********************************************************"
	@echo " HDF5 library successfully created."
	@echo ""
	@echo "***********************************************************"
	@echo ""

