#ifndef _H5DUMP_H
#define _H5DUMP_H

#include <hdf5.h>

#define BOOT_BLOCK	"BOOT_BLOCK"
#define GROUPNAME	"GROUP"
#define DATASET		"DATASET"
#define ATTRIBUTE	"ATTRIBUTE"
#define	DATATYPE	"DATATYPE"
#define DATASPACE	"DATASPACE"
#define DATA		"DATA"
#define SCALAR		"SCALAR"
#define SIMPLE		"SIMPLE"
#define COMPLEX		"COMPLEX"
#define STORAGELAYOUT	"STORAGELAYOUT"
#define COMPRESSION	"COMPRESSION"
#define EXTERNAL	"EXTERNAL"
#define SOFTLINK	"SOFTLINK"
#define HARDLINK	"HARDLINK"
#define NLINK		"NLINK"
#define FILENO		"FILENO"
#define OBJNO		"OBJNO"
#define STRSIZE		"STRSIZE"
#define STRPAD		"STRPAD"
#define CSET		"CSET"
#define CTYPE		"CTYPE"
#define CONCATENATOR "//"
#define BEGIN		"{"
#define END		"}"



#define H5DUMP_MAX_RANK	H5S_MAX_RANK

#define begin_obj(obj,name)	printf("%s \"%s\" %s\n", obj, name, BEGIN)
#define end_obj()		printf("%s\n", END);


#endif
