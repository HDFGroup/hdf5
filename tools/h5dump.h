#ifndef H5DUMP_H__
#define H5DUMP_H__

#include <hdf5.h>

#define H5DUMP_MAX_RANK		H5S_MAX_RANK

#define begin_obj(obj,name,begin)											\
	if (name)																\
		printf("%s \"%s\" %s\n", (obj), (name), (begin));					\
	else																	\
		printf("%s %s\n", (obj), (begin));

#define end_obj(obj,end)													\
	printf("%s %s\n", (end), (obj));

#endif	/* H5DUMP_H__ */
