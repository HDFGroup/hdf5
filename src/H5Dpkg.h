/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Monday, December  8, 1997
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5D package.  Source files outside the H5D package should
 *		include H5Dprivate.h instead.
 */
#ifndef H5D_PACKAGE
#error "Do not include this file outside the H5D package!"
#endif

#ifndef _H5Dpkg_H
#define _H5Dpkg_H

/*
 * Define this to enable debugging.
 */
#ifdef NDEBUG
#  undef H5D_DEBUG
#endif

#include <H5Dprivate.h>

/*
 * A dataset is the following struct.
 */
struct H5D_t {
    H5G_entry_t		ent;		/*cached object header stuff	*/
    H5T_t		*type;		/*datatype of this dataset	*/
    H5S_t		*space;		/*dataspace of this dataset	*/
    H5D_create_t	*create_parms;	/*creation parameters		*/
    H5O_layout_t	layout;		/*data layout			*/
};


#endif
