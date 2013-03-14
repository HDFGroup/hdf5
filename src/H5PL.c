/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#define H5PL_PACKAGE		/*suppress error about including H5PLpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5PL_init_interface

#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5Sprivate.h"		/* Dataspace functions			*/
#include "H5Zprivate.h"		/* Filter pipeline			*/
#include "H5PLpkg.h"		/* Plugin       			*/

#define H5PL_DEFAULT_PATH       "/usr:/usr/lib:/usr/local"
#define H5PL_PATH_SEPERATOR     ":"


/*--------------------------------------------------------------------------
NAME
   H5PL_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5PL_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5PL_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5PL_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5PL_term_interface
 *
 * Purpose:	Terminate the H5PL interface: release all memory, reset all
 *		global variables to initial values. This only happens if all
 *		types have been destroyed from other interfaces.
 *
 * Return:	Success:	Positive if any action was taken that might
 *				affect some other interface; zero otherwise.
 *
 * 		Failure:	Negative.
 *
 * Programmer:	Raymond Lu
 *              20 February 2013
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5PL_term_interface(void)
{
    void    *handle = NULL;
    size_t  i = 0;
    
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_interface_initialize_g) {
	/* Close opened dynamic libraries */
        for(i=0; i<H5PL_table_used_g; i++) { 
            handle = (H5PL_table_g[i]).handle;
            H5PL_close(handle);
        }

	/* Free the table of dynamic libraries */
	H5PL_table_g = (H5PL_table_t *)H5MM_xfree(H5PL_table_g);
	H5PL_table_used_g = H5PL_table_alloc_g = 0;

        /* Free the table of search paths */
        for(i = 0; i < num_paths; i++) {
            if(path_table[i])
                path_table[i] = H5MM_xfree(path_table[i]);
        }
        num_paths = 0;
        path_found = FALSE;

	H5_interface_initialize_g = 0;
        i = 1;
    } /* end if */

    FUNC_LEAVE_NOAPI(i)
} /* end H5PL_term_interface() */



/*-------------------------------------------------------------------------
 * Function:	H5PL_load
 *
 * Purpose:	Given the plugin type and identifier, this function searches
 *              and/or loads a dynamic plugin library first among the already
 *              opened libraries then in the designated location paths.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              13 February 2013
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5PL_load(H5PL_type_t type, int id)
{
    char           *dl_path = NULL;
    char           *origin_dl_path = NULL;
    char           *pathname = NULL;
    size_t         len = 0;
    char           *dir = NULL;
    DIR            *dirp = NULL;
    int	           i;
    void           *handle = NULL;
    htri_t         found_in_table = FALSE;
    htri_t         found_in_path = FALSE;
    H5Z_class2_t*  (*PL_get_plugin_info)(void) = NULL; 
    H5Z_class2_t   *plugin_info = NULL;
    void           *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    /* Search in the table of already loaded plugin libraries */
    if((found_in_table = H5PL_search_table(type, id, &plugin_info)) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL, "search in table failed")

    /* Finish the function if found */
    if(found_in_table && plugin_info) {
        ret_value = (void *)plugin_info;
        HGOTO_DONE(ret_value)
    }

    /* Find the location paths for dynamic libraries */
    if(FALSE == path_found) {
        /* Retrieve paths from HDF5_PLUGIN_PATH if the user sets it
         * or from the default paths if it isn't set */
        origin_dl_path = HDgetenv("HDF5_PLUGIN_PATH");
        if(origin_dl_path == NULL) {
            len = HDstrlen(H5PL_DEFAULT_PATH) + 1;
            dl_path = (char *)H5MM_malloc(len*sizeof(char));
            HDstrncpy(dl_path, H5PL_DEFAULT_PATH, len-1);
            dl_path[len-1] = '\0';
        } else {
            len = HDstrlen(origin_dl_path) + 1;
            dl_path = (char *)H5MM_malloc(len*sizeof(char));
            HDstrncpy(dl_path, origin_dl_path, len);
        }

        /* Put paths in the path table.  They are seperated by ":" */
        dir = HDstrtok(dl_path, H5PL_PATH_SEPERATOR);
        while(dir) {
            path_table[num_paths] = (char *)HDmalloc(HDstrlen(dir) + 1);
            path_table[num_paths][0] = '\0';
            HDstrcat(path_table[num_paths], dir);
            num_paths++;
            dir = HDstrtok(NULL, H5PL_PATH_SEPERATOR);
        }
   
        path_found = TRUE;
    }

    /* Iterate through the path table to find the right dynamic libraries */
    for(i=0; i<num_paths; i++) {
        if((found_in_path = H5PL_find(type, id, path_table[i], &plugin_info)) < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL, "search in paths failed")
 
        /* Finish the function if found */
        if(found_in_path && plugin_info) {
            ret_value = (void *)plugin_info;
            HGOTO_DONE(ret_value)
        }
    }

done:
    if(dl_path)
        dl_path = (char *)H5MM_xfree(dl_path);

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5PL_find
 *
 * Purpose:     Given a path, this function opens the directory and iterates
 *              through all files to find the right plugin library.  It 
 *              loads the dynamic plugin library and keeps it on the list 
 *              of loaded libraries. 	
 *
 * Return:	TRUE on success, 
 *              FALSE on not found,
 *              negative on failure
 *
 * Programmer:	Raymond Lu
 *              13 February 2013
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5PL_find(H5PL_type_t plugin_type, int type_id, char *dir, void **info)
{
    char           *dl_path = NULL;
    char           *pathname = NULL;
    DIR            *dirp = NULL;
    int	           i;
    void           *handle = NULL;
    htri_t         found_in_table = FALSE;
    hbool_t        free_path = FALSE;
    H5Z_class2_t*  (*H5PL_get_plugin_info)(void) = NULL; 
    H5Z_class2_t   *plugin_info = NULL;
    htri_t         ret_value = FALSE;

    FUNC_ENTER_NOAPI(FAIL)

    /* Open the directory */  
    if(dirp = HDopendir(dir)) {
        struct dirent *dp = NULL;
        struct stat my_stat;
        /* Iterates through all entries in the directory to find the right plugin library */
        while ((dp = HDreaddir(dirp)) != NULL) {
                   /* The library we are looking for should be called libxxx.so... */ 
                   if(!HDstrncmp(dp->d_name, "lib", 3) && HDstrstr(dp->d_name, ".so")) {
                       pathname = (char *)H5MM_malloc(strlen(dir) + strlen(dp->d_name) + 2); 
                       HDstrncpy(pathname, dir, strlen(dir)+1);
                       HDstrcat(pathname, "/");
                       HDstrcat(pathname, dp->d_name);

                       /*fprintf(stderr, "dp->d_name=%s, pathname=%s. ", dp->d_name, pathname);
                       fprintf(stderr, "\n");*/

                       if(HDstat(pathname, &my_stat) == -1)
                           HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't stat file: %s", strerror(errno))
                       
                       if(!S_ISDIR(my_stat.st_mode)) { /* if it is a directory, skip it */
                           if(NULL == (handle = dlopen(pathname, RTLD_NOW|RTLD_LAZY))) {
                               /*fprintf(stderr, "not open dl library: %s", dlerror());*/
                               /* There are different reasons why a library can't be open, e.g. wrong architecture.
                                * simply continue if we can't open it */
                               continue;
                           }

                           dlerror(); /*clear error*/
    
                           /* Return a handle for the function H5PL_get_plugin_info in the dynamic library.
                            * The plugin library is suppose to define this function. */
                           if(NULL == (H5PL_get_plugin_info = dlsym(handle, "H5PL_get_plugin_info"))) {
                               if(H5PL_close(handle) < 0)
                                   HGOTO_ERROR(H5E_PLUGIN, H5E_CLOSEERROR, FAIL, "can't close dynamic library")
                           }

                           /* Envoke H5PL_get_plugin_info to verify this is the right library we are looking for.
                            * Move on if it isn't. */
                           if(H5PL_get_plugin_info) {
                               if(NULL == (plugin_info = (*H5PL_get_plugin_info)())) {
                                   if(H5PL_close(handle) < 0)
                                       HGOTO_ERROR(H5E_PLUGIN, H5E_CLOSEERROR, FAIL, "can't close dynamic library")
                                   HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get plugin info")
                               }

                               if(plugin_info->id == type_id) {
                                   (H5PL_table_g[H5PL_table_used_g]).handle = handle;
                                   (H5PL_table_g[H5PL_table_used_g]).pl_type = plugin_type;
                                   (H5PL_table_g[H5PL_table_used_g]).pl_id = plugin_info->id;
/*fprintf(stderr, "%s: H5PL_table_used_g=%d, id=%d, id 2=%d\n", FUNC, H5PL_table_used_g, (H5PL_table_g[H5PL_table_used_g]).pl_id, plugin_info->id);*/
                                   H5PL_table_used_g++;

      	                           *info = (void *)plugin_info;
	                           ret_value = TRUE;
                         
	                           HGOTO_DONE(ret_value)
                               } else if(H5PL_close(handle) < 0)
                                   HGOTO_ERROR(H5E_PLUGIN, H5E_CLOSEERROR, FAIL, "can't close dynamic library")
                           }
                       }
 
                       if(pathname) 
                           pathname = (char *)H5MM_xfree(pathname);
                   }
        }

        if(HDclosedir(dirp) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CLOSEERROR, FAIL, "can't close directory: %s", strerror(errno))
        dirp = NULL;
    }


done:
    if(pathname) 
        pathname = (char *)H5MM_xfree(pathname);
    if(dirp) 
        HDclosedir(dirp);

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5PL_search_table
 *
 * Purpose:     Search in the list of already opened dynamic libraries
 *              to see if the one we are looking for is already opened.
 *
 * Return:	TRUE on success, 
 *              FALSE on not found,
 *              negative on failure
 *
 * Programmer:	Raymond Lu
 *              13 February 2013
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5PL_search_table(H5PL_type_t plugin_type, int type_id, void **info)
{
    htri_t         found = FALSE;
    H5Z_class2_t*  (*H5PL_get_plugin_info)(void) = NULL; 
    H5Z_class2_t   *plugin_info = NULL;
    int            i;
    htri_t         ret_value = FALSE;

    FUNC_ENTER_NOAPI(FAIL)

/*if(0 < H5PL_table_used_g)
fprintf(stderr, "%s: H5PL_table_used_g=%d, id=%d\n", FUNC, H5PL_table_used_g, (H5PL_table_g[H5PL_table_used_g-1]).pl_id);*/

    /* Search in the table of already opened dynamic libraries */
    if(0 < H5PL_table_used_g) {
        for(i=0; i<H5PL_table_used_g; i++) {
            if((plugin_type == (H5PL_table_g[i]).pl_type) && (type_id == (H5PL_table_g[i]).pl_id)) {
  	        if(NULL == (H5PL_get_plugin_info = dlsym((H5PL_table_g[i]).handle, "H5PL_get_plugin_info")))
		    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get function: %s", dlerror())

	        if(NULL == (plugin_info = (*H5PL_get_plugin_info)()))
		    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get plugin info")

	        /*fprintf(stderr, "%s: handle=%p, H5PL_get_plugin_info=%p, plugin_info=%p, id=%d\n", FUNC, (H5PL_table_g[i]).handle, H5PL_get_plugin_info, plugin_info, plugin_info->id);*/

	        *info = (void *)plugin_info;
	        ret_value = TRUE;
            }
        }
    }

    /* Expand the table if it is too small */
    if(H5PL_table_used_g >= H5PL_table_alloc_g) {
        size_t n = MAX(H5Z_MAX_NFILTERS, 2*H5PL_table_alloc_g);
        H5PL_table_t *table = (H5PL_table_t *)H5MM_realloc(H5PL_table_g, n * sizeof(H5PL_table_t));

        if(!table)
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to extend dynamic library table")

        H5PL_table_g = table;
        H5PL_table_alloc_g = n;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5PL_close
 *
 * Purpose:     Closes the handle for dynamic library	
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              13 February 2013
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PL_close(void *handle)
{
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)
/*fprintf(stderr, "%s: closing. handle=%p\n", FUNC, handle);*/
    dlclose(handle);
   
done:
    FUNC_LEAVE_NOAPI(ret_value)
}

