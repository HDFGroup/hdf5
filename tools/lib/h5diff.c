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

#include <stdlib.h>
#include "h5diff.h"
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"


/*-------------------------------------------------------------------------
 * Function: print_objname
 *
 * Purpose: check if object name is to be printed, only when:
 *  1) verbose mode
 *  2) when diff was found (normal mode)
 *-------------------------------------------------------------------------
 */
int
print_objname (diff_opt_t * options, hsize_t nfound)
{
    return ((options->m_verbose || nfound) && !options->m_quiet) ? 1 : 0;
}

/*-------------------------------------------------------------------------
 * Function: do_print_objname
 *
 * Purpose: print object name 
 *
 *-------------------------------------------------------------------------
 */
void
do_print_objname (const char *OBJ, const char *path1, const char *path2)
{
    printf("%-7s: <%s> and <%s>\n", OBJ, path1, path2);
}

/*-------------------------------------------------------------------------
 * Function: h5diff
 *
 * Purpose: public function, can be called in an application program.
 *   return differences between 2 HDF5 files
 *
 * Return: Number of differences found.
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 22, 2003
 *
 *-------------------------------------------------------------------------
 */

hsize_t h5diff(const char *fname1,
               const char *fname2,
               const char *objname1,
               const char *objname2,
               diff_opt_t *options)
{    
    trav_info_t  *info1=NULL;
    trav_info_t  *info2=NULL;
    hid_t        file1_id = (-1);
    hid_t        file2_id = (-1);
    int          nobjects1;
    int          nobjects2;
    hsize_t      nfound = 0;
    
    if(options->m_quiet && (options->m_verbose || options->m_report)) 
    {
        printf("Error: -q (quiet mode) cannot be added to verbose or report modes\n");
        options->err_stat=1;
        return 0;
    } /* end if */
    
   /*-------------------------------------------------------------------------
    * open the files first; if they are not valid, no point in continuing
    *-------------------------------------------------------------------------
    */

    
     /* disable error reporting */
    H5E_BEGIN_TRY 
    {
        /* open file 1 */
        
        if((file1_id = h5tools_fopen(fname1, NULL, NULL, 0)) < 0) 
        {
            printf("h5diff: <%s>: unable to open file\n", fname1 );
            options->err_stat = 1;
            goto out;
        } 

        /* open file 2 */

        if((file2_id = h5tools_fopen(fname2, NULL, NULL, 0)) < 0) 
        {
            printf("h5diff: <%s>: unable to open file\n", fname2 );
            options->err_stat = 1;
            goto out;
        } 

    /* enable error reporting */
    } H5E_END_TRY;
    
    
   /*-------------------------------------------------------------------------
    * get the number of objects in the files
    *-------------------------------------------------------------------------
    */
    nobjects1 = h5trav_getinfo( file1_id, NULL, 0 );
    nobjects2 = h5trav_getinfo( file2_id, NULL, 0 );
    
    if (nobjects1<0 || nobjects2<0)
    {
        printf("Error: Could not get get file contents\n");
        options->err_stat=1;
        goto out;
    }
    
   /*-------------------------------------------------------------------------
    * get the list of objects in file1
    *-------------------------------------------------------------------------
    */

    if ( nobjects1 )
    {
        
        info1 = (trav_info_t*) malloc( nobjects1 * sizeof(trav_info_t));
        if ( info1 == NULL )
        {
            printf("Error: Not enough memory for object list in file <%s>\n",fname1);
            options->err_stat=1;
            goto out;
        }
        
        h5trav_getinfo( file1_id, info1, 0 );
    }

    /*-------------------------------------------------------------------------
    * get the list of objects in file2
    *-------------------------------------------------------------------------
    */
    
    if ( nobjects2 )
    {
        
        info2 = (trav_info_t*) malloc( nobjects2 * sizeof(trav_info_t));
        if ( info2 == NULL )
        {
            printf("Error: Not enough memory for object list in file <%s>\n",fname2);
            options->err_stat=1;
            goto out;
        }
        
        h5trav_getinfo( file2_id, info2, 0 );
    }


    
   /*-------------------------------------------------------------------------
    * object name was supplied
    *-------------------------------------------------------------------------
    */
    if( objname1 ) 
    {
        assert(objname2);
        options->cmn_objs = 1; /* eliminate warning */
        nfound = diff_compare(file1_id, 
            fname1,
            objname1,
            nobjects1,
            info1,
            file2_id,
            fname2,
            objname2,
            nobjects2,
            info2,
            options);
     } /* end if */

   /*-------------------------------------------------------------------------
    * compare all
    *-------------------------------------------------------------------------
    */

    else 
    {
        nfound = diff_match(file1_id, 
                            nobjects1,
                            info1,
                            file2_id,
                            nobjects2,
                            info2,
                            options);
    } /* end else */
    
   
    
out:
    /* close */
    H5E_BEGIN_TRY 
    {
        H5Fclose(file1_id);
        H5Fclose(file2_id);
    } H5E_END_TRY;
    
    if (info1!=NULL) 
        h5trav_freeinfo(info1,nobjects1);
    if (info2!=NULL)
        h5trav_freeinfo(info2,nobjects2);
    
    
    return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_match
 *
 * Purpose: Find common objects; the algorithm used for this search is the
 *  cosequential match algorithm and is described in
 *  Folk, Michael; Zoellick, Bill. (1992). File Structures. Addison-Wesley.
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Modifications: 
 *
 * Aug 2008 
 *    Added a "contents" mode check.
 *    If this mode is present, objects in both files must match (must be exactly the same)
 *    If this does not happen, the tool returns an error code of 1 
 *    (instead of the success code of 0)
 *
 *-------------------------------------------------------------------------
 */
hsize_t diff_match(hid_t file1_id,
                   int nobjects1,
                   trav_info_t *info1,
                   hid_t file2_id,
                   int nobjects2,
                   trav_info_t *info2,
                   diff_opt_t *options)
{
    int           more_names_exist = (nobjects1>0 && nobjects2>0) ? 1 : 0;
    trav_table_t  *table = NULL;
    int           cmp;
    int           curr1=0;
    int           curr2=0;
    unsigned      infile[2];
    hsize_t       nfound = 0;
    unsigned      i;
    
   /*-------------------------------------------------------------------------
    * build the list
    *-------------------------------------------------------------------------
    */
    trav_table_init( &table );
    
    while ( more_names_exist )
    {
        /* criteria is string compare */
        cmp = strcmp( info1[curr1].name, info2[curr2].name );
        if ( cmp == 0 )
        {
            infile[0]=1; infile[1]=1;
            trav_table_addflags(infile, info1[curr1].name, info1[curr1].type, table );
            
            curr1++;
            curr2++;
        }
        else if ( cmp < 0 )
        {
            infile[0]=1; infile[1]=0;
            trav_table_addflags(infile, info1[curr1].name, info1[curr1].type, table );
            curr1++;
        }
        else
        {
            infile[0]=0; infile[1]=1;
            trav_table_addflags(infile, info2[curr2].name, info2[curr2].type, table );
            curr2++;
        }
        
        more_names_exist = (curr1<nobjects1 && curr2<nobjects2) ? 1 : 0;
        
        
    } /* end while */
    
    /* list1 did not end */
    if (curr1<nobjects1)
    {
        while ( curr1<nobjects1 )
        {
            infile[0]=1; infile[1]=0;
            trav_table_addflags(infile, info1[curr1].name, info1[curr1].type, table );
            curr1++;
        }
    }
    
    /* list2 did not end */
    if (curr2<nobjects2)
    {
        while ( curr2<nobjects2 )
        {
            infile[0]=0; infile[1]=1;
            trav_table_addflags(infile, info2[curr2].name, info2[curr2].type, table );
            curr2++;
        }
    }
    
   /*-------------------------------------------------------------------------
    * print the list
    *-------------------------------------------------------------------------
    */
    if(options->m_verbose) 
    {
        printf("\n");
        printf("file1     file2\n");
        printf("---------------------------------------\n");
        for(i = 0; i < table->nobjs; i++) {
            char c1, c2;

            c1 = (table->objs[i].flags[0]) ? 'x' : ' ';
            c2 = (table->objs[i].flags[1]) ? 'x' : ' ';
            printf("%5c %6c    %-15s\n", c1, c2, table->objs[i].name);
        } /* end for */
        printf ("\n");
    } /* end if */

    /*-------------------------------------------------------------------------
    * regarding the return value of h5diff (0, no difference in files, 1 difference )
    * 1) the number of objects in file1 must be the same as in file2
    * 2) the graph must match, i.e same names (absolute path)
    * 3) objects with the same name must be of the same type
    *-------------------------------------------------------------------------
    */     
    
    /* number of different objects */
    if ( nobjects1 != nobjects2 )
    {
        options->contents = 0;
    }
    
    /* objects in one file and not the other */
    for( i = 0; i < table->nobjs; i++) 
    {
        if( table->objs[i].flags[0] != table->objs[i].flags[1] ) 
        {
            options->contents = 0;
        }
    }

    /* objects with the same name but different HDF5 types */
    for( i = 0; i < table->nobjs; i++) 
    {
        if ( table->objs[i].flags[0] && table->objs[i].flags[1] )
        {
            if ( table->objs[i].type != table->objs[i].type )
            {
                options->contents = 0;
            }
        }
    }
        

    
    
   /*-------------------------------------------------------------------------
    * do the diff for common objects
    *-------------------------------------------------------------------------
    */
    
    for (i = 0; i < table->nobjs; i++)
    {
        if ( table->objs[i].flags[0] && table->objs[i].flags[1] )
        {
            options->cmn_objs=1;
            nfound+=diff( file1_id,
                table->objs[i].name,
                file2_id,
                table->objs[i].name,
                options,
                table->objs[i].type );
        }
    }
    
    /* free table */
    trav_table_free(table);
    
    
   /*-------------------------------------------------------------------------
    * do the diff for the root, it compares only the root group attributes
    *-------------------------------------------------------------------------
    */
    
    nfound+=diff( file1_id,
        "/",
        file2_id,
        "/",
        options,
        H5G_GROUP );
    
    
    return nfound;
}


/*-------------------------------------------------------------------------
 * Function: diff_compare
 *
 * Purpose: get objects from list, and check for the same type
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */

hsize_t diff_compare(hid_t file1_id,
                     const char *file1_name,
                     const char *obj1_name,
                     int nobjects1,
                     trav_info_t *info1,
                     hid_t file2_id,
                     const char *file2_name,
                     const char *obj2_name,
                     int nobjects2,
                     trav_info_t *info2,
                     diff_opt_t *options)
{
    int     f1 = 0;
    int     f2 = 0;
    hsize_t nfound = 0;
    
    int i = h5trav_getindex( obj1_name, nobjects1, info1 );
    int j = h5trav_getindex( obj2_name, nobjects2, info2 );
    
    if ( i == -1 )
    {
        printf( "Object <%s> could not be found in <%s>\n", obj1_name, file1_name );
        f1=1;
    }
    if ( j == -1 )
    {
        printf( "Object <%s> could not be found in <%s>\n", obj2_name, file2_name );
        f2=1;
    }
    if (f1 || f2)
    {
        options->err_stat = 1;
        return 0;
    }
    
    /* use the name with "/" first, as obtained by iterator function */
    obj1_name=info1[i].name;
    obj2_name=info2[j].name;
    
    /* objects are not the same type */
    if ( info1[i].type != info2[j].type)
    {
        if (options->m_verbose)
            printf("Comparison not possible: <%s> is of type %s and <%s> is of type %s\n",
            obj1_name, get_type(info1[i].type),
            obj2_name, get_type(info2[j].type) );
        options->not_cmp=1;
        return 0;
    }
    
    nfound = diff(file1_id, 
                  obj1_name, 
                  file2_id, 
                  obj2_name, 
                  options, 
                  info1[i].type );
    
    return nfound;
}


/*-------------------------------------------------------------------------
 * Function: diff
 *
 * Purpose: switch between types and choose the diff function
 * TYPE is either
 *  H5G_GROUP		  Object is a group
 *  H5G_DATASET 	  Object is a dataset
 *  H5G_TYPE          Object is a named data type
 *  H5G_LINK          Object is a symbolic link
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */

hsize_t diff (hid_t file1_id,
              const char *path1,
              hid_t file2_id, 
              const char *path2, 
              diff_opt_t * options, 
              H5G_obj_t1 type)
{
    hid_t      type1_id = (-1);
    hid_t      type2_id = (-1);
    hid_t      grp1_id = (-1);
    hid_t      grp2_id = (-1);
    int        ret;
    H5G_stat_t sb1;
    H5G_stat_t sb2;
    hsize_t    nfound = 0;
    
    switch (type)
    {
    
   /*-------------------------------------------------------------------------
    * H5G_DATASET
    *-------------------------------------------------------------------------
    */
    case H5G_DATASET:
        
       /*-------------------------------------------------------------------------
        * verbose, always print name
        *-------------------------------------------------------------------------
        */
        if (options->m_verbose)
        {
            if (print_objname(options,(hsize_t)1))
                do_print_objname ("dataset", path1, path2);
            nfound=diff_dataset(file1_id,file2_id,path1,path2,options);
            print_found(nfound);
            
        }
       /*-------------------------------------------------------------------------
        * non verbose, check first if we have differences by enabling quiet mode
        * so that printing is off, and compare again if differences found,
        * disabling quite mode
        *-------------------------------------------------------------------------
        */
        else
        {
            if (options->m_quiet==0)
            {
                /* shut up temporarily */
                options->m_quiet=1;
                nfound=diff_dataset(file1_id,file2_id,path1,path2,options);
                /* print again */
                options->m_quiet=0;
                if (nfound)
                {
                    if (print_objname(options,nfound))
                        do_print_objname ("dataset", path1, path2);
                    nfound=diff_dataset(file1_id,file2_id,path1,path2,options);
                    print_found(nfound);
                } 
            } 
            /* in quiet mode, just count differences */
            else
            {
                nfound=diff_dataset(file1_id,file2_id,path1,path2,options);
            }
        }
        
        break;
        
   /*-------------------------------------------------------------------------
    * H5G_TYPE
    *-------------------------------------------------------------------------
    */
    case H5G_TYPE:

        if ((type1_id = H5Topen(file1_id, path1))<0)
            goto out;
        if ((type2_id = H5Topen(file2_id, path2))<0)
            goto out;
        
        if ((ret = H5Tequal(type1_id,type2_id))<0)
            goto out;
        
        /* if H5Tequal is > 0 then the datatypes refer to the same datatype */
        nfound = (ret>0) ? 0 : 1;
        
        if (print_objname(options,nfound))
            do_print_objname ("datatype", path1, path2);

        /* always print the number of differences found in verbose mode */
        if (options->m_verbose)
            print_found(nfound);
        
        /*-------------------------------------------------------------------------
         * compare attributes
         * the if condition refers to cases when the dataset is a referenced object
         *-------------------------------------------------------------------------
         */
        if (path1)
            nfound += diff_attr(type1_id,type2_id,path1,path2,options);
        
        if ( H5Tclose(type1_id)<0)
            goto out;
        if ( H5Tclose(type2_id)<0)
            goto out;
        
        break;
        
   /*-------------------------------------------------------------------------
    * H5G_GROUP
    *-------------------------------------------------------------------------
    */
    case H5G_GROUP:

        ret = HDstrcmp(path1,path2);
    
        /* if "path1" != "path2" then the groups are "different" */
        nfound = (ret!=0) ? 1 : 0;
        
        if (print_objname(options,nfound))
            do_print_objname ("group", path1, path2);

        /* always print the number of differences found in verbose mode */
        if (options->m_verbose)
            print_found(nfound);
        
        if ((grp1_id = H5Gopen(file1_id, path1))<0)
            goto out;
        if ((grp2_id = H5Gopen(file2_id, path2))<0)
            goto out;
        /*-------------------------------------------------------------------------
         * compare attributes
         * the if condition refers to cases when the dataset is a referenced object
         *-------------------------------------------------------------------------
         */
        if (path1)
            nfound += diff_attr(grp1_id,grp2_id,path1,path2,options);
        
        if ( H5Gclose(grp1_id)<0)
            goto out;
        if ( H5Gclose(grp2_id)<0)
            goto out;
        
        break;
        
        
   /*-------------------------------------------------------------------------
    * H5G_LINK
    *-------------------------------------------------------------------------
    */
    case H5G_LINK:
        {
            char *buf1 = NULL;
            char *buf2 = NULL;
            
            if (H5Gget_objinfo (file1_id, path1, FALSE, &sb1) < 0)
                goto out;
            if (H5Gget_objinfo (file1_id, path1, FALSE, &sb2) < 0)
                goto out;
            
            buf1 = HDmalloc (sb1.linklen);
            buf2 = HDmalloc (sb2.linklen);
            
            if (H5Gget_linkval (file1_id, path1, sb1.linklen, buf1) < 0)
                goto out;
            if (H5Gget_linkval (file2_id, path2, sb1.linklen, buf2) < 0)
                goto out;
            
            ret = HDstrcmp (buf1, buf2);
            
            /* if "buf1" != "buf2" then the links are "different" */
            nfound = (ret != 0) ? 1 : 0;
            
            if (print_objname (options, nfound))
                do_print_objname ("link", path1, path2);

            /* always print the number of differences found in verbose mode */
            if (options->m_verbose)
                print_found(nfound);
            
            HDfree (buf1);
            HDfree (buf2);
        }
        break;
        
    default:
        if (options->m_verbose) 
        {
            printf("Comparison not supported: <%s> and <%s> are of type %s\n",
                path1, path2, get_type(type) );
        }
        options->not_cmp=1;
        break;
 }
 
 
 return nfound;
 
out:
    options->err_stat = 1;

    /* close */
    /* disable error reporting */
    H5E_BEGIN_TRY {
        H5Tclose(type1_id);
        H5Tclose(type2_id);
        H5Gclose(grp1_id);
        H5Tclose(grp2_id);
        /* enable error reporting */
    } H5E_END_TRY;

    return nfound;
}


