
#include <hdf5.h>
#include <H5private.h>

typedef struct shared_obj_t{
unsigned long objno[2];
char objname[1024];
int objflag;
} shared_obj_t;

typedef struct table_t{
int size;
int nobjs;
shared_obj_t *objs;
} table_t;

int prefix_len = 1024;
char *prefix;
table_t group_table, dset_table, type_table;

typedef herr_t (*H5G_operator_t)(hid_t, const char*, void*);

extern void init_table(void);
extern void free_table(void);
extern int search_obj (table_t, unsigned long *);
extern void add_obj (table_t *, unsigned long *, char *);
extern void dump_tables(void);
extern herr_t find_shared_objs(hid_t , char *, void *);
extern herr_t H5findobj_once(hid_t , char *, void *);
extern int get_table_idx(int, unsigned long *);
extern int get_tableflag(int, int);
extern int set_tableflag(int, int);
extern char* get_objectname(int, int);

/*-------------------------------------------------------------------------
 * Function:    init_table
 *
 * Purpose:     allocate and initialize tables for shared groups, datasets, 
 *              and committed types
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
void
init_table (void){
int i;

    group_table.size = dset_table.size = type_table.size = 20;
    group_table.nobjs = dset_table.nobjs = type_table.nobjs = 0;

    group_table.objs = (shared_obj_t*) HDmalloc(group_table.size*sizeof(shared_obj_t));
    dset_table.objs = (shared_obj_t*) HDmalloc(dset_table.size*sizeof(shared_obj_t));
    type_table.objs = (shared_obj_t*) HDmalloc(type_table.size*sizeof(shared_obj_t));

    for (i = 0; i < group_table.size; i++) {
         group_table.objs[i].objno[0] = group_table.objs[i].objno[1] = 0;
         group_table.objs[i].objflag = 0;
    }

    for (i = 0; i < dset_table.size; i++) {
         dset_table.objs[i].objno[0] = dset_table.objs[i].objno[1] = 0;
         dset_table.objs[i].objflag = 0;
    }

    for (i = 0; i < type_table.size; i++) {
         dset_table.objs[i].objno[0] = dset_table.objs[i].objno[1] = 0;
         dset_table.objs[i].objflag = 0;
    }

    prefix = (char *) HDmalloc(prefix_len * sizeof (char));
    strcpy(prefix, "");

}

/*-------------------------------------------------------------------------
 * Function:    free_table
 *
 * Purpose:     free tables for shared groups, datasets, 
 *              and committed types
 *
 * Return:      void
 *
 * Programmer:  Paul Harten
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
void
free_table (void){

	HDfree(&group_table);
	HDfree(&dset_table);
	HDfree(&type_table);

}


/*-------------------------------------------------------------------------
 * Function:    search_obj
 *
 * Purpose:     search the object specified by objno in the table
 *
 * Return:      an integer, the location of the object
 *              -1   if object is not found
 *
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
int 
search_obj (table_t table, unsigned long *objno) {
int i=0, found=0;

    while (i < table.nobjs && !found) 
           if (table.objs[i].objno[0] == *(objno) &&
               table.objs[i].objno[1] == *(objno+1) )  found = 1;
           else     i++; 
  
    if (!found) return -1;
    else return i;

}


/*-------------------------------------------------------------------------
 * Function:    add_obj
 *
 * Purpose:     add a shared object to the table
 *              realloc the table if necessary
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
void
add_obj (table_t *table, unsigned long *objno, char *objname) {
int i;

    if (table->nobjs == table->size) {
        table->size *= 2;
        table->objs = HDrealloc (table->objs, table->size*sizeof(shared_obj_t));
        for (i = table->nobjs; i < table->size; i++) {
             table->objs[i].objno[0] = table->objs[i].objno[1] = 0;
             table->objs[i].objflag = 0;
        }
    }

    i = table->nobjs++;
    table->objs[i].objno[0] = *objno;
    table->objs[i].objno[1] = *(objno+1);
    strcpy (table->objs[i].objname, objname);

}


/*-------------------------------------------------------------------------
 * Function:    dump_tables
 *
 * Purpose:     display the contents of tables for debugging purposes
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
void 
dump_tables(void) {
int i;

    printf("group_table: # of entries = %d\n", group_table.nobjs);
    for ( i = 0; i < group_table.nobjs; i++)
        printf ("%lu %lu %s %d\n", group_table.objs[i].objno[0],
                                   group_table.objs[i].objno[1],
                                   group_table.objs[i].objname,
                                   group_table.objs[i].objflag);

    printf("\ndset_table: # of entries = %d\n", dset_table.nobjs);
    for ( i = 0; i < dset_table.nobjs; i++)
        printf ("%lu %lu %s %d\n", dset_table.objs[i].objno[0],
                                   dset_table.objs[i].objno[1],
                                   dset_table.objs[i].objname,
                                   dset_table.objs[i].objflag);
   
    printf("\ntype_table: # of entries = %d\n", type_table.nobjs);
    for ( i = 0; i < type_table.nobjs; i++)
        printf ("%lu %lu %s %d\n", type_table.objs[i].objno[0],
                                   type_table.objs[i].objno[1],
                                   type_table.objs[i].objname,
                                   type_table.objs[i].objflag);
}


/*-------------------------------------------------------------------------
 * Function:   Find_shared_objs 
 *
 * Purpose:    Find shared objects, committed types and store them in tables
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications: Paul Harten
 *
 *-----------------------------------------------------------------------*/
herr_t
find_shared_objs(hid_t group, char *name, void __unused__ *op_data)
{
hid_t obj, type;
H5G_stat_t statbuf;
char *tmp;
int status=SUCCEED;
int i;

    H5Gget_objinfo(group, name, TRUE, &statbuf);

    tmp = (char *) HDmalloc ((strlen(prefix)+strlen(name)+2) * sizeof(char));

    strcpy(tmp, prefix); 

    switch (statbuf.type) {

    case H5G_GROUP:
        if ((obj=H5Gopen (group, name))>=0) {

            if (prefix_len < (int)(strlen(prefix) + strlen(name) + 2)) {
                prefix_len *= 2;
                prefix = HDrealloc (prefix, prefix_len * sizeof(char));
            } 
            strcat(strcat(prefix,"/"), name);

            if (statbuf.nlink > 1) {
                if (search_obj (group_table,  statbuf.objno) < 0) {
                    add_obj (&group_table, statbuf.objno, prefix); 
                    status = H5Giterate (obj, ".", NULL, (H5G_operator_t)find_shared_objs, NULL);
                }
            } else 
                status = H5Giterate (obj, ".", NULL, (H5G_operator_t)find_shared_objs, NULL);

            strcpy(prefix, tmp);
            H5Gclose (obj);

        } else 
            status = FAIL;

        break;

    case H5G_DATASET:

        strcat(tmp,"/");
        strcat(tmp,name); /* absolute name of the data set */
        if (statbuf.nlink > 1  && 
            search_obj (dset_table, statbuf.objno) < 0)
            add_obj (&dset_table, statbuf.objno, tmp);

        if ((obj=H5Dopen (group, name))>=0) {              
             type = H5Dget_type (obj);
             if (H5Tcommitted(type) > 0 ) {
                 H5Gget_objinfo(type, ".", TRUE, &statbuf);
                 if (search_obj (type_table, statbuf.objno) < 0) {
                     add_obj (&type_table, statbuf.objno, tmp) ;
                     type_table.objs[type_table.nobjs-1].objflag = 0;
                 }
             }
             H5Tclose(type);
             H5Dclose (obj);
            status = SUCCEED;
        } else
            status = FAIL;
            
        break;

    case H5G_TYPE:
         strcat(tmp,"/");
         strcat(tmp,name); /* absolute name of the type */
         i = search_obj (type_table, statbuf.objno);
         if (i < 0) {
             add_obj (&type_table, statbuf.objno, tmp) ;
             type_table.objs[type_table.nobjs-1].objflag = 1; /* named data type */
         } else {
             strcpy (type_table.objs[i].objname, tmp);
             type_table.objs[i].objflag = 1; /* named data type */
         }
            status = SUCCEED;
         break;

    default:
        break;
    }

    HDfree (tmp);

    return status;

}
/*-------------------------------------------------------------------------
 * Function:   H5findobj_once 
 *
 * Purpose:    Find objects only once and store them in tables
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Paul Harten
 *
 * Modifications: 
 *
 *-----------------------------------------------------------------------*/
herr_t
H5findobj_once(hid_t group, char *name, void __unused__ *op_data)
{
hid_t obj, type;
H5G_stat_t statbuf;
char *tmp;
int status=SUCCEED;
int i;

    H5Gget_objinfo(group, name, TRUE, &statbuf);

    tmp = (char *) HDmalloc ((strlen(prefix)+strlen(name)+2) * sizeof(char));

    strcpy(tmp, prefix); 

    switch (statbuf.type) {

    case H5G_GROUP:
        if ((obj=H5Gopen (group, name))>=0) {

            if (prefix_len < (int)(strlen(prefix) + strlen(name) + 2)) {
                prefix_len *= 2;
                prefix = HDrealloc (prefix, prefix_len * sizeof(char));
            } 
            strcat(strcat(prefix,"/"), name);

            if (search_obj (group_table,  statbuf.objno) < 0) {
                add_obj (&group_table, statbuf.objno, prefix); 
                status = H5Giterate (obj, ".", NULL, (H5G_operator_t)H5findobj_once, NULL);
            }

            strcpy(prefix, tmp);
            H5Gclose (obj);

        } else 
            status = FAIL;

        break;

    case H5G_DATASET:

        strcat(tmp,"/");
        strcat(tmp,name); /* absolute name of the data set */
        if (search_obj (dset_table, statbuf.objno) < 0)
            add_obj (&dset_table, statbuf.objno, tmp);

        if ((obj=H5Dopen (group, name))>=0) {              
             type = H5Dget_type (obj);
             if (H5Tcommitted(type) > 0 ) {
                 H5Gget_objinfo(type, ".", TRUE, &statbuf);
                 if (search_obj (type_table, statbuf.objno) < 0) {
                     add_obj (&type_table, statbuf.objno, tmp) ;
                     type_table.objs[type_table.nobjs-1].objflag = 0;
                 }
             }
             H5Tclose(type);
             H5Dclose (obj);
            status = SUCCEED;
        } else
            status = FAIL;
            
        break;

    case H5G_TYPE:
         strcat(tmp,"/");
         strcat(tmp,name); /* absolute name of the type */
         i = search_obj (type_table, statbuf.objno);
         if (i < 0) {
             add_obj (&type_table, statbuf.objno, tmp) ;
             type_table.objs[type_table.nobjs-1].objflag = 1; /* named data type */
         } else {
             strcpy (type_table.objs[i].objname, tmp);
             type_table.objs[i].objflag = 1; /* named data type */
         }
            status = SUCCEED;
         break;

    default:
        break;
    }

    HDfree (tmp);

    return status;

}
/*-------------------------------------------------------------------------
 * Function:   get_table_idx
 *
 * Purpose:    Determine if objects are in a link loop
 *
 * Return:      Success:        table index of object detected to be in loop
 *
 *              Failure:        FAIL
 *
 * Programmer:  Paul Harten
 *
 *-----------------------------------------------------------------------*/
int
get_table_idx(int type, unsigned long *objno)
{
int idx;

    switch (type) {

    case H5G_GROUP:

	idx = search_obj(group_table, objno);
        break;

    case H5G_DATASET:

	idx = search_obj(dset_table, objno);
        break;

    case H5G_TYPE:

	idx = search_obj(type_table, objno);
	break;

    default:

	idx = -1;

    }

    return idx;

}
/*-------------------------------------------------------------------------
 * Function:   Get table flag setting
 *
 * Purpose:    Keep the structures and variables used private to
 *             this file.
 *
 * Return:      Success:        Boolean setting of the i'th element of the
 *                              object table flag
 *
 *              Failure:        FAIL
 *
 * Programmer:  Paul Harten
 *
 *-----------------------------------------------------------------------*/
int
get_tableflag(int type, int idx)
{

    switch (type) {

    case H5G_GROUP:

	return group_table.objs[idx].objflag;

    case H5G_DATASET:

	return dset_table.objs[idx].objflag;

    case H5G_TYPE:

	return type_table.objs[idx].objflag;

    default:

	return -1;

    }

}
/*-------------------------------------------------------------------------
 * Function:   Set table flag setting
 *
 * Purpose:    Keep the structures and variables used private to
 *             this file.
 *
 * Return:      Success:        Boolean setting of the i'th element of the
 *                              object table flag
 *
 *              Failure:        FAIL
 *
 * Programmer:  Paul Harten
 *
 *-----------------------------------------------------------------------*/
int
set_tableflag(int type, int idx)
{

    switch (type) {

    case H5G_GROUP:

	group_table.objs[idx].objflag = TRUE;
	return SUCCEED;

    case H5G_DATASET:

	dset_table.objs[idx].objflag = TRUE;
	return SUCCEED;

    case H5G_TYPE:

	type_table.objs[idx].objflag = TRUE;
	return SUCCEED;

    default:

	return FAIL;

    }

}
/*-------------------------------------------------------------------------
 * Function:   Get name of i'th object in table
 *
 * Purpose:    
 *
 * Return:      Success:       strdup() of object name character string
 *
 *              Failure:       NULL
 *
 * Programmer:  Paul Harten
 *
 *-----------------------------------------------------------------------*/
char *
get_objectname(int type, int idx)
{

    switch (type) {

    case H5G_GROUP:

	return strdup(group_table.objs[idx].objname);

    case H5G_DATASET:

	return strdup(dset_table.objs[idx].objname);

    case H5G_TYPE:

	return strdup(type_table.objs[idx].objname);

    default:

	return NULL;

    }

}
