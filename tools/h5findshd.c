
#include <hdf5.h>
#include <H5private.h>
#include <h5tools.h>



typedef herr_t (*H5G_operator_t)(hid_t, const char*, void*);

extern void init_prefix(char **temp, int length);
extern void init_table(table_t **table);
extern void free_table(table_t **table);
extern void dump_table(char *name, table_t* table);
extern herr_t find_objs(hid_t group, const char *name, void *op_data);
extern int search_obj (table_t *temp, unsigned long *);
extern int get_table_idx(table_t *table, unsigned long *);
extern int get_tableflag(table_t*, int);
extern int set_tableflag(table_t*, int);
extern char* get_objectname(table_t*, int);


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
void init_table (table_t** temp){

	int i;
	table_t *table = malloc(sizeof(table_t));

    table->size = 20;
    table->nobjs = 0;

    table->objs = (obj_t*) malloc(table->size*sizeof(obj_t));
  

    for (i = 0; i < table->size; i++) {
         table->objs[i].objno[0] = table->objs[i].objno[1] = 0;
         table->objs[i].displayed = 0;
         table->objs[i].recorded = 0;
		 table->objs[i].objflag = 0;
    }
	*temp = table;

}


/*-------------------------------------------------------------------------
 * Function:    init_prefix
 *
 * Purpose:     allocate and initialize prefix
 *
 * Return:      void
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
void init_prefix(char **prefix, int prefix_len){
	char *temp;
    temp = (char *) malloc(prefix_len * sizeof (char));
    *temp = '\0';
	*prefix = temp;
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
free_table (table_t **table){

	table_t *temp = *table;
        if (temp->objs != NULL) {
		HDfree(temp->objs);
        }
	*table = temp;
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
search_obj (table_t *table, unsigned long *objno) {
int i=0, found=0;

    while (i < table->nobjs && !found) 
           if (table->objs[i].objno[0] == *(objno) &&
               table->objs[i].objno[1] == *(objno+1) )  found = 1;
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
        table->objs = realloc (table->objs, table->size*sizeof(obj_t));
        for (i = table->nobjs; i < table->size; i++) {
             table->objs[i].objno[0] = table->objs[i].objno[1] = 0;
             table->objs[i].displayed = 0;
             table->objs[i].recorded = 0;
			 table->objs[i].objflag = 0;
        }
    }

    i = table->nobjs++;
    table->objs[i].objno[0] = *objno;
    table->objs[i].objno[1] = *(objno+1);
    strcpy (table->objs[i].objname, objname);

}
/*-------------------------------------------------------------------------
 * Function:   Find_objs 
 *
 * Purpose:    Find objects, committed types and store them in tables
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
herr_t find_objs(hid_t group, const char *name, void *op_data)
{
	hid_t obj, type;
	H5G_stat_t statbuf;
	char *tmp;
	int i;
	find_objs_t *info = (find_objs_t*)op_data;

	if (info->threshold > 1) { /*will get an infinite loop if greater than 1*/
		return(FAIL);
	}

    H5Gget_objinfo(group, name, TRUE, &statbuf);

    tmp = (char *) malloc ((strlen(info->prefix)+strlen(name)+2) * sizeof(char));

    strcpy(tmp, info->prefix); 

    switch (statbuf.type) {

    case H5G_GROUP:
        if ((obj=H5Gopen (group, name))>=0) {

            if (info->prefix_len < (int)(strlen(info->prefix) + strlen(name) + 2)) {
                info->prefix_len *= 2;
                info->prefix = realloc (info->prefix, info->prefix_len * sizeof(char));
            } 
            strcat(strcat(info->prefix,"/"), name);

            if (statbuf.nlink > info->threshold) {
                if (search_obj (info->group_table,  statbuf.objno) < 0) {
                    add_obj (info->group_table, statbuf.objno, info->prefix); 
                    H5Giterate (obj, ".", NULL, find_objs, (void*)info);
                }
            } else 
                H5Giterate (obj, ".", NULL, find_objs, (void*)info);

            strcpy(info->prefix, tmp);
            H5Gclose (obj);

        } else 
            info->status = 1;

        break;

    case H5G_DATASET:

        strcat(tmp,"/");
        strcat(tmp,name); /* absolute name of the data set */
        if (statbuf.nlink > info->threshold  && 
            search_obj (info->dset_table, statbuf.objno) < 0)
            add_obj (info->dset_table, statbuf.objno, tmp);

        if ((obj=H5Dopen (group, name))>=0) {              
             type = H5Dget_type (obj);
             if (H5Tcommitted(type) > 0 ) {
                 H5Gget_objinfo(type, ".", TRUE, &statbuf);
                 if (search_obj (info->type_table, statbuf.objno) < 0) {
                     add_obj (info->type_table, statbuf.objno, tmp) ;
					 info->type_table->objs[info->type_table->nobjs - 1].objflag = 0;
				 }
             }
             H5Tclose(type);
             H5Dclose (obj);
        } else
             info->status = 1;
            
        break;

    case H5G_TYPE:
         strcat(tmp,"/");
         strcat(tmp,name); /* absolute name of the type */
         i = search_obj (info->type_table, statbuf.objno);
         if (i < 0) {
             add_obj (info->type_table, statbuf.objno, tmp) ;
             info->type_table->objs[info->type_table->nobjs-1].recorded = 1; /* named data type */
			 info->type_table->objs[info->type_table->nobjs-1].objflag = 1; /* named data type */
         } else {
             strcpy (info->type_table->objs[i].objname, tmp);
             info->type_table->objs[i].recorded = 1; 
			 info->type_table->objs[info->type_table->nobjs-1].objflag = 1; /* named data type */  
		 }
         break;

    default:
        break;
    }

    free (tmp);

    return SUCCEED;
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
dump_table(char* tablename, table_t *table) {
int i;

    printf("%s: # of entries = %d\n", tablename,table->nobjs);
    for ( i = 0; i < table->nobjs; i++)
        printf ("%lu %lu %s %d\n", table->objs[i].objno[0],
                                   table->objs[i].objno[1],
                                   table->objs[i].objname,
                                   table->objs[i].objflag);

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
get_table_idx(table_t *table, unsigned long *objno)
{
int idx = -1;
/*
    switch (type) {

    case H5G_GROUP:

	idx = search_obj(&group_table, objno);
        break;

    case H5G_DATASET:

	idx = search_obj(&dset_table, objno);
        break;

    case H5G_TYPE:

	idx = search_obj(&type_table, objno);
	break;

    default:

	idx = -1;

    }
*/
   idx = search_obj(table, objno);
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
get_tableflag(table_t *table, int idx)
{
  /*
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
  */
  return(table->objs[idx].objflag);
  

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
set_tableflag(table_t *table, int idx)
{
/*
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
*/
    table->objs[idx].objflag = TRUE;
    return(SUCCEED);
    

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
get_objectname(table_t* table, int idx)
{
  /*
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
  */

  return(strdup(table->objs[idx].objname));
}














