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

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5F_init_super_interface


/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5SMprivate.h"        /* Shared Object Header Messages        */

/* PRIVATE PROTOTYPES */


/*--------------------------------------------------------------------------
NAME
   H5F_init_super_interface -- Initialize interface-specific information
USAGE
    herr_t H5F_init_super_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5F_init() currently).

--------------------------------------------------------------------------*/
static herr_t
H5F_init_super_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5F_init_super_interface)

    FUNC_LEAVE_NOAPI(H5F_init())
} /* H5F_init_super_interface() */


/*-------------------------------------------------------------------------
 * Function:    H5F_read_superblock
 *
 * Purpose:     Reads the superblock from the file or from the BUF. If
 *              ADDR is a valid address, then it reads it from the file.
 *              If not, then BUF must be non-NULL for it to read from the
 *              BUF.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Bill Wendling
 *              wendling@ncsa.uiuc.edu
 *              Sept 12, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_read_superblock(H5F_t *f, hid_t dxpl_id, H5G_loc_t *root_loc, haddr_t addr, uint8_t *buf, size_t buf_size)
{
    haddr_t             stored_eoa;         /*relative end-of-addr in file  */
    haddr_t             eof;                /*end of file address           */
    size_t              sizeof_addr = 0;
    size_t              sizeof_size = 0;
    const size_t        fixed_size = H5F_SUPERBLOCK_FIXED_SIZE; /*fixed sizeof superblock   */
    unsigned            sym_leaf_k = 0;
    size_t              variable_size;      /*variable sizeof superblock    */
    unsigned            btree_k[H5B_NUM_BTREE_ID];  /* B-tree internal node 'K' values */
    H5F_file_t         *shared = NULL;      /* shared part of `file'        */
    H5FD_t             *lf = NULL;          /* file driver part of `shared' */
    uint8_t            *p;                  /* Temporary pointer into encoding buffers */
    uint8_t            *start_p;            /* Start of encoding buffers    */
    uint32_t            read_chksum;        /* Checksum read from file  */
    uint32_t            computed_chksum;    /* Computed checksum  */
    size_t              driver_size;        /* Size of driver info block, in bytes */
    char                driver_name[9];     /* Name of driver, for driver info block */
    unsigned            super_vers;         /* Super block version          */
    unsigned            freespace_vers;     /* Freespace info version       */
    unsigned            obj_dir_vers;       /* Object header info version   */
    unsigned            share_head_vers;    /* Shared header info version   */
    uint8_t             sbuf[H5F_MAX_SUPERBLOCK_SIZE];     /* Local buffer                 */
    unsigned            nindexes;           /* Number of shared message indexes */
    H5P_genplist_t     *c_plist;            /* File creation property list  */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5F_read_superblock, FAIL)

    /* Short cuts */
    shared = f->shared;
    lf = shared->lf;

    /* Get the shared file creation property list */
    if (NULL == (c_plist = H5I_object(shared->fcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get property list")

    /* Read the superblock if it hasn't been read before. */
    if (addr == HADDR_UNDEF) {
        if (HADDR_UNDEF == (shared->super_addr=H5F_locate_signature(lf,dxpl_id)))
            HGOTO_ERROR(H5E_FILE, H5E_NOTHDF5, FAIL, "unable to find file signature")
    } else {
        shared->super_addr = addr;
    }

    if (!buf) {
        start_p = p = sbuf;
        buf_size=sizeof(sbuf);

        if (H5FD_set_eoa(lf, H5FD_MEM_SUPER, shared->super_addr + fixed_size) < 0 ||
                H5FD_read(lf, H5FD_MEM_SUPER, dxpl_id, shared->super_addr, fixed_size, p) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_READERROR, FAIL, "unable to read superblock")
    } else {
        start_p = p = buf;
    }

    /* Signature, already checked */
    p += H5F_SIGNATURE_LEN;

    /* Superblock version */
    super_vers = *p++;
    if (super_vers > HDF5_SUPERBLOCK_VERSION_MAX)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "bad superblock version number")
    if (H5P_set(c_plist, H5F_CRT_SUPER_VERS_NAME, &super_vers) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set superblock version")

    /* Freespace version */
    freespace_vers = *p++;
    if (HDF5_FREESPACE_VERSION != freespace_vers)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "bad free space version number")
    if (H5P_set(c_plist, H5F_CRT_FREESPACE_VERS_NAME, &freespace_vers) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set free space version")

    /* Root group version number */
    obj_dir_vers = *p++;
    if (HDF5_OBJECTDIR_VERSION != obj_dir_vers)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "bad object directory version number")
    if (H5P_set(c_plist, H5F_CRT_OBJ_DIR_VERS_NAME, &obj_dir_vers) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set object directory version")

    /* Skip over reserved byte */
    p++;

    /* Shared header version number */
    share_head_vers = *p++;
    if (HDF5_SHAREDHEADER_VERSION != share_head_vers)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "bad shared-header format version number")
    if (H5P_set(c_plist, H5F_CRT_SHARE_HEAD_VERS_NAME, &share_head_vers) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set shared-header format version")

    /* Size of file addresses */
    sizeof_addr = *p++;
    if (sizeof_addr != 2 && sizeof_addr != 4 &&
            sizeof_addr != 8 && sizeof_addr != 16 && sizeof_addr != 32)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "bad byte number in an address")
    if (H5P_set(c_plist, H5F_CRT_ADDR_BYTE_NUM_NAME,&sizeof_addr) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set byte number in an address")
    shared->sizeof_addr = sizeof_addr;  /* Keep a local copy also */

    /* Size of file sizes */
    sizeof_size = *p++;
    if (sizeof_size != 2 && sizeof_size != 4 &&
            sizeof_size != 8 && sizeof_size != 16 && sizeof_size != 32)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "bad byte number for object size")
    if (H5P_set(c_plist, H5F_CRT_OBJ_BYTE_NUM_NAME, &sizeof_size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set byte number for object size")
    shared->sizeof_size = sizeof_size;  /* Keep a local copy also */

    /* Skip over reserved byte */
    p++;

    /* Various B-tree sizes */
    UINT16DECODE(p, sym_leaf_k);
    if (sym_leaf_k == 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADRANGE, FAIL, "bad symbol table leaf node 1/2 rank")
    if (H5P_set(c_plist, H5F_CRT_SYM_LEAF_NAME, &sym_leaf_k) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set rank for symbol table leaf nodes")
    shared->sym_leaf_k = sym_leaf_k;    /* Keep a local copy also */

    /* Need 'get' call to set other array values */
    if (H5P_get(c_plist, H5F_CRT_BTREE_RANK_NAME, btree_k) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get rank for btree internal nodes")
    UINT16DECODE(p, btree_k[H5B_SNODE_ID]);
    if (btree_k[H5B_SNODE_ID] == 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADRANGE, FAIL, "bad 1/2 rank for btree internal nodes")
    /*
     * Delay setting the value in the property list until we've checked
     * for the indexed storage B-tree internal 'K' value later.
     */

    /* File consistency flags. Not really used yet */
    UINT32DECODE(p, shared->consist_flags);
    assert(((size_t)(p - start_p)) == fixed_size);

    /* Decode the variable-length part of the superblock... */
    if(super_vers == HDF5_SUPERBLOCK_VERSION_DEF)
        variable_size = H5F_SUPERBLOCK_VARLEN_SIZE_V0(f);
    else if (super_vers == HDF5_SUPERBLOCK_VERSION_1)
        variable_size = H5F_SUPERBLOCK_VARLEN_SIZE_V1(f);
    else
        variable_size = H5F_SUPERBLOCK_VARLEN_SIZE_V2(f);

    assert(fixed_size + variable_size <= buf_size);

    /* The buffer (buf) is either passed in or the "local_buf" variable now */
    if(!buf) {
        if (H5FD_set_eoa(lf, H5FD_MEM_SUPER, shared->super_addr + fixed_size+variable_size) < 0 ||
                H5FD_read(lf, H5FD_MEM_SUPER, dxpl_id, shared->super_addr + fixed_size,
                          variable_size, p) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to read superblock")
    } /* end if */

    /*
     * If the superblock version # is greater than 0, read in the indexed
     * storage B-tree internal 'K' value
     */
    if (super_vers > HDF5_SUPERBLOCK_VERSION_DEF) {
        UINT16DECODE(p, btree_k[H5B_ISTORE_ID]);
        /* Reserved bytes are present only in version 1 */
        if(super_vers == HDF5_SUPERBLOCK_VERSION_1) {
            p += 2;   /* reserved */
        }
    }
    else
        btree_k[H5B_ISTORE_ID] = HDF5_BTREE_ISTORE_IK_DEF;

    /* Set the B-tree internal node values, etc */
    if (H5P_set(c_plist, H5F_CRT_BTREE_RANK_NAME, btree_k) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set rank for btree internal nodes")
    HDmemcpy(shared->btree_k, btree_k, sizeof(unsigned) * (size_t)H5B_NUM_BTREE_ID);    /* Keep a local copy also */

    H5F_addr_decode(f, (const uint8_t **)&p, &shared->base_addr/*out*/);
    H5F_addr_decode(f, (const uint8_t **)&p, &(shared->extension_addr)/*out*/);

    /* If the superblock version is greater than 1, read in the shared OH message table information */
#ifdef JAMES
    if(super_vers >= HDF5_SUPERBLOCK_VERSION_2) {
        H5F_addr_decode(f, (const uint8_t **)&p, &shared->sohm_addr/*out*/);
        shared->sohm_vers = *p++;
        shared->sohm_nindexes = *p++;
    }
#endif /* JAMES */

    H5F_addr_decode(f, (const uint8_t **)&p, &stored_eoa/*out*/);
    H5F_addr_decode(f, (const uint8_t **)&p, &shared->driver_addr/*out*/);
    if(H5G_obj_ent_decode(f, (const uint8_t **)&p, root_loc->oloc/*out*/) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to read root symbol entry")

    /* Compute super block checksum for versions greater than 1 */
    if(super_vers >= HDF5_SUPERBLOCK_VERSION_2) {
        computed_chksum = H5_checksum_metadata(sbuf, (unsigned) (p - sbuf), 0);
        /* Read stored super block checksum */
        UINT32DECODE(p, read_chksum);

        if(read_chksum != computed_chksum)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "bad checksum on superblock")
    }

    HDassert((unsigned) (p - sbuf) == H5F_SUPERBLOCK_SIZE(super_vers, f));

    /*
     * Check if superblock address is different from base address and
     * adjust base address and "end of address" address if so.
     */
    if (!H5F_addr_eq(shared->super_addr,shared->base_addr)) {
        /* Check if the superblock moved earlier in the file */
        if (H5F_addr_lt(shared->super_addr, shared->base_addr))
            stored_eoa -= (shared->base_addr - shared->super_addr);
        else
            /* The superblock moved later in the file */
            stored_eoa += (shared->super_addr - shared->base_addr);

        shared->base_addr = shared->super_addr;
    } /* end if */


    /* This step is for h5repart tool only. If user wants to change file driver from
     * family to sec2 while using h5repart, set the driver address to undefined to let
     * the library ignore the family driver information saved in the superblock.
     */
    if(shared->fam_to_sec2)
        shared->driver_addr = HADDR_UNDEF;

    /* Decode the optional driver information block */
    if (H5F_addr_defined(shared->driver_addr)) {
        unsigned drv_vers;
        haddr_t drv_addr = shared->base_addr + shared->driver_addr;
        uint8_t dbuf[H5F_MAX_DRVINFOBLOCK_SIZE];     /* Local buffer                 */
        size_t dbuf_size;               /* Size available for driver info */
        const uint8_t *driver_p;        /* Remember beginning of driver info block */


        if(!buf) {
            driver_p = p = dbuf;
            dbuf_size=sizeof(dbuf);

            if (H5FD_set_eoa(lf, H5FD_MEM_SUPER, drv_addr + H5F_DRVINFOBLOCK_HDR_SIZE) < 0 ||
                    H5FD_read(lf, H5FD_MEM_SUPER, dxpl_id, drv_addr, (size_t)H5F_DRVINFOBLOCK_HDR_SIZE, p) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to read driver information block")
        } /* end if */
        else {
            driver_p = p;
            dbuf_size=buf_size-(p-start_p);
        } /* end else */

        /* Version number */
        drv_vers = *p++;
        if (drv_vers > HDF5_DRIVERINFO_VERSION_MAX)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "bad driver information block version number")

        p += 3; /* reserved bytes */

        /* Driver info size */
        UINT32DECODE(p, driver_size);

        /* Driver name and/or version */
        HDstrncpy(driver_name, (const char *)p, (size_t)8);
        driver_name[8] = '\0';
        p += 8; /* advance past name/version */

        HDassert(p - dbuf == H5F_DRVINFOBLOCK_HDR_SIZE);

        /* Read driver information */
        HDassert((driver_size + H5F_DRVINFOBLOCK_HDR_SIZE + H5F_DRVINFO_CHKSUM(drv_vers)) <= dbuf_size);

        if(!buf) {
            if (H5FD_set_eoa(lf, H5FD_MEM_SUPER, drv_addr + H5F_DRVINFOBLOCK_HDR_SIZE + driver_size + H5F_DRVINFO_CHKSUM(drv_vers)) < 0 ||
                    H5FD_read(lf, H5FD_MEM_SUPER, dxpl_id, drv_addr+H5F_DRVINFOBLOCK_HDR_SIZE, driver_size, p) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to read file driver information")
        } /* end if */

        /* Decode driver information */
        /* Check if driver matches driver information saved. Unfortunately, we can't push this
         * function to each specific driver because we're checking if the driver is correct.*/
        if(!HDstrncmp(driver_name, "NCSAfami", (size_t)8) && HDstrcmp(lf->cls->name, "family"))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "family driver should be used")
        if(!HDstrncmp(driver_name, "NCSAmult", (size_t)8) && HDstrcmp(lf->cls->name, "multi"))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "multi driver should be used")

        if (H5FD_sb_decode(lf, driver_name, p) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to decode driver information")

        p += driver_size;

        /* Compute checksum for versions > 0*/
        if(drv_vers >= HDF5_DRIVERINFO_VERSION_1) {
            computed_chksum = H5_checksum_metadata(dbuf, (H5F_DRVINFOBLOCK_HDR_SIZE + driver_size), 0);
            /* Read checksum */
            UINT32DECODE(p, read_chksum);

            if(read_chksum != computed_chksum)
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "bad checksum on driver information block")
        }
 
        HDassert((unsigned) (p - dbuf) == driver_size + H5F_DRVINFOBLOCK_HDR_SIZE + H5F_DRVINFO_CHKSUM(drv_vers));
    } /* end if */

    /*
     * The user-defined data is the area of the file before the base
     * address.
     */
    if (H5P_set(c_plist, H5F_CRT_USER_BLOCK_NAME, &shared->base_addr) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set usr block size")

    /*
     * Make sure that the data is not truncated. One case where this is
     * possible is if the first file of a family of files was opened
     * individually.
     */
    if (HADDR_UNDEF == (eof = H5FD_get_eof(lf)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to determine file size")

    if (eof < stored_eoa)
        HGOTO_ERROR(H5E_FILE, H5E_TRUNCATED, FAIL, "truncated file")

    /*
     * Tell the file driver how much address space has already been
     * allocated so that it knows how to allocate additional memory.
     */
    if (H5FD_set_eoa(lf, H5FD_MEM_SUPER, stored_eoa) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to set end-of-address marker for file")

    /* Read the file's superblock extension, if there is one. */
    if(shared->extension_addr != HADDR_UNDEF && super_vers >= HDF5_SUPERBLOCK_VERSION_2) {
        H5O_loc_t ext_loc;
/* JAMES        H5O_shmesg_table_t sohm_table; */

        H5O_loc_reset(&ext_loc);
        ext_loc.file = f;
        ext_loc.addr = shared->extension_addr;

        /* JAMES: bump the number of open objects to avoid closing the file here */
        f->nopen_objs++;
        if(H5O_open(&ext_loc) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENFILE, FAIL, "unable to open superblock extension")

        if(NULL == H5O_msg_read(&ext_loc, H5O_SHMESG_ID, &shared->sohm_table, dxpl_id)) {
            H5E_clear_stack(NULL);
            shared->sohm_addr = HADDR_UNDEF;
            shared->sohm_nindexes = 0;
            shared->sohm_vers = 0;
        } else {
            shared->sohm_addr = shared->sohm_table.addr;
            shared->sohm_vers = shared->sohm_table.version;
            shared->sohm_nindexes = shared->sohm_table.nindexes;
        }
        /* JAMES
        HDassert(sohm_table.addr == shared->sohm_addr);
        HDassert(sohm_table.version == shared->sohm_vers);
        HDassert(sohm_table.nindexes == shared->sohm_nindexes);
*/
        if(H5O_close(&ext_loc) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENFILE, FAIL, "unable to close superblock extension")
        f->nopen_objs--;
    }

    /* Decode shared object header message information and store it in the
     * fcpl */
    if(shared->sohm_addr != HADDR_UNDEF)
    {
        unsigned index_flags[H5O_SHMESG_MAX_NINDEXES] = {0};
        unsigned minsizes[H5O_SHMESG_MAX_NINDEXES] = {0};
        unsigned sohm_l2b;           /* SOHM list-to-btree cutoff    */
        unsigned sohm_b2l;           /* SOHM btree-to-list cutoff    */

        HDassert(shared->sohm_nindexes > 0 && shared->sohm_nindexes <= H5O_SHMESG_MAX_NINDEXES);

        /* Read in the shared OH message information if there is any */
        if(H5SM_get_info(f, index_flags, minsizes, &sohm_l2b, &sohm_b2l, dxpl_id) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to read SOHM table information")

        /* Set values in the property list */
        if(H5P_set(c_plist, H5F_CRT_SHMSG_NINDEXES_NAME, &(shared->sohm_nindexes)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set number of SOHM indexes");
        if(H5P_set(c_plist, H5F_CRT_SHMSG_INDEX_TYPES_NAME, index_flags) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set type flags for indexes");
        if(H5P_set(c_plist, H5F_CRT_SHMSG_INDEX_MINSIZE_NAME, minsizes) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set type flags for indexes");
        if(H5P_set(c_plist, H5F_CRT_SHMSG_LIST_MAX_NAME, &sohm_l2b) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set SOHM cutoff in property list");
        if(H5P_set(c_plist, H5F_CRT_SHMSG_BTREE_MIN_NAME, &sohm_b2l) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set SOHM cutoff in property list");
    }
    else
    {
        /* Shared object header messages are disabled */
        nindexes = 0;
        if(H5P_set(c_plist, H5F_CRT_SHMSG_NINDEXES_NAME, &nindexes) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set number of SOHM indexes");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_read_superblock() */


/*-------------------------------------------------------------------------
 * Function:    H5F_init_superblock
 *
 * Purpose:     Allocates the superblock for the file and initializes
 *              information about the superblock in memory.  Does not write
 *              any superblock information to the file.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Sept 15, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5F_init_superblock(const H5F_t *f, hid_t dxpl_id)
{
    hsize_t         userblock_size = 0;         /* Size of userblock, in bytes */
    size_t          superblock_size;            /* Size of superblock, in bytes     */
    size_t          driver_size;                /* Size of driver info block (bytes)*/
    unsigned        super_vers;                 /* Super block version              */
    haddr_t         addr;                       /* Address of superblock            */
    H5P_genplist_t *plist;                      /* Property list                    */
    hsize_t         ret_value;

    FUNC_ENTER_NOAPI(H5F_init_superblock, UFAIL)

    /* Get the shared file creation property list */
    if (NULL == (plist = H5I_object(f->shared->fcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, UFAIL, "not a property list")

    /*
     * The superblock starts immediately after the user-defined
     * header, which we have already insured is a proper size. The
     * base address is set to the same thing as the superblock for
     * now.
     */
    if(H5P_get(plist, H5F_CRT_USER_BLOCK_NAME, &userblock_size) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, UFAIL, "unable to get user block size")
    f->shared->super_addr = userblock_size;
    f->shared->base_addr = f->shared->super_addr;
    f->shared->consist_flags = 0x03;

    /* Grab superblock version from property list */
    if (H5P_get(plist, H5F_CRT_SUPER_VERS_NAME, &super_vers) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, UFAIL, "unable to get super block version")

    /* Compute the size of the superblock */
    superblock_size=H5F_SUPERBLOCK_FIXED_SIZE;

    if(super_vers==HDF5_SUPERBLOCK_VERSION_DEF)
        superblock_size += H5F_SUPERBLOCK_VARLEN_SIZE_V0(f);
    else if (super_vers == HDF5_SUPERBLOCK_VERSION_1)
        superblock_size += H5F_SUPERBLOCK_VARLEN_SIZE_V1(f);
    else if (super_vers == HDF5_SUPERBLOCK_VERSION_2)
        superblock_size += H5F_SUPERBLOCK_VARLEN_SIZE_V2(f);

    /* Compute the size of the driver information block. Use driver block
     * version 1 if the superblock is at least version 2 */
    H5_ASSIGN_OVERFLOW(driver_size, H5FD_sb_size(f->shared->lf), hsize_t, size_t);
    if (driver_size > 0)
    {
        driver_size += H5F_DRVINFOBLOCK_HDR_SIZE;
        if(super_vers > HDF5_SUPERBLOCK_VERSION_1)
            driver_size += H5F_DRVINFO_CHKSUM(1);
        else
            driver_size += H5F_DRVINFO_CHKSUM(0);
    }

    /*
     * Allocate space for the userblock, superblock, driver info
     * block, and shared object header message table. We do it with
     * one allocation request because the userblock and superblock
     * need to be at the beginning of the file and only the first
     * allocation request is required to return memory at format 
     * address zero.
     */

    H5_CHECK_OVERFLOW(f->shared->base_addr, haddr_t, hsize_t);
    addr = H5FD_alloc(f->shared->lf, H5FD_MEM_SUPER, dxpl_id,
                      ((hsize_t)f->shared->base_addr + superblock_size + driver_size));

    if (HADDR_UNDEF == addr)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, UFAIL,
                    "unable to allocate file space for userblock and/or superblock")

    if (0 != addr)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, UFAIL,
                    "file driver failed to allocate userblock and/or superblock at address zero")

    /*
     * The file driver information block begins immediately after the
     * superblock.
     */
    if (driver_size > 0)
        f->shared->driver_addr = superblock_size;

    /* Return the size of the super block+driver info block */
    ret_value=superblock_size+driver_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_init_superblock() */


/*-------------------------------------------------------------------------
 * Function:    H5F_write_superblock
 *
 * Purpose:     Writes (and optionally allocates) the superblock for the file.
 *              If BUF is non-NULL, then write the serialized superblock
 *              information into it. It should be a buffer of size
 *              H5F_SUPERBLOCK_SIZE + H5F_DRVINFOBLOCK_SIZE
 *              or larger.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Bill Wendling
 *              wendling@ncsa.uiuc.edu
 *              Sept 12, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_write_superblock(H5F_t *f, hid_t dxpl_id)
{
    uint8_t         sbuf[H5F_MAX_SUPERBLOCK_SIZE];  /* Superblock encoding buffer   */
    uint8_t         dbuf[H5F_MAX_DRVINFOBLOCK_SIZE];/* Driver info block encoding buffer*/
    uint8_t        *p = NULL;                   /* Ptr into encoding buffers        */
    uint32_t        chksum;                     /* Checksum temporary variable      */
    size_t          superblock_size;            /* Size of superblock, in bytes     */
    size_t          driver_size;                /* Size of driver info block (bytes)*/
    char            driver_name[9];             /* Name of driver, for driver info block */
    unsigned        super_vers;                 /* Super block version              */
    unsigned        freespace_vers;             /* Freespace info version           */
    unsigned        obj_dir_vers;               /* Object header info version       */
    unsigned        share_head_vers;            /* Shared header info version       */
    H5P_genplist_t *plist;                      /* Property list                    */
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5F_write_superblock, FAIL)

    /* Get the shared file creation property list */
    if (NULL == (plist = H5I_object(f->shared->fcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Grab values from property list */
    if (H5P_get(plist, H5F_CRT_SUPER_VERS_NAME, &super_vers) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get super block version")
    if (H5P_get(plist, H5F_CRT_FREESPACE_VERS_NAME, &freespace_vers) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get free space version")
    if (H5P_get(plist, H5F_CRT_OBJ_DIR_VERS_NAME, &obj_dir_vers) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get object directory version")
    if (H5P_get(plist, H5F_CRT_SHARE_HEAD_VERS_NAME, &share_head_vers) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get shared-header format version")

    /* Encode the file super block */
    p = sbuf;
    HDmemcpy(p, H5F_SIGNATURE, (size_t)H5F_SIGNATURE_LEN);
    p += H5F_SIGNATURE_LEN;
    *p++ = (uint8_t)super_vers;
    *p++ = (uint8_t)freespace_vers;
    *p++ = (uint8_t)obj_dir_vers;
    *p++ = 0;   /* reserved*/

    *p++ = (uint8_t)share_head_vers;
    assert (H5F_SIZEOF_ADDR(f) <= 255);
    *p++ = (uint8_t)H5F_SIZEOF_ADDR(f);
    assert (H5F_SIZEOF_SIZE(f) <= 255);
    *p++ = (uint8_t)H5F_SIZEOF_SIZE(f);
    *p++ = 0;   /* reserved */

    UINT16ENCODE(p, f->shared->sym_leaf_k);
    UINT16ENCODE(p, f->shared->btree_k[H5B_SNODE_ID]);
    UINT32ENCODE(p, f->shared->consist_flags);

    /*
     * Versions of the superblock >0 have the indexed storage B-tree
     * internal 'K' value stored
     */
    if (super_vers > HDF5_SUPERBLOCK_VERSION_DEF) {
        UINT16ENCODE(p, f->shared->btree_k[H5B_ISTORE_ID]);
        if (super_vers < HDF5_SUPERBLOCK_VERSION_2) {
            *p++ = 0;   /*reserved */
            *p++ = 0;   /*reserved */
        }
    }

    H5F_addr_encode(f, &p, f->shared->base_addr);
    H5F_addr_encode(f, &p, f->shared->extension_addr);

#ifdef JAMES
    if(super_vers >= HDF5_SUPERBLOCK_VERSION_2) {
      H5F_addr_encode(f, &p, f->shared->sohm_addr);
      *p++ = f->shared->sohm_vers;
      *p++ = f->shared->sohm_nindexes;
    }
#endif /* JAMES */

    H5F_addr_encode(f, &p, H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER));
    H5F_addr_encode(f, &p, f->shared->driver_addr);
    if(H5G_obj_ent_encode(f, &p, H5G_oloc(f->shared->root_grp))<0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to encode root group information")

    /* Compute super block checksum */
    if(super_vers >= HDF5_SUPERBLOCK_VERSION_2) {
        chksum = H5_checksum_metadata(sbuf, H5F_SUPERBLOCK_SIZE(super_vers, f) - H5F_SIZEOF_CHKSUM, 0);
        UINT32ENCODE(p, chksum);
    }

    /* Double check we didn't overrun the block (unlikely) */
    H5_ASSIGN_OVERFLOW(superblock_size, p - sbuf, int, size_t);
    HDassert(superblock_size <= sizeof(sbuf));

    /* Double check that the superblock is the right size */
    HDassert(superblock_size == (H5F_SUPERBLOCK_SIZE(super_vers, f)));

    /* Write superblock */
    if (H5FD_write(f->shared->lf, H5FD_MEM_SUPER, dxpl_id,
                    f->shared->super_addr, superblock_size, sbuf) < 0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "unable to write superblock")


    /* Encode the driver information block. */
    H5_ASSIGN_OVERFLOW(driver_size, H5FD_sb_size(f->shared->lf), hsize_t, size_t);

    if (driver_size > 0) {
        unsigned driver_vers;

        /* Use driver info block version 1 on superblock version >= 2 */
        if(super_vers > HDF5_SUPERBLOCK_VERSION_1) {
            driver_vers = HDF5_DRIVERINFO_VERSION_1;
        } else {
            driver_vers = HDF5_DRIVERINFO_VERSION_0;
        }

        driver_size += H5F_DRVINFOBLOCK_HDR_SIZE + H5F_DRVINFO_CHKSUM(driver_vers);

        /* Double check we didn't overrun the block (unlikely) */
	HDassert(driver_size <= sizeof(dbuf));

        /* Encode the driver information block */
	p = dbuf;

	*p++ = driver_vers; /* Version */
        *p++ = 0; /* reserved */
        *p++ = 0; /* reserved */
        *p++ = 0; /* reserved */

	/* Driver info size, excluding header and checksum */
	UINT32ENCODE(p, driver_size - H5F_DRVINFOBLOCK_HDR_SIZE - H5F_DRVINFO_CHKSUM(driver_vers));

	/* Encode driver-specific data */
	if (H5FD_sb_encode(f->shared->lf, driver_name, dbuf + H5F_DRVINFOBLOCK_HDR_SIZE) < 0)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to encode driver information")

	/* Driver name */
	HDmemcpy(dbuf + 8, driver_name, (size_t)8);

        /* Compute driver info block checksum on versions > 0 */
        if(driver_vers > HDF5_DRIVERINFO_VERSION_0) {
            chksum = H5_checksum_metadata(dbuf, (driver_size - H5F_SIZEOF_CHKSUM), 0);
            UINT32ENCODE(p, chksum);
        }

        /* Write driver information block */
        if (H5FD_write(f->shared->lf, H5FD_MEM_SUPER, dxpl_id,
                        f->shared->base_addr + f->shared->driver_addr, driver_size, dbuf) < 0)
            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "unable to write driver information block")
    } /* end if */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_write_superblock() */

