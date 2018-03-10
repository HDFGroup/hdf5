/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5FaccProp.h"
#include "H5FcreatProp.h"
#include "H5OcreatProp.h"
#include "H5DcreatProp.h"
#include "H5DxferProp.h"
#include "H5LaccProp.h"
#include "H5Location.h"
#include "H5Object.h"
#include "H5AbstractDs.h"
#include "H5DataSpace.h"
#include "H5DataSet.h"
#include "H5CommonFG.h"
#include "H5Attribute.h"
#include "H5Group.h"
#include "H5File.h"
#include "H5Alltypes.h"

namespace H5 {
    using std::cerr;
    using std::endl;

//--------------------------------------------------------------------------
// Function:    Group default constructor
///\brief       Default constructor: creates a stub Group.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Group::Group() : H5Object(), CommonFG(), id(H5I_INVALID_HID) {}

//--------------------------------------------------------------------------
// Function:    Group copy constructor
///\brief       Copy constructor: makes a copy of the original Group object.
///\param       original - IN: Original group to copy
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Group::Group(const Group& original) : H5Object(), CommonFG(), id(original.id)
{
    incRefCount(); // increment number of references to this id
}

//--------------------------------------------------------------------------
// Function:    Group::closeObjId
///\brief       Closes an object, which was opened with Group::getObjId
///\exception   H5::FileIException or H5::GroupIException
// Programmer   Binh-Minh Ribler - March, 2017
//--------------------------------------------------------------------------
void Group::closeObjId(hid_t obj_id) const
{
    herr_t ret_value = H5Oclose(obj_id);
    if (ret_value < 0)
    {
        throwException("Group::closeObjId", "H5Oclose failed");
    }
}

/*** For H5L API ***/

//--------------------------------------------------------------------------
// Function:    Group::newLink
///\brief       Creates a soft link from \a link_name to \a target_name.
///\param       target_name - IN: Name of object, can be a non-existing object
///\param       link_name   - IN: Link name for the target name
///\param       lcpl - IN: Link creation plist - default to PropList::DEFAULT
///\param       lapl - IN: Link access plist - default to PropList::DEFAULT
///\exception   H5::FileIException or H5::GroupIException
///\par Description
///             Note that both names are interpreted relative to the current
///             location.
///             For information on creating a soft link, please refer to the
///             H5Lcreate_soft APIs in the HDF5 C Reference Manual.
//  March 2018
//--------------------------------------------------------------------------
void Group::newLink(const char *target_name, const char *link_name,
             const PropList& lcpl, const PropList& lapl) const
{
    herr_t ret_value = -1;
    hid_t lcpl_id = lcpl.getId();
    hid_t lapl_id = lapl.getId();

    ret_value = H5Lcreate_soft(target_name, id, link_name, lcpl_id, lapl_id);
    if (ret_value < 0)
        throwException("newLink", "creating soft link failed");
}

//--------------------------------------------------------------------------
// Function:    Group::newLink
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a target_name and \a link_name.
///\exception   H5::FileIException or H5::GroupIException
// March, 2018
//--------------------------------------------------------------------------
void Group::newLink(const H5std_string& target_name, const H5std_string&
             link_name, const PropList& lcpl, const PropList& lapl) const
{
    newLink(target_name.c_str(), link_name.c_str(), lcpl, lapl);
}

//--------------------------------------------------------------------------
// Function:    Group::newLink
///\brief       Creates a hard link from \a new_name to \a curr_name.
///\param       curr_name - IN: Name of the existing object
///\param       new_name  - IN: New name for the object
///\param       lcpl - IN: Link creation plist - default to PropList::DEFAULT
///\param       lapl - IN: Link access plist - default to PropList::DEFAULT
///\exception   H5::FileIException or H5::GroupIException
///\par Description
///             Note that both names are interpreted relative to the
///             specified location.
///             For information on creating a hard link, please refer to the
///             H5Lcreate_hard APIs in the HDF5 C Reference Manual.
//  March 2018
//--------------------------------------------------------------------------
void Group::newLink(const char *curr_name, const Group& new_loc,
             const char *new_name, const PropList& lcpl, const PropList& lapl) const
{
    herr_t ret_value = -1;
    hid_t new_loc_id = new_loc.getId();
    hid_t lcpl_id = lcpl.getId();
    hid_t lapl_id = lapl.getId();

    ret_value = H5Lcreate_hard(getId(), curr_name, new_loc.getId(), new_name, H5P_DEFAULT, H5P_DEFAULT);
   if (ret_value < 0)
        throwException("newLink", "creating link failed");
}

//--------------------------------------------------------------------------
// Function:    Group::newLink
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a curr_name and \a new_name.
///\exception   H5::FileIException or H5::GroupIException
// March, 2018
//--------------------------------------------------------------------------
void Group::newLink(const H5std_string& curr_name, const Group& new_loc,
             const H5std_string& new_name, const PropList& lcpl, const PropList& lapl) const
{
    newLink(curr_name.c_str(), new_loc, new_name.c_str(), lcpl, lapl);
}

//--------------------------------------------------------------------------
// Function:    Group::newLink
///\brief       Creates a hard link from \a new_name to \a curr_name - can be
///             used to pass in H5L_SAME_LOC.
///\param       curr_name - IN: Name of the existing object
///\param       new_name  - IN: New name for the object
///\param       lcpl - IN: Link creation plist - default to PropList::DEFAULT
///\param       lapl - IN: Link access plist - default to PropList::DEFAULT
///\exception   H5::FileIException or H5::GroupIException
///\par Description
///             Note that both names are interpreted relative to the
///             specified location.
///             For information on creating a hard link, please refer to the
///             H5Lcreate_hard APIs in the HDF5 C Reference Manual.
//  March 2018
//--------------------------------------------------------------------------
void Group::newLink(const char *curr_name, const hid_t same_loc,
             const char *new_name, const PropList& lcpl, const PropList& lapl) const
{
    herr_t ret_value = -1;
    hid_t lcpl_id = lcpl.getId();
    hid_t lapl_id = lapl.getId();

    ret_value = H5Lcreate_hard(getId(), curr_name, same_loc, new_name, H5P_DEFAULT, H5P_DEFAULT);

   if (ret_value < 0)
        throwException("newLink", "creating link failed");
}

//--------------------------------------------------------------------------
// Function:    Group::newLink
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a curr_name and \a new_name.
///\exception   H5::FileIException or H5::GroupIException
// March, 2018
//--------------------------------------------------------------------------
void Group::newLink(const H5std_string& curr_name, const hid_t same_loc,
             const H5std_string& new_name, const PropList& lcpl, const PropList& lapl) const
{
    newLink(curr_name.c_str(), same_loc, new_name.c_str(), lcpl, lapl);
}


//--------------------------------------------------------------------------
// Function:    Group::copyLink
///\brief       Copies a link from one location to another.
///\param       src      - IN: Source location
///\param       src_name - IN: Original name
///\param       dst      - IN: Destination location
///\param       dst_name - IN: New name
///\param       lcpl     - IN: Link creation plist - default PropList::DEFAULT
///\param       lapl     - IN: Link access plist - default PropList::DEFAULT
///\exception   H5::FileIException or H5::GroupIException
// March, 2018
//--------------------------------------------------------------------------
void Group::copyLink(const char *src_name,
        const Group& dst, const char *dst_name, const PropList& lcpl,
        const PropList& lapl) const
{
    herr_t ret_value;
    hid_t dst_id = dst.getId();
    hid_t lcpl_id = lcpl.getId();
    hid_t lapl_id = lapl.getId();

    ret_value = H5Lcopy(getId(), src_name, dst_id, dst_name, lcpl_id, lapl_id);
    if(ret_value < 0)
        throwException("copyLink", "H5Lcopy failed");
}

//--------------------------------------------------------------------------
// Function:    Group::copyLink
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a src_name and \a dst_name.
///\exception   H5::FileIException or H5::GroupIException
// March, 2018
//--------------------------------------------------------------------------
void Group::copyLink(const H5std_string& src_name,
        const Group& dst, const H5std_string& dst_name, const PropList& lcpl,
        const PropList& lapl) const
{
    copyLink(src_name.c_str(), dst, dst_name.c_str(), lcpl, lapl);
}

//--------------------------------------------------------------------------
// Function:    Group::copyLink
///\brief       Copies a link to the same location.
///\param       src      - IN: Source location
///\param       src_name - IN: Original name
///\param       dst_name - IN: New name
///\param       lcpl     - IN: Link creation plist - default PropList::DEFAULT
///\param       lapl     - IN: Link access plist - default PropList::DEFAULT
///\exception   H5::FileIException or H5::GroupIException
// March, 2018
//--------------------------------------------------------------------------
void Group::copyLink(const char *src_name,
        const char *dst_name, const PropList& lcpl,
        const PropList& lapl) const
{
    herr_t ret_value;
    hid_t lcpl_id = lcpl.getId();
    hid_t lapl_id = lapl.getId();

    ret_value = H5Lcopy(getId(), src_name, H5L_SAME_LOC, dst_name, lcpl_id, lapl_id);
    if(ret_value < 0)
        throwException("copyLink", "H5Lcopy H5L_SAME_LOC failed");
}

//--------------------------------------------------------------------------
// Function:    Group::copyLink
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a src_name and \a dst_name.
///\exception   H5::FileIException or H5::GroupIException
// March, 2018
//--------------------------------------------------------------------------
void Group::copyLink(const H5std_string& src_name,
        const H5std_string& dst_name, const PropList& lcpl,
        const PropList& lapl) const
{
    copyLink(src_name.c_str(), dst_name.c_str(), lcpl, lapl);
}

//--------------------------------------------------------------------------
// Function:    Group::moveLink
///\brief       Renames an object in a group/file and moves it to a new location.
///\param       src      - IN: Source location
///\param       src_name - IN: Original name
///\param       dst      - IN: Destination location
///\param       dst_name - IN: New name
///\param       lcpl     - IN: Link creation plist - default PropList::DEFAULT
///\param       lapl     - IN: Link access plist - default PropList::DEFAULT
///\exception   H5::FileIException or H5::GroupIException
///\note
///             Exercise care in moving groups as it is possible to render
///             data in a file inaccessible with Group::moveLink. Please refer
///             to the Group Interface in the HDF5 User's Guide for details.
// March, 2018
//--------------------------------------------------------------------------
void Group::moveLink(const char* src_name, const Group& dst, const char* dst_name, const PropList& lcpl, const PropList& lapl) const
{
    herr_t ret_value;
    hid_t dst_id = dst.getId();
    hid_t lcpl_id = lcpl.getId();
    hid_t lapl_id = lapl.getId();

    ret_value = H5Lmove(getId(), src_name, dst_id, dst_name, lcpl_id, lapl_id);
    if (ret_value < 0)
        throwException("moveLink", "H5Lmove failed");
}

//--------------------------------------------------------------------------
// Function:    Group::moveLink
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a src_name and \a dst_name.
///\exception   H5::FileIException or H5::GroupIException
// March, 2018
//--------------------------------------------------------------------------
void Group::moveLink(const H5std_string& src_name, const Group& dst, const H5std_string& dst_name, const PropList& lcpl, const PropList& lapl) const
{
    moveLink(src_name.c_str(), dst, dst_name.c_str(), lcpl, lapl);
}

//--------------------------------------------------------------------------
// Function:    Group::moveLink
///\brief       Renames an object in a group or file to the same location.
///\param       src      - IN: Source location
///\param       src_name - IN: Original name
///\param       dst_name - IN: New name
///\param       lcpl     - IN: Link creation plist - default PropList::DEFAULT
///\param       lapl     - IN: Link access plist - default PropList::DEFAULT
///\exception   H5::FileIException or H5::GroupIException
///\note
///             Exercise care in moving groups as it is possible to render
///             data in a file inaccessible with Group::moveLink. Please refer
///             to the Group Interface in the HDF5 User's Guide for details.
// March, 2018
//--------------------------------------------------------------------------
void Group::moveLink(const char* src_name, const char* dst_name, const PropList& lcpl, const PropList& lapl) const
{
    herr_t ret_value;
    hid_t lcpl_id = lcpl.getId();
    hid_t lapl_id = lapl.getId();

    ret_value = H5Lmove(getId(), src_name, H5L_SAME_LOC, dst_name, lcpl_id, lapl_id);
    if (ret_value < 0)
        throwException("moveLink", "H5Lmove H5L_SAME_LOC failed");
}

//--------------------------------------------------------------------------
// Function:    Group::moveLink
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a src_name and \a dst_name.
///\exception   H5::FileIException or H5::GroupIException
// March, 2018
//--------------------------------------------------------------------------
void Group::moveLink(const H5std_string& src_name, const H5std_string& dst_name, const PropList& lcpl, const PropList& lapl) const
{
    moveLink(src_name.c_str(), H5L_SAME_LOC, dst_name.c_str(), lcpl, lapl);
}

/*** End of H5L API section ***/

//--------------------------------------------------------------------------
// Function:    Group::getLocId
// Purpose:     Get the id of this group
// Programmer   Binh-Minh Ribler - 2000
// Description
//              This function is a redefinition of CommonFG::getLocId.  It
//              is used by CommonFG member functions to get the file id.
// Deprecated:
//      Aug 18, 2016 -BMR
//              After HDFFV-9920, the Group's methods can use getId() and
//              getLocId() is kept for backward compatibility.
//--------------------------------------------------------------------------
hid_t Group::getLocId() const
{
    return(getId());
}

//--------------------------------------------------------------------------
// Function:    Group overloaded constructor
///\brief       Creates a Group object using the id of an existing group.
///\param       existing_id - IN: Id of an existing group
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Group::Group(const hid_t existing_id) : H5Object(), CommonFG(), id(existing_id)
{
    incRefCount(); // increment number of references to this id
}

//--------------------------------------------------------------------------
// Function:    Group overload constructor - dereference
///\brief       Given a reference, ref, to an hdf5 group, creates a Group object
///\param       loc - IN: Specifying location referenced object is in
///\param       ref - IN: Reference pointer
///\param       ref_type - IN: Reference type - default to H5R_OBJECT
///\param       plist - IN: Property list - default to PropList::DEFAULT
///\exception   H5::ReferenceException
///\par Description
///             \c obj can be DataSet, Group, or named DataType, that
///             is a datatype that has been named by DataType::commit.
// Programmer   Binh-Minh Ribler - Oct, 2006
//--------------------------------------------------------------------------
Group::Group(const H5Location& loc, const void* ref, H5R_type_t ref_type, const PropList& plist) : H5Object(), CommonFG(), id(H5I_INVALID_HID)
{
    id = H5Location::p_dereference(loc.getId(), ref, ref_type, plist, "constructor - by dereference");
}

//--------------------------------------------------------------------------
// Function:    Group::getNumObjs
///\brief       Returns the number of objects in this group.
///\return      Number of objects
///\exception   H5::FileIException or H5::GroupIException
// Programmer   Binh-Minh Ribler - January, 2003
//--------------------------------------------------------------------------
hsize_t Group::getNumObjs() const
{
    H5G_info_t ginfo;      // Group information

    herr_t ret_value = H5Gget_info(getId(), &ginfo);
    if(ret_value < 0)
        throwException("getNumObjs", "H5Gget_info failed");
    return (ginfo.nlinks);
}

//--------------------------------------------------------------------------
// Function:    Group::getObjId
///\brief       Opens an object via object header.
///\param       obj_name - IN: Path to the object
///\param       plist    - IN: Access property list for the link pointing to
///                            the object
///\exception   H5::FileIException or H5::GroupIException
///\par Description
///             This function opens an object in a group or file, using
///             H5Oopen.  Thus, an object can be opened without knowing
///             the object's type.
// Programmer   Binh-Minh Ribler - March, 2017
//--------------------------------------------------------------------------
hid_t Group::getObjId(const char* obj_name, const PropList& plist) const
{
    hid_t ret_value = H5Oopen(getId(), obj_name, plist.getId());
    if (ret_value < 0)
    {
        throwException("Group::getObjId", "H5Oopen failed");
    }
    return(ret_value);
}

//--------------------------------------------------------------------------
// Function:    Group::getObjId
///\brief       This is an overloaded member function, provided for convenience.
///             It takes a reference to a \c H5std_string for the object's name.
///\param       obj_name - IN: Path to the object
///\param       plist    - IN: Access property list for the link pointing to
///                            the object
///\exception   H5::FileIException or H5::GroupIException
// Programmer   Binh-Minh Ribler - March, 2017
//--------------------------------------------------------------------------
hid_t Group::getObjId(const H5std_string& obj_name, const PropList& plist) const
{
    return(getObjId(obj_name.c_str(), plist));
}

//--------------------------------------------------------------------------
// Function:    Group::getId
///\brief       Get the id of this group
///\return      Group identifier
// Modification:
//      May 2008 - BMR
//              Class hierarchy is revised to address bugzilla 1068.  Class
//              AbstractDS and Attribute are moved out of H5Object.  In
//              addition, member IdComponent::id is moved into subclasses, and
//              IdComponent::getId now becomes pure virtual function.
// Programmer   Binh-Minh Ribler - May, 2008
//--------------------------------------------------------------------------
hid_t Group::getId() const
{
    return(id);
}

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// Function:    Group::p_setId
///\brief       Sets the identifier of this object to a new value.
///
///\exception   H5::IdComponentException when the attempt to close the HDF5
///             object fails
// Description:
//              The underlaying reference counting in the C library ensures
//              that the current valid id of this object is properly closed.
//              Then the object's id is reset to the new id.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void Group::p_setId(const hid_t new_id)
{
    // handling references to this old id
    try {
        close();
    }
    catch (Exception& close_error) {
        throwException("Group::p_setId", close_error.getDetailMsg());
    }
    // reset object's id to the given id
    id = new_id;
}
#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    Group::close
///\brief       Closes this group.
///
///\exception   H5::GroupIException
// Programmer   Binh-Minh Ribler - Mar 9, 2005
//--------------------------------------------------------------------------
void Group::close()
{
    if (p_valid_id(id))
    {
        herr_t ret_value = H5Gclose(id);
        if (ret_value < 0)
        {
            throwException("Group::close", "H5Gclose failed");
        }
        // reset the id
        id = H5I_INVALID_HID;
    }
}

//--------------------------------------------------------------------------
// Function:    Group::throwException
///\brief       Throws H5::GroupIException.
///\param       func_name - Name of the function where failure occurs
///\param       msg       - Message describing the failure
///\exception   H5::GroupIException
// Description
//              This function is also used in H5Location's methods so that
//              proper exception can be thrown for file or group.  The
//              "Group::" will be inserted to indicate the function called is
//              an implementation of Group.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void Group::throwException(const H5std_string& func_name, const H5std_string& msg) const
{
    H5std_string full_name = func_name;
    full_name.insert(0, "Group::");
    throw GroupIException(full_name, msg);
}

//--------------------------------------------------------------------------
// Function:    Group destructor
///\brief       Properly terminates access to this group.
// Programmer   Binh-Minh Ribler - 2000
// Modification
//              - Replaced resetIdComponent() with decRefCount() to use C
//              library ID reference counting mechanism - BMR, Feb 20, 2005
//              - Replaced decRefCount with close() to let the C library
//              handle the reference counting - BMR, Jun 1, 2006
//--------------------------------------------------------------------------
Group::~Group()
{
    try {
        close();
    }
    catch (Exception& close_error) {
        cerr << "Group::~Group - " << close_error.getDetailMsg() << endl;
    }
}

} // end namespace
