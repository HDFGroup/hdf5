// C++ informative line for the emacs editor: -*- C++ -*-
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5FilterParam_H
#define H5FilterParam_H

#include <cstdint>
#include <vector>
#include "H5Include.h"
#include "H5Exception.h"

namespace H5 {

/*! \class FilterParam
    \brief Utility class for parsing filter configuration parameter strings.

    All methods are static; there is no instance state.
    Wraps H5Zconfig_has_key, H5Zconfig_get_int, H5Zconfig_get_double,
    H5Zconfig_get_bool, and H5Zconfig_get_str as overloaded
    \p config_get_param methods.
*/
class FilterParam {
  public:
    ///\brief Returns true if \p key is present in \p params, false if absent.
    ///\exception H5::LibraryIException on error.
    static bool
    config_has_key(const H5std_string &params, const H5std_string &key)
    {
        htri_t ret = H5Zconfig_has_key(params.c_str(), key.c_str());
        if (ret < 0)
            throw LibraryIException("FilterParam::config_has_key", "H5Zconfig_has_key failed");
        return ret > 0;
    }

    ///\brief Retrieves a 64-bit integer parameter from \p params.
    ///\return true if found, false if absent.
    ///\exception H5::LibraryIException on error.
    static bool
    config_get_param(const H5std_string &params, const H5std_string &key, int64_t &value)
    {
        htri_t ret = H5Zconfig_get_int(params.c_str(), key.c_str(), &value);
        if (ret < 0)
            throw LibraryIException("FilterParam::config_get_param", "H5Zconfig_get_int failed");
        return ret > 0;
    }

    ///\brief Retrieves a double parameter from \p params.
    ///\return true if found, false if absent.
    ///\exception H5::LibraryIException on error.
    static bool
    config_get_param(const H5std_string &params, const H5std_string &key, double &value)
    {
        htri_t ret = H5Zconfig_get_double(params.c_str(), key.c_str(), &value);
        if (ret < 0)
            throw LibraryIException("FilterParam::config_get_param", "H5Zconfig_get_double failed");
        return ret > 0;
    }

    ///\brief Retrieves a boolean parameter from \p params.
    ///\return true if found, false if absent.
    ///\exception H5::LibraryIException on error.
    static bool
    config_get_param(const H5std_string &params, const H5std_string &key, bool &value)
    {
        hbool_t c_val = false;
        htri_t  ret   = H5Zconfig_get_bool(params.c_str(), key.c_str(), &c_val);
        if (ret < 0)
            throw LibraryIException("FilterParam::config_get_param", "H5Zconfig_get_bool failed");
        if (ret > 0)
            value = (c_val != false);
        return ret > 0;
    }

    ///\brief Retrieves a string parameter from \p params.
    ///\return true if found, false if absent.
    ///\exception H5::LibraryIException on error.
    static bool
    config_get_param(const H5std_string &params, const H5std_string &key, H5std_string &value)
    {
        size_t buf_size = 0;
        htri_t ret      = H5Zconfig_get_str(params.c_str(), key.c_str(), nullptr, &buf_size);
        if (ret < 0)
            throw LibraryIException("FilterParam::config_get_param", "H5Zconfig_get_str failed");
        if (ret == 0)
            return false;
        size_t            capacity = buf_size + 1;
        std::vector<char> buf(capacity, '\0');
        ret = H5Zconfig_get_str(params.c_str(), key.c_str(), buf.data(), &capacity);
        if (ret < 0)
            throw LibraryIException("FilterParam::config_get_param", "H5Zconfig_get_str failed");
        value = H5std_string(buf.data());
        return true;
    }

#ifndef DOXYGEN_SHOULD_SKIP_THIS
  private:
    FilterParam()              = delete;
    ~FilterParam()             = default;
    FilterParam(FilterParam &) = delete;
#endif // DOXYGEN_SHOULD_SKIP_THIS
};

} // namespace H5

#endif // H5FilterParam_H
