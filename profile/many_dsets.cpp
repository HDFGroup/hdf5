#include "hdf5.h"
#include <string>
#include <iostream>

static const char* const FILENAME = "fileWithLargeNumDsets.h5";

// Read this map as: 
// First column: Number of datasets to create
// Second column: Number of elements in the dataset
static const size_t dsetsAndNumElemsMap[3][2] { { 23792 * 40, 1 }, { 35285 * 40, 50 }, { 2173 * 40, 50000 } };

static const std::string GRP_NAME_BASE {"group_"};
static const std::string DSET_NAME_BASE{ "dset_" };

int main(int argc, char* argv[])
{
    hsize_t dsetDims[2];
    hid_t fcpl;
    H5AC_cache_config_t mdc_config =
        {.version = H5AC__CURR_CACHE_CONFIG_VERSION};

    fcpl = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_NONE, 0, (hsize_t)1);
    //H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, 0, (hsize_t)1);

    hid_t fileID = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT);
    if(fileID < 0)
    {
        std::cout << "Could not create file" << std::endl;
        return -1;
    }

    if (H5Fget_mdc_config(fileID, &mdc_config) == H5I_INVALID_HID) {
        std::cout << "Could not fetch MDC configuration" <<std::endl;
        return -1;
    }

    std::cout << "original MDC min size " << mdc_config.min_size << " max size " << mdc_config.max_size << std::endl;

    // mdc_config.min_size = 4096;
    mdc_config.max_size = 128 * 1024 * 1024;

    if (H5Fset_mdc_config(fileID, &mdc_config) == H5I_INVALID_HID) {
        std::cout << "Could not set MDC configuration" <<std::endl;
        return -1;
    }

    if (H5Fget_mdc_config(fileID, &mdc_config) == H5I_INVALID_HID) {
        std::cout << "Could not fetch MDC configuration" <<std::endl;
        return -1;
    }

    std::cout << "new MDC min size " << mdc_config.min_size << " max size " << mdc_config.max_size << std::endl;

    for(size_t cnt = 0; cnt < 3; cnt++)
    {
        std::string grpName = GRP_NAME_BASE + std::to_string(cnt);
        
        hid_t groupID = H5Gcreate(fileID, grpName.c_str(), H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if( groupID < 0 )
        {
            std::cout << "Could not create group: " << grpName <<std::endl;
            return -1;
        }

        // Specify the dataset dimensions
        dsetDims[0] = dsetsAndNumElemsMap[cnt][1];
        dsetDims[1] = 1;

        hid_t spaceID = H5Screate_simple(2, dsetDims, dsetDims);

        // Create some dummy data to write
        uint8_t* dataToWrite = new uint8_t[dsetDims[0]];
        for(size_t cntDsets = 0; cntDsets < dsetsAndNumElemsMap[cnt][0]; cntDsets++)
        {
            std::string dsetName = DSET_NAME_BASE + std::to_string(cntDsets);

            hid_t dsetID = H5Dcreate(groupID, dsetName.c_str(), H5T_NATIVE_UINT8, spaceID, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

            if(dsetID < 0)
            {
                std::cout << "Could not create dataset: " << dsetName << std::endl;
                return -1;
            }

            herr_t ret = H5Dwrite(dsetID, H5T_NATIVE_UINT8, H5S_ALL, H5S_ALL, H5P_DEFAULT, dataToWrite);
            if (ret < 0) {
                std::cout << "Could not write dataset: " << dsetName << std::endl;
                return -1;
            }

            if (H5Dclose(dsetID) < 0) {
                std::cout << "Could not close dataset: " << dsetName << std::endl;
                return -1;
	    }
        }

        H5Sclose(spaceID);
        if (H5Gclose(groupID) < 0) {
	    std::cout << "Could not close group: " << grpName << std::endl;
	    return -1;
	}
        delete[] dataToWrite;
    }

    H5Pclose(fcpl);
    H5Fclose(fileID);
    return 0;
}
