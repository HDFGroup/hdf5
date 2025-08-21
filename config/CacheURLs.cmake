#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# This is the URLCache file.
########################
# compression options
########################
set (ZLIB_PACKAGE_NAME "zlib" CACHE STRING "Name of ZLIB package")
mark_as_advanced (ZLIB_PACKAGE_NAME)
set (ZLIB_TGZ_NAME "zlib-1.3.1.tar.gz" CACHE STRING "Use HDF5_ZLib from compressed file")
set (ZLIB_TGZ_ORIGPATH "https://github.com/madler/zlib/releases/download/v1.3.1" CACHE STRING "Use ZLIB from original location")
mark_as_advanced (ZLIB_TGZ_NAME)
mark_as_advanced (ZLIB_TGZ_ORIGPATH)
set (ZLIB_GIT_URL "https://github.com/madler/zlib.git" CACHE STRING "Use ZLIB from  GitHub repository")
set (ZLIB_GIT_BRANCH "develop" CACHE STRING "")
mark_as_advanced (ZLIB_GIT_URL)
mark_as_advanced (ZLIB_GIT_BRANCH)

set (ZLIBNG_PACKAGE_NAME "zlib-ng" CACHE STRING "Name of ZLIBNG package")
mark_as_advanced (ZLIBNG_PACKAGE_NAME)
set (ZLIBNG_TGZ_NAME "2.2.4.tar.gz" CACHE STRING "Use HDF5_ZLib from compressed file")
set (ZLIBNG_TGZ_ORIGPATH "https://github.com/zlib-ng/zlib-ng/archive/refs/tags" CACHE STRING "Use ZLIBNG from original location")
mark_as_advanced (ZLIBNG_TGZ_NAME)
mark_as_advanced (ZLIBNG_TGZ_ORIGPATH)
set (ZLIBNG_GIT_URL "https://github.com/zlib-ng/zlib-ng.git" CACHE STRING "Use ZLIBNG from  GitHub repository")
set (ZLIBNG_GIT_BRANCH "develop" CACHE STRING "")
mark_as_advanced (ZLIBNG_GIT_URL)
mark_as_advanced (ZLIBNG_GIT_BRANCH)

set (LIBAEC_PACKAGE_NAME "libaec" CACHE STRING "Name of AEC SZIP package")
mark_as_advanced (LIBAEC_PACKAGE_NAME)
set (LIBAEC_TGZ_NAME "libaec-1.1.3.tar.gz" CACHE STRING "Use SZip AEC from compressed file")
set (LIBAEC_TGZ_ORIGPATH "https://github.com/MathisRosenhauer/libaec/releases/download/v1.1.3" CACHE STRING "Use LIBAEC from original location")
mark_as_advanced (LIBAEC_TGZ_NAME)
mark_as_advanced (LIBAEC_TGZ_ORIGPATH)
set (LIBAEC_GIT_URL "https://github.com/MathisRosenhauer/libaec.git" CACHE STRING "Use LIBAEC from  GitHub repository")
set (LIBAEC_GIT_BRANCH "v1.1.3" CACHE STRING "")
mark_as_advanced (LIBAEC_GIT_URL)
mark_as_advanced (LIBAEC_GIT_BRANCH)

########################
# API test options
########################
set (KWSYS_TGZ_ORIGPATH "https://gitlab.kitware.com/utils/kwsys/-/archive/master" CACHE STRING "Use KWSYS from original location")
set (KWSYS_TGZ_NAME "kwsys-master.tar.gz" CACHE STRING "Use KWSYS from original compressed file")
mark_as_advanced (KWSYS_TGZ_ORIGPATH)
mark_as_advanced (KWSYS_TGZ_NAME)

########################
# filter plugin options
########################

set (PLUGIN_TGZ_ORIGPATH "https://github.com/HDFGroup/hdf5_plugins/releases/download/snapshot" CACHE STRING "Use PLUGINS from original location")
set (PLUGIN_TGZ_NAME "hdf5_plugins-master.tar.gz" CACHE STRING "Use PLUGINS from compressed file")
mark_as_advanced (PLUGIN_TGZ_ORIGPATH)
mark_as_advanced (PLUGIN_TGZ_NAME)
set (PLUGIN_PACKAGE_NAME "pl" CACHE STRING "Name of PLUGIN package")
mark_as_advanced (PLUGIN_PACKAGE_NAME)
set (PLUGIN_GIT_URL "https://github.com/HDFGroup/hdf5_plugins.git" CACHE STRING "Use plugins from HDF Group repository")
set (PLUGIN_GIT_BRANCH "master" CACHE STRING "")
mark_as_advanced (PLUGIN_GIT_URL)
mark_as_advanced (PLUGIN_GIT_BRANCH)

###########
# bitgroom
###########
set (BITGROOM_PACKAGE_NAME "bitgroom" CACHE STRING "Name of BITGROOM package")
mark_as_advanced (BITGROOM_PACKAGE_NAME)

###########
# bitround
###########
set (BITROUND_PACKAGE_NAME "bitround" CACHE STRING "Name of BITROUND package")
mark_as_advanced (BITROUND_PACKAGE_NAME)

#############
# bitshuffle
#############

set (BSHUF_GIT_URL "https://github.com/kiyo-masui/bitshuffle.git" CACHE STRING "Use BSHUF from HDF repository")
set (BSHUF_GIT_BRANCH "master" CACHE STRING "")
mark_as_advanced (BSHUF_GIT_URL)
mark_as_advanced (BSHUF_GIT_BRANCH)

set (BSHUF_TGZ_ORIGPATH "https://github.com/kiyo-masui/bitshuffle/archive/refs/tags" CACHE STRING "Use PLUGINS from original location")
set (BSHUF_TGZ_NAME "bitshuffle-0.5.2.tar.gz" CACHE STRING "Use BSHUF from compressed file")
mark_as_advanced (BSHUF_TGZ_ORIGPATH)
mark_as_advanced (BSHUF_TGZ_NAME)

set (BSHUF_PACKAGE_NAME "bshuf" CACHE STRING "Name of BSHUF package")
mark_as_advanced (BSHUF_PACKAGE_NAME)

########
# blosc
########

set (BLOSC_GIT_URL "https://github.com/Blosc/c-blosc.git" CACHE STRING "Use BLOSC from Github repository")
set (BLOSC_GIT_BRANCH "main" CACHE STRING "")
mark_as_advanced (BLOSC_GIT_URL)
mark_as_advanced (BLOSC_GIT_BRANCH)

set (BLOSC_TGZ_ORIGPATH "https://github.com/Blosc/c-blosc/archive/refs/tags" CACHE STRING "Use PLUGINS from original location")
set (BLOSC_TGZ_NAME "c-blosc-1.21.6.tar.gz" CACHE STRING "Use BLOSC from compressed file")
mark_as_advanced (BLOSC_TGZ_ORIGPATH)
mark_as_advanced (BLOSC_TGZ_NAME)

set (BLOSC_PACKAGE_NAME "blosc" CACHE STRING "Name of BLOSC package")
mark_as_advanced (BLOSC_PACKAGE_NAME)

set (BLOSC_ZLIB_GIT_URL "https://github.com/madler/zlib.git" CACHE STRING "Use ZLIB from  GitHub repository")
set (BLOSC_ZLIB_GIT_BRANCH "develop" CACHE STRING "")
mark_as_advanced (BLOSC_ZLIB_GIT_URL)
mark_as_advanced (BLOSC_ZLIB_GIT_BRANCH)

set (BLOSC_ZLIB_TGZ_ORIGPATH "https://github.com/madler/zlib/releases/download/v1.3.1" CACHE STRING "Use PLUGINS from original location")
set (BLOSC_ZLIB_TGZ_NAME "zlib-1.3.1.tar.gz" CACHE STRING "Use ZLib from compressed file")
mark_as_advanced (BLOSC_ZLIB_TGZ_ORIGPATH)
mark_as_advanced (BLOSC_ZLIB_TGZ_NAME)

set (BLOSC_ZLIB_PACKAGE_NAME "zlib" CACHE STRING "Name of BLOSC_ZLIB package")
mark_as_advanced (BLOSC_ZLIB_PACKAGE_NAME)

########
# blosc2
########

set (BLOSC2_GIT_URL "https://github.com/Blosc/c-blosc2.git" CACHE STRING "Use BLOSC2 from Github repository")
set (BLOSC2_GIT_BRANCH "main" CACHE STRING "")
mark_as_advanced (BLOSC2_GIT_URL)
mark_as_advanced (BLOSC2_GIT_BRANCH)

set (BLOSC2_TGZ_ORIGPATH "https://github.com/Blosc/c-blosc2/archive/refs/tags" CACHE STRING "Use PLUGINS from original location")
set (BLOSC2_TGZ_NAME "c-blosc2-2.17.1.tar.gz" CACHE STRING "Use BLOSC2 from compressed file")
mark_as_advanced (BLOSC2_TGZ_ORIGPATH)
mark_as_advanced (BLOSC2_TGZ_NAME)

set (BLOSC2_PACKAGE_NAME "blosc2" CACHE STRING "Name of BLOSC2 package")
mark_as_advanced (BLOSC2_PACKAGE_NAME)

set (BLOSC2_ZLIB_GIT_URL "https://github.com/madler/zlib.git" CACHE STRING "Use ZLIB from  GitHub repository")
set (BLOSC2_ZLIB_GIT_BRANCH "develop" CACHE STRING "")
mark_as_advanced (BLOSC2_ZLIB_GIT_URL)
mark_as_advanced (BLOSC2_ZLIB_GIT_BRANCH)

set (BLOSC2_ZLIB_TGZ_ORIGPATH "https://github.com/madler/zlib/releases/download/v1.3.1" CACHE STRING "Use PLUGINS from original location")
set (BLOSC2_ZLIB_TGZ_NAME "zlib-1.3.1.tar.gz" CACHE STRING "Use ZLib from compressed file")
mark_as_advanced (BLOSC2_ZLIB_TGZ_ORIGPATH)
mark_as_advanced (BLOSC2_ZLIB_TGZ_NAME)

set (BLOSC2_ZLIB_PACKAGE_NAME "zlib" CACHE STRING "Name of BLOSC2_ZLIB package")
mark_as_advanced (BLOSC2_ZLIB_PACKAGE_NAME)

########
# bzip2
########

set (BZ2_GIT_URL "https://github.com/libarchive/bzip2.git" CACHE STRING "Use BZ2 from  GitHub repository")
set (BZ2_GIT_BRANCH "bzip2-1.0.8" CACHE STRING "")
mark_as_advanced (BZ2_GIT_URL)
mark_as_advanced (BZ2_GIT_BRANCH)

set (BZ2_TGZ_ORIGPATH "https://github.com/libarchive/bzip2/archive/refs/tags" CACHE STRING "Use PLUGINS from original location")
set (BZ2_TGZ_NAME "bzip2-bzip2-1.0.8.tar.gz" CACHE STRING "Use BZ2 from compressed file")
mark_as_advanced (BZ2_TGZ_ORIGPATH)
mark_as_advanced (BZ2_TGZ_NAME)

set (BZ2_PACKAGE_NAME "bz2" CACHE STRING "Name of BZ2 package")
mark_as_advanced (BZ2_PACKAGE_NAME)

########
# fpzip
########

set (FPZIP_GIT_URL "https://github.com/LLNL/fpzip.git" CACHE STRING "Use FPZIP from  GitHub repository")
set (FPZIP_GIT_BRANCH "develop" CACHE STRING "")
mark_as_advanced (FPZIP_GIT_URL)
mark_as_advanced (FPZIP_GIT_BRANCH)

set (FPZIP_TGZ_ORIGPATH "https://github.com/LLNL/fpzip/releases/download/1.3.0" CACHE STRING "Use PLUGINS from original location")
set (FPZIP_TGZ_NAME "fpzip-1.3.0.tar.gz" CACHE STRING "Use FPZIP from compressed file")
mark_as_advanced (FPZIP_TGZ_ORIGPATH)
mark_as_advanced (FPZIP_TGZ_NAME)

set (FPZIP_PACKAGE_NAME "fpzip" CACHE STRING "Name of FPZIP package")
mark_as_advanced (FPZIP_PACKAGE_NAME)

#######
# jpeg
#######

set (JPEG_GIT_URL "https://github.com/libjpeg-turbo/libjpeg-turbo.git" CACHE STRING "Use JPEG from TurboJPEG")
set (JPEG_GIT_BRANCH "jpeg-9e" CACHE STRING "")
mark_as_advanced (JPEG_GIT_URL)
mark_as_advanced (JPEG_GIT_BRANCH)

set (JPEG_TGZ_ORIGPATH "https://www.ijg.org/files" CACHE STRING "Use PLUGINS from original location")
set (JPEG_TGZ_NAME "jpegsrc.v9e.tar.gz" CACHE STRING "Use JPEG from compressed file")
mark_as_advanced (JPEG_TGZ_ORIGPATH)
mark_as_advanced (JPEG_TGZ_NAME)

set (JPEG_PACKAGE_NAME "jpeg" CACHE STRING "Name of JPEG package")
mark_as_advanced (JPEG_PACKAGE_NAME)

######
# lz4
######

set (BUILD_LZ4_LIBRARY_SOURCE ON CACHE BOOL "build the lz4 library within the plugin")
mark_as_advanced (BUILD_LZ4_LIBRARY_SOURCE)

set (LZ4_GIT_URL "https://github.com/lz4/lz4.git" CACHE STRING "Use LZ4 from  GitHub repository")
set (LZ4_GIT_BRANCH "dev" CACHE STRING "")
mark_as_advanced (LZ4_GIT_URL)
mark_as_advanced (LZ4_GIT_BRANCH)

set (LZ4_TGZ_ORIGPATH "https://github.com/lz4/lz4/releases/download/v1.10.0" CACHE STRING "Use PLUGINS from original location")
set (LZ4_TGZ_NAME "lz4-1.10.0.tar.gz" CACHE STRING "Use LZ4 from compressed file")
mark_as_advanced (LZ4_TGZ_ORIGPATH)
mark_as_advanced (LZ4_TGZ_NAME)

set (LZ4_PACKAGE_NAME "lz4" CACHE STRING "Name of LZ4 package")
mark_as_advanced (LZ4_PACKAGE_NAME)

######
# lzf
######

set (LZF_URL "http://software.schmorp.de/pkg/liblzf.html" CACHE STRING "LZF home")
mark_as_advanced (LZF_URL)

set (LZF_TGZ_ORIGPATH "http://dist.schmorp.de/liblzf" CACHE STRING "Use LZF from original location")
set (LZF_TGZ_NAME "liblzf-3.6.tar.gz" CACHE STRING "Use LZF from compressed file")
mark_as_advanced (LZF_TGZ_ORIGPATH)
mark_as_advanced (LZF_TGZ_NAME)

set (LZF_PACKAGE_NAME "lzf" CACHE STRING "Name of LZF package")
mark_as_advanced (LZF_PACKAGE_NAME)

#####
# sz
#####

set (SZ_GIT_URL "https://github.com/szcompressor/SZ.git" CACHE STRING "Use SZ filter from  GitHub repository")
set (SZ_GIT_BRANCH "master" CACHE STRING "")
mark_as_advanced (SZ_GIT_URL)
mark_as_advanced (SZ_GIT_BRANCH)

set (SZ_TGZ_ORIGPATH "https://github.com/szcompressor/SZ/releases/download/v2.1.12.5" CACHE STRING "Use PLUGINS from original location")
set (SZ_TGZ_NAME "SZ-2.1.12.5.tar.gz" CACHE STRING "Use SZ filter from compressed file")
mark_as_advanced (SZ_TGZ_ORIGPATH)
mark_as_advanced (SZ_TGZ_NAME)

set (SZ_PACKAGE_NAME "SZ" CACHE STRING "Name of SZ filter package")
mark_as_advanced (SZ_PACKAGE_NAME)

######
# zfp
######

set (ZFP_GIT_URL "https://github.com/LLNL/zfp.git" CACHE STRING "Use ZFP from  GitHub repository")
set (ZFP_GIT_BRANCH "develop" CACHE STRING "")
mark_as_advanced (ZFP_GIT_URL)
mark_as_advanced (ZFP_GIT_BRANCH)

set (ZFP_TGZ_ORIGPATH "https://github.com/LLNL/zfp/releases/download/1.0.0" CACHE STRING "Use PLUGINS from original location")
set (ZFP_TGZ_NAME "zfp-1.0.1.tar.gz" CACHE STRING "Use ZFP from compressed file")
mark_as_advanced (ZFP_TGZ_ORIGPATH)
mark_as_advanced (ZFP_TGZ_NAME)

set (ZFP_PACKAGE_NAME "zfp" CACHE STRING "Name of ZFP package")
mark_as_advanced (ZFP_PACKAGE_NAME)

######
# zstd
######

set (ZSTD_GIT_URL "https://github.com/facebook/zstd.git" CACHE STRING "Use ZSTD from  GitHub repository")
set (ZSTD_GIT_BRANCH "dev" CACHE STRING "")
mark_as_advanced (ZSTD_GIT_URL)
mark_as_advanced (ZSTD_GIT_BRANCH)

set (ZSTD_TGZ_ORIGPATH "https://github.com/facebook/zstd/releases/download/v1.5.7" CACHE STRING "Use PLUGINS from original location")
set (ZSTD_TGZ_NAME "zstd-1.5.7.tar.gz" CACHE STRING "Use ZSTD from compressed file")
mark_as_advanced (ZSTD_TGZ_ORIGPATH)
mark_as_advanced (ZSTD_TGZ_NAME)

set (ZSTD_PACKAGE_NAME "zstd" CACHE STRING "Name of ZSTD package")
mark_as_advanced(ZSTD_PACKAGE_NAME)

