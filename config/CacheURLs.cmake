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
set (ZLIB_TGZ_NAME "zlib-1.3.1.tar.gz" CACHE STRING "Use HDF5_ZLib from compressed file")
set (ZLIB_TGZ_ORIGPATH "https://github.com/madler/zlib/releases/download/v1.3.1" CACHE STRING "Use ZLIB from original location")
set (ZLIB_GIT_URL "https://github.com/madler/zlib.git" CACHE STRING "Use ZLIB from  GitHub repository")
set (ZLIB_GIT_BRANCH "develop" CACHE STRING "")

set (ZLIBNG_PACKAGE_NAME "zlib-ng" CACHE STRING "Name of ZLIBNG package")
set (ZLIBNG_TGZ_NAME "2.2.4.tar.gz" CACHE STRING "Use HDF5_ZLib from compressed file")
set (ZLIBNG_TGZ_ORIGPATH "https://github.com/zlib-ng/zlib-ng/archive/refs/tags" CACHE STRING "Use ZLIBNG from original location")
set (ZLIBNG_GIT_URL "https://github.com/zlib-ng/zlib-ng.git" CACHE STRING "Use ZLIBNG from  GitHub repository")
set (ZLIBNG_GIT_BRANCH "develop" CACHE STRING "")

set (LIBAEC_PACKAGE_NAME "libaec" CACHE STRING "Name of AEC SZIP package")
set (LIBAEC_TGZ_NAME "libaec-1.1.3.tar.gz" CACHE STRING "Use SZip AEC from compressed file")
set (LIBAEC_TGZ_ORIGPATH "https://github.com/MathisRosenhauer/libaec/releases/download/v1.1.3" CACHE STRING "Use LIBAEC from original location")
set (LIBAEC_GIT_URL "https://github.com/MathisRosenhauer/libaec.git" CACHE STRING "Use LIBAEC from  GitHub repository")
set (LIBAEC_GIT_BRANCH "v1.1.3" CACHE STRING "")

########################
# API test options
########################
set (KWSYS_TGZ_ORIGPATH "https://gitlab.kitware.com/utils/kwsys/-/archive/master" CACHE STRING "Use KWSYS from original location")
set (KWSYS_TGZ_NAME "kwsys-master.tar.gz" CACHE STRING "Use KWSYS from original compressed file")

########################
# filter plugin options
########################

set (PLUGIN_TGZ_ORIGPATH "https://github.com/HDFGroup/hdf5_plugins/releases/download/snapshot" CACHE STRING "Use PLUGINS from original location")
set (PLUGIN_TGZ_NAME "hdf5_plugins-master.tar.gz" CACHE STRING "Use PLUGINS from compressed file")
set (PLUGIN_PACKAGE_NAME "pl" CACHE STRING "Name of PLUGIN package")
set (PLUGIN_GIT_URL "https://github.com/HDFGroup/hdf5_plugins.git" CACHE STRING "Use plugins from HDF Group repository")
set (PLUGIN_GIT_BRANCH "master" CACHE STRING "")

###########
# bitgroom
###########
set (BITGROOM_PACKAGE_NAME "bitgroom" CACHE STRING "Name of BITGROOM package")

###########
# bitround
###########
set (BITROUND_PACKAGE_NAME "bitround" CACHE STRING "Name of BITROUND package")

#############
# bitshuffle
#############

set (BSHUF_GIT_URL "https://github.com/kiyo-masui/bitshuffle.git" CACHE STRING "Use BSHUF from HDF repository")
set (BSHUF_GIT_BRANCH "master" CACHE STRING "")

set (BSHUF_TGZ_ORIGPATH "https://github.com/kiyo-masui/bitshuffle/archive/refs/tags" CACHE STRING "Use PLUGINS from original location")
set (BSHUF_TGZ_NAME "bitshuffle-0.5.2.tar.gz" CACHE STRING "Use BSHUF from compressed file")

set (BSHUF_PACKAGE_NAME "bshuf" CACHE STRING "Name of BSHUF package")

########
# blosc
########

set (BLOSC_GIT_URL "https://github.com/Blosc/c-blosc.git" CACHE STRING "Use BLOSC from Github repository")
set (BLOSC_GIT_BRANCH "main" CACHE STRING "")

set (BLOSC_TGZ_ORIGPATH "https://github.com/Blosc/c-blosc/archive/refs/tags" CACHE STRING "Use PLUGINS from original location")
set (BLOSC_TGZ_NAME "c-blosc-1.21.6.tar.gz" CACHE STRING "Use BLOSC from compressed file")

set (BLOSC_PACKAGE_NAME "blosc" CACHE STRING "Name of BLOSC package")

set (BLOSC_ZLIB_GIT_URL "https://github.com/madler/zlib.git" CACHE STRING "Use ZLIB from  GitHub repository")
set (BLOSC_ZLIB_GIT_BRANCH "develop" CACHE STRING "")

set (BLOSC_ZLIB_TGZ_ORIGPATH "https://github.com/madler/zlib/releases/download/v1.3.1" CACHE STRING "Use PLUGINS from original location")
set (BLOSC_ZLIB_TGZ_NAME "zlib-1.3.1.tar.gz" CACHE STRING "Use ZLib from compressed file")

set (BLOSC_ZLIB_PACKAGE_NAME "zlib" CACHE STRING "Name of BLOSC_ZLIB package")

########
# blosc2
########

set (BLOSC2_GIT_URL "https://github.com/Blosc/c-blosc2.git" CACHE STRING "Use BLOSC2 from Github repository")
set (BLOSC2_GIT_BRANCH "main" CACHE STRING "")

set (BLOSC2_TGZ_ORIGPATH "https://github.com/Blosc/c-blosc2/archive/refs/tags" CACHE STRING "Use PLUGINS from original location")
set (BLOSC2_TGZ_NAME "c-blosc2-2.17.1.tar.gz" CACHE STRING "Use BLOSC2 from compressed file")

set (BLOSC2_PACKAGE_NAME "blosc2" CACHE STRING "Name of BLOSC2 package")

set (BLOSC2_ZLIB_GIT_URL "https://github.com/madler/zlib.git" CACHE STRING "Use ZLIB from  GitHub repository")
set (BLOSC2_ZLIB_GIT_BRANCH "develop" CACHE STRING "")

set (BLOSC2_ZLIB_TGZ_ORIGPATH "https://github.com/madler/zlib/releases/download/v1.3.1" CACHE STRING "Use PLUGINS from original location")
set (BLOSC2_ZLIB_TGZ_NAME "zlib-1.3.1.tar.gz" CACHE STRING "Use ZLib from compressed file")

set (BLOSC2_ZLIB_PACKAGE_NAME "zlib" CACHE STRING "Name of BLOSC2_ZLIB package")

########
# bzip2
########

set (BZ2_GIT_URL "https://github.com/libarchive/bzip2.git" CACHE STRING "Use BZ2 from  GitHub repository")
set (BZ2_GIT_BRANCH "bzip2-1.0.8" CACHE STRING "")

set (BZ2_TGZ_ORIGPATH "https://github.com/libarchive/bzip2/archive/refs/tags" CACHE STRING "Use PLUGINS from original location")
set (BZ2_TGZ_NAME "bzip2-bzip2-1.0.8.tar.gz" CACHE STRING "Use BZ2 from compressed file")

set (BZ2_PACKAGE_NAME "bz2" CACHE STRING "Name of BZ2 package")

########
# fpzip
########

set (FPZIP_GIT_URL "https://github.com/LLNL/fpzip.git" CACHE STRING "Use FPZIP from  GitHub repository")
set (FPZIP_GIT_BRANCH "develop" CACHE STRING "")

set (FPZIP_TGZ_ORIGPATH "https://github.com/LLNL/fpzip/releases/download/1.3.0" CACHE STRING "Use PLUGINS from original location")
set (FPZIP_TGZ_NAME "fpzip-1.3.0.tar.gz" CACHE STRING "Use FPZIP from compressed file")

set (FPZIP_PACKAGE_NAME "fpzip" CACHE STRING "Name of FPZIP package")

#######
# jpeg
#######

set (JPEG_GIT_URL "https://github.com/libjpeg-turbo/libjpeg-turbo.git" CACHE STRING "Use JPEG from TurboJPEG")
set (JPEG_GIT_BRANCH "jpeg-9e" CACHE STRING "")

set (JPEG_TGZ_ORIGPATH "https://www.ijg.org/files" CACHE STRING "Use PLUGINS from original location")
set (JPEG_TGZ_NAME "jpegsrc.v9e.tar.gz" CACHE STRING "Use JPEG from compressed file")

set (JPEG_PACKAGE_NAME "jpeg" CACHE STRING "Name of JPEG package")

######
# lz4
######

set (BUILD_LZ4_LIBRARY_SOURCE ON CACHE BOOL "build the lz4 library within the plugin")

set (LZ4_GIT_URL "https://github.com/lz4/lz4.git" CACHE STRING "Use LZ4 from  GitHub repository")
set (LZ4_GIT_BRANCH "dev" CACHE STRING "")

set (LZ4_TGZ_ORIGPATH "https://github.com/lz4/lz4/releases/download/v1.10.0" CACHE STRING "Use PLUGINS from original location")
set (LZ4_TGZ_NAME "lz4-1.10.0.tar.gz" CACHE STRING "Use LZ4 from compressed file")

set (LZ4_PACKAGE_NAME "lz4" CACHE STRING "Name of LZ4 package")

######
# lzf
######

set (LZF_URL "http://software.schmorp.de/pkg/liblzf.html" CACHE STRING "LZF home")

set (LZF_TGZ_ORIGPATH "http://dist.schmorp.de/liblzf" CACHE STRING "Use LZF from original location")
set (LZF_TGZ_NAME "liblzf-3.6.tar.gz" CACHE STRING "Use LZF from compressed file")

set (LZF_PACKAGE_NAME "lzf" CACHE STRING "Name of LZF package")

#########
# mafisc
#########

#set (BUILD_MAFISC_LIBRARY_SOURCE OFF CACHE BOOL "build the mafisc library within the plugin")

#set (MAFISC_TGZ_ORIGPATH "" CACHE STRING "Use PLUGINS from original location")
#set (MAFISC_TGZ_NAME ".tar.gz" CACHE STRING "Use MAFISC from compressed file")
#set (MAFISC_PACKAGE_NAME "mafisc" CACHE STRING "Name of MAFISC package")

#####
# sz
#####

set (SZ_GIT_URL "https://github.com/szcompressor/SZ.git" CACHE STRING "Use SZ filter from  GitHub repository")
set (SZ_GIT_BRANCH "master" CACHE STRING "")

set (SZ_TGZ_ORIGPATH "https://github.com/szcompressor/SZ/releases/download/v2.1.12.5" CACHE STRING "Use PLUGINS from original location")
set (SZ_TGZ_NAME "SZ-2.1.12.5.tar.gz" CACHE STRING "Use SZ filter from compressed file")

set (SZ_PACKAGE_NAME "SZ" CACHE STRING "Name of SZ filter package")

######
# zfp
######

set (ZFP_GIT_URL "https://github.com/LLNL/zfp.git" CACHE STRING "Use ZFP from  GitHub repository")
set (ZFP_GIT_BRANCH "develop" CACHE STRING "")

set (ZFP_TGZ_ORIGPATH "https://github.com/LLNL/zfp/releases/download/1.0.0" CACHE STRING "Use PLUGINS from original location")
set (ZFP_TGZ_NAME "zfp-1.0.1.tar.gz" CACHE STRING "Use ZFP from compressed file")

set (ZFP_PACKAGE_NAME "zfp" CACHE STRING "Name of ZFP package")

######
# zstd
######

set (ZSTD_GIT_URL "https://github.com/facebook/zstd.git" CACHE STRING "Use ZSTD from  GitHub repository")
set (ZSTD_GIT_BRANCH "dev" CACHE STRING "")

set (ZSTD_TGZ_ORIGPATH "https://github.com/facebook/zstd/releases/download/v1.5.7" CACHE STRING "Use PLUGINS from original location")
set (ZSTD_TGZ_NAME "zstd-1.5.7.tar.gz" CACHE STRING "Use ZSTD from compressed file")

set (ZSTD_PACKAGE_NAME "zstd" CACHE STRING "Name of ZSTD package")
