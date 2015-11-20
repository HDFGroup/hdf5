# The name of the package.
Name: hdf5


Version: 1.8.16
			# Version of the package contained in the RPM.


Release: 1.with.szip.encoder
			# Version of the RPM.


License: BSD-style		
			# Licensing Terms


Group: Development/Libraries	
			# Group, identifies types of software. Used by users to manage multiple RPMs.

Source0: hdf5-1.8.16.tar.gz	
Source1: szip2.1inst64.tar.gz

			#Source tar ball name
URL: http://www.hdfgroup.org/HDF5		


			# URL to find package
Buildroot: %{_tmppath}/%{name}-%{version}-%{release}-root	


			#used with non-root builds of RPM files
BuildRequires: gcc-c++, gcc-gfortran

Requires: zlib
Requires: zlib-devel
Requires(post): /sbin/ldconfig
Requires(postun): /sbin/ldconfig

Summary:  HDF5 is a unique technology suite that makes possible the management of extremely large and complex data collections.
			# One line summary of package

Prefix: /usr


%define _libdir /usr/lib64

%description					

			# Full description. Can be multiple lines.
The HDF5 technology suite includes:

    * A versatile data model that can represent very complex data objects and a wide variety of metadata.

    * A completely portable file format with no limit on the number or size of data objects in the collection.

    * A software library that runs on a range of computational platforms, from laptops to massively parallel systems, and implements a high-level API with C, C++, Fortran 90, and Java interfaces.

    * A rich set of integrated performance features that allow for access time and storage space optimizations.

    * Tools and applications for managing, manipulating, viewing, and analyzing the data in the collection.

The HDF5 data model, file format, API, library, and tools are open and distributed without charge.

%package devel
Summary: Header files and compile scripts for developing software using the HDF5 library.
Group: Development/Libraries
Requires: hdf5 = 1.8.16
Requires(post): ed

%description devel

Header files and compile scripts for developing software using the HDF5 library.

The HDF5 technology suite includes:

    * A versatile data model that can represent very complex data objects and a wide variety of metadata.

    * A completely portable file format with no limit on the number or size of data objects in the collection.

    * A software library that runs on a range of computational platforms, from laptops to massively parallel systems, and implements a high-level API with C, C++, Fortran 90, and Java interfaces.

    * A rich set of integrated performance features that allow for access time and storage space optimizations.

    * Tools and applications for managing, manipulating, viewing, and analyzing the data in the collection.

The HDF5 data model, file format, API, library, and tools are open and distributed without charge.

%package static
Summary: Static library files for linking software using the HDF5 library.
Group: Development/Libraries
Requires: hdf5 = 1.8.16
Requires(post): ed
Requires(postun): ed

%description static 

Static library files for linking software using the HDF5 library.

The HDF5 technology suite includes:

    * A versatile data model that can represent very complex data objects and a wide variety of metadata.

    * A completely portable file format with no limit on the number or size of data objects in the collection.

    * A software library that runs on a range of computational platforms, from laptops to massively parallel systems, and implements a high-level API with C, C++, Fortran 90, and Java interfaces.

    * A rich set of integrated performance features that allow for access time and storage space optimizations.

    * Tools and applications for managing, manipulating, viewing, and analyzing the data in the collection.

The HDF5 data model, file format, API, library, and tools are open and distributed without charge.



%prep				
			#prep: list steps after this to unpack the package.			
%setup -n hdf5-1.8.16
			# setup is a macro used to unpack the package with default settings (i.e., gunzip, untar)

%build				
			#build: steps after this should compile the package
			#macro used to configure the package with standard ./configure command
rm -rf $RPM_BUILD_ROOT		
mkdir $RPM_BUILD_ROOT
                        # Extract the szip binaries before configure for configure/compile/link.
tar zxvf $RPM_SOURCE_DIR/szip2.1inst64.tar.gz -C $RPM_BUILD_DIR

                        # configure 
                        # The rpmbuild configure macro adds several flags to CFLAGS.  In order to avoid those flags we put
                        # the rpmbuild configure options plus our own options here and do not use the configure macro.
./configure --build=x86_64-redhat-linux-gnu --host=x86_64-redhat-linux-gnu --target=x86_64-redhat-linux-gnu --program-prefix= --prefix=/usr --exec-prefix=/usr --bindir=/usr/bin --sbindir=/usr/sbin --sysconfdir=/etc --datadir=/usr/share --includedir=/usr/include --libdir=/usr/lib64 --libexecdir=/usr/libexec --localstatedir=/var --sharedstatedir=/usr/com --mandir=/usr/share/man --infodir=/usr/share/info \
            --enable-fortran --enable-fortran2003 --enable-cxx \
            --enable-production \
            --with-szlib=$RPM_BUILD_DIR/usr/include,$RPM_BUILD_DIR/usr/lib64 \
            --with-zlib=/usr

make 	
			#this is a direct command-line option, which just runs .make.: compiles the package.


%install			
			#install: steps after this will install the package.

			#used with non-root builds of RPM files.

make install DESTDIR=$RPM_BUILD_ROOT	
			#performs a make install

                        # Extract szip files again for installing.  This can't 
                        # be done before the install step, which now deletes 
                        # everthing in $RPM_BUILD_ROOT before installing files there.
tar zxvf /scr/lrknox/rpmbuild/SOURCES/szip2.1inst64.tar.gz -C $RPM_BUILD_ROOT
                        # Delete files that we won't install
rm -f $RPM_BUILD_ROOT%{_libdir}/*.la

#
#  Post-install-Script
#
%post

if test `whoami` == root; then
   echo "Running /sbin/ldconfig"
   /sbin/ldconfig
fi

%post devel

# h5redeploy will set the prefix in the installed compile scripts (h5cc, etc)
# to the installed directory.  The paths to the hd5 libraries and include files 
# should then be correct when compiling files.  The ed script that follows it 
# will remove paths on the build system that may not be correct on the install
# system and switch the value of STATIC_AVAILABLE to no.
(cd $RPM_INSTALL_PREFIX/bin
   ./h5redeploy -libdir='${exec_prefix}/lib64' -force
   # clean up build paths in compile scripts
   h5tools="h5cc h5pcc h5fc h5pfc h5c++"   # possible hdf5 tools
   foundtools=             # tools found and will be modified

   for x in $h5tools; do
      if [ -f $x ]; then
         foundtools="$foundtools $x"
         if [ ! -w $x ]; then
            ERROR "h5tool($x) is not writable"
            exit $EXIT_FAILURE
         fi
      fi
   done

   # remove any -I<path> entries in H5BLD_CPPFLAGS and 
   # -L<path> entries in H5BLD_LDFLAGS left from the build.
   if [ -f $RPM_INSTALL_PREFIX/lib64/libhdf5.a ]; then
      for t in $foundtools; do
         echo Update $t ...
         ed - $t << end
         g/^H5BLD_CPPFLAGS/s/-I\/.*include //g
         g/^H5BLD_LDFLAGS/s/-L\/.*lib //g
         g/^H5BLD_LDFLAGS/s/-L\/.*lib64 //g
         .
         w
end
      done
   else
      for t in $foundtools; do
         echo Update $t ...
         ed - $t << end
         g/^H5BLD_CPPFLAGS/s/-I\/.*include //g
         g/^H5BLD_LDFLAGS/s/-L\/.*lib //g
         g/^H5BLD_LDFLAGS/s/-L\/.*lib64 //g
         g/^STATIC_AVAILABLE/s/yes/no/
         .
         w
end
      done
   fi
)

%post static

# The ed script switches the value of STATIC_AVAILABLE to yes
(cd $RPM_INSTALL_PREFIX/bin
   h5tools="h5cc h5pcc h5fc h5pfc h5c++"   # possible hdf5 tools
   foundtools=             # tools found and will be modified

   for x in $h5tools; do
      if [ -f $x ]; then
         foundtools="$foundtools $x"
         if [ ! -w $x ]; then
            ERROR "h5tool($x) is not writable"
            exit $EXIT_FAILURE
         fi
      fi
   done

   for t in $foundtools; do
      echo Update $t ...
      ed - $t << end
      g/^STATIC_AVAILABLE/s/no/yes/
      .
      w
end
   done
)


%clean				
			#performs a make clean after the install
rm -rf $RPM_BUILD_ROOT		

			#used with non-root builds of RPM files.

%postun
if test `whoami` == root; then
   echo "Running /sbin/ldconfig"
   /sbin/ldconfig
fi

%postun static

# The ed script switches the value of STATIC_AVAILABLE to no
(cd $RPM_INSTALL_PREFIX/bin
   h5tools="h5cc h5pcc h5fc h5pfc h5c++"   # possible hdf5 tools
   foundtools=             # tools found and will be modified

   for x in $h5tools; do
      if [ -f $x ]; then
         foundtools="$foundtools $x"
         if [ ! -w $x ]; then
            ERROR "h5tool($x) is not writable"
            exit $EXIT_FAILURE
         fi
      fi
   done

   # remove any -I<path> entries in H5BLD_CPPFLAGS and 
   # -L<path> entries in H5BLD_LDFLAGS left from the build.
   for t in $foundtools; do
      echo Update $t ...
      ed - $t << end
      g/^STATIC_AVAILABLE/s/yes/no/
      .
      w
end
   done
)

%files				
			#files should be followed by a list of all files that get installed by the main package.
%defattr(0755,root,root)
%{_bindir}/gif2h5
%{_bindir}/h52gif
%{_bindir}/h5copy
%{_bindir}/h5debug
%{_bindir}/h5diff
%{_bindir}/h5dump
%{_bindir}/h5import
%{_bindir}/h5jam
%{_bindir}/h5ls
%{_bindir}/h5mkgrp
%{_bindir}/h5perf_serial
%{_bindir}/h5repack
%{_bindir}/h5repart
%{_bindir}/h5stat
%{_bindir}/h5unjam
%defattr(0644,root,root)
%{_libdir}/libhdf5.settings
%doc ./COPYING
%doc ./release_docs/RELEASE.txt
%defattr(0755,root,root)
%{_libdir}/libhdf5.so*
%{_libdir}/libhdf5_cpp.so*
%{_libdir}/libhdf5_fortran.so*
%{_libdir}/libhdf5_hl.so*
%{_libdir}/libhdf5_hl_cpp.so*
%{_libdir}/libhdf5hl_fortran.so*
%{_libdir}/libsz.so*

%files devel
                        #files to be installed by the hdf5-devel package.
%defattr(0755,root,root)
%{_bindir}/h5cc
%{_bindir}/h5c++
%{_bindir}/h5fc
%{_bindir}/h5redeploy
%defattr(0644,root,root)
%{_includedir}/H5ACpublic.h
%{_includedir}/H5Apublic.h
%{_includedir}/H5Cpublic.h
%{_includedir}/H5DOpublic.h
%{_includedir}/H5DSpublic.h
%{_includedir}/H5Dpublic.h
%{_includedir}/H5Epubgen.h
%{_includedir}/H5Epublic.h
%{_includedir}/H5FDcore.h
%{_includedir}/H5FDdirect.h
%{_includedir}/H5FDfamily.h
%{_includedir}/H5FDlog.h
%{_includedir}/H5FDmpi.h
%{_includedir}/H5FDmpio.h
%{_includedir}/H5FDmulti.h
%{_includedir}/H5FDpublic.h
%{_includedir}/H5FDsec2.h
%{_includedir}/H5FDstdio.h
%{_includedir}/H5Fpublic.h
%{_includedir}/H5f90i.h
%{_includedir}/H5f90i_gen.h
%{_includedir}/H5Gpublic.h
%{_includedir}/H5IMpublic.h
%{_includedir}/H5Ipublic.h
%{_includedir}/H5Location.h
%{_includedir}/H5LTpublic.h
%{_includedir}/H5Lpublic.h
%{_includedir}/H5MMpublic.h
%{_includedir}/H5Opublic.h
%{_includedir}/H5PLextern.h
%{_includedir}/H5PLpublic.h
%{_includedir}/H5PTpublic.h
%{_includedir}/H5Ppublic.h
%{_includedir}/H5Rpublic.h
%{_includedir}/H5Spublic.h
%{_includedir}/H5TBpublic.h
%{_includedir}/H5Tpublic.h
%{_includedir}/H5Zpublic.h
%{_includedir}/H5api_adpt.h
%{_includedir}/H5overflow.h
%{_includedir}/H5pubconf.h
%{_includedir}/H5public.h
%{_includedir}/H5version.h
%{_includedir}/hdf5.h
%{_includedir}/hdf5_hl.h
%{_includedir}/H5AbstractDs.h
%{_includedir}/H5ArrayType.h
%{_includedir}/H5AtomType.h
%{_includedir}/H5Attribute.h
%{_includedir}/H5Classes.h
%{_includedir}/H5CommonFG.h
%{_includedir}/H5CompType.h
%{_includedir}/H5Cpp.h
%{_includedir}/H5CppDoc.h
%{_includedir}/H5DataSet.h
%{_includedir}/H5DataSpace.h
%{_includedir}/H5DataType.h
%{_includedir}/H5DcreatProp.h
%{_includedir}/H5DxferProp.h
%{_includedir}/H5EnumType.h
%{_includedir}/H5Exception.h
%{_includedir}/H5FaccProp.h
%{_includedir}/H5FcreatProp.h
%{_includedir}/H5File.h
%{_includedir}/H5FloatType.h
%{_includedir}/H5Group.h
%{_includedir}/H5IdComponent.h
%{_includedir}/H5Include.h
%{_includedir}/H5IntType.h
%{_includedir}/H5Library.h
%{_includedir}/H5Object.h
%{_includedir}/H5OcreatProp.h
%{_includedir}/H5PacketTable.h
%{_includedir}/H5PredType.h
%{_includedir}/H5PropList.h
%{_includedir}/H5StrType.h
%{_includedir}/H5VarLenType.h
%{_includedir}/h5_dble_interface.mod
%{_includedir}/h5a.mod
%{_includedir}/h5a_provisional.mod
%{_includedir}/h5d.mod
%{_includedir}/h5d_provisional.mod
%{_includedir}/h5ds.mod
%{_includedir}/h5e.mod
%{_includedir}/h5e_provisional.mod
%{_includedir}/h5f.mod
%{_includedir}/h5f_provisional.mod
%{_includedir}/h5fortran_types.mod
%{_includedir}/h5g.mod
%{_includedir}/h5global.mod
%{_includedir}/h5i.mod
%{_includedir}/h5im.mod
%{_includedir}/h5l.mod
%{_includedir}/h5l_provisional.mod
%{_includedir}/h5lib.mod
%{_includedir}/h5lib_provisional.mod
%{_includedir}/h5lt.mod
%{_includedir}/h5o.mod
%{_includedir}/h5o_provisional.mod
%{_includedir}/h5p.mod
%{_includedir}/h5p_provisional.mod
%{_includedir}/h5r.mod
%{_includedir}/h5r_provisional.mod
%{_includedir}/h5s.mod
%{_includedir}/h5t.mod
%{_includedir}/h5t_provisional.mod
%{_includedir}/h5tb.mod
%{_includedir}/h5test_kind_*_mod.mod
%{_includedir}/h5z.mod
%{_includedir}/hdf5.mod
%{_includedir}/ricehdf.h
%{_includedir}/szip_adpt.h
%{_includedir}/szlib.h
%defattr(0755,root,root)
%dir %{_datadir}/hdf5_examples
%{_datadir}/hdf5_examples/*.sh
%{_datadir}/hdf5_examples/*/*.sh
%{_datadir}/hdf5_examples/*/*/*.sh
%defattr(0644,root,root)
%{_datadir}/hdf5_examples/README
%{_datadir}/hdf5_examples/*/*.c
%{_datadir}/hdf5_examples/*/*.f90
%{_datadir}/hdf5_examples/*/*.cpp
%{_datadir}/hdf5_examples/*/*/*.c
%{_datadir}/hdf5_examples/*/*/*.h
%{_datadir}/hdf5_examples/*/*/*.txt
%{_datadir}/hdf5_examples/*/*/*.f90
%{_datadir}/hdf5_examples/*/*/*.cpp

%files static

%{_libdir}/libhdf5.a
%{_libdir}/libhdf5_cpp.a
%{_libdir}/libhdf5_fortran.a
%{_libdir}/libhdf5_hl.a
%{_libdir}/libhdf5_hl_cpp.a
%{_libdir}/libhdf5hl_fortran.a
%{_libdir}/libsz.a

#list of changes to this spec file since last version.
%changelog
* Wed Nov 11 2015 Larry Knox
lrknox@hdfgroup.org 1.8.16-1
