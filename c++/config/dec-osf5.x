
if test -z "$CXX"; then
    CXX=cxx
    CXX_BASENAME=cxx
fi


case $CXX_BASENAME in
    g++)
	CXXFLAGS="$CXXFLAGS -Wsign-compare" #Only works for some versions
	DEBUG_CXXFLAGS="-g -fverbose-asm"
	DEBUG_CPPFLAGS=
	PROD_CXXFLAGS="-O3 -fomit-frame-pointer"
	PROD_CPPFLAGS=
	PROFILE_CXXFLAGS="-pg"
	PROFILE_CPPFLAGS=
	;;

    *)
	CXXFLAGS="$CXXFLAGS -tlocal -D__USE_STD_IOSTREAM "
	DEBUG_CXXFLAGS="-g"
	DEBUG_CPPFLAGS=
	PROD_CXXFLAGS="-O"
	PROD_CPPFLAGS=
	PROFILE_CXXFLAGS="-pg"
	PROFILE_CPPFLAGS=
	;;
esac

