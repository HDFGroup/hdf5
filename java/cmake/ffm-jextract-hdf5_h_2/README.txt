Post-jextract adjustment for org.hdfgroup.javahdf5.hdf5_h_* (FFM).

needle.txt       exact jextract SYMBOL_LOOKUP snippet (LF only).
replacement.txt  SYMBOL_LOOKUP built by createHdf5SymbolLookup(), which loads
                 the bundled natives through hdf.hdf5lib.Hdf5NativeLoader and
                 looks symbols up in the extracted libhdf5 when present.

CMake (java/CMakeLists.txt) picks the first hdf5_h*.java that contains
SYMBOL_LOOKUP (jextract may place it in hdf5_h_2, hdf5_h_3, etc. depending on
declaration split count), then replaces needle.txt with replacement.txt.
If jextract changes its emitted SYMBOL_LOOKUP block, update needle.txt and
keep replacement.txt semantically in sync.
