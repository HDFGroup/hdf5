Post-jextract adjustment for org.hdfgroup.javahdf5.hdf5_h_* (FFM).

needle.txt — exact jextract SYMBOL_LOOKUP snippet (LF only).
replacement.txt — shared patched block (createHdf5SymbolLookup + helpers).

CMake picks the first hdf5_h*.java that contains SYMBOL_LOOKUP (jextract may
place it in hdf5_h_2, hdf5_h_3, etc. depending on declaration split count),
then replaces needle.txt with replacement.txt.
If jextract changes its emitted SYMBOL_LOOKUP block, update needle.txt and
keep replacement.txt semantically in sync.
