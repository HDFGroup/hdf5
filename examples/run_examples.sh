#/bin/csh!
echo " ...Running h5_write..."
./h5_write
echo " ...Done..."
echo " "
echo " ...Running h5_read..."
echo " "
./h5_read
echo " "
echo " ...Done..."
echo " "
echo " ...Running h5_extend_write..."
./h5_extend_write
echo " ...Done..."
echo " "
echo " ...Running h5_chunk_read..."
echo " "
./h5_chunk_read
echo " "
echo " ...Done..."
echo " "
echo " ...Running h5_compound..."
echo " "
./h5_compound
echo " "
echo " ...Done..."
echo " "
echo " ...Running h5_attribute..."
echo " "
./h5_attribute
echo " "
echo " ...Done..."
echo " "
echo " ...Running h5_mount..."
echo " "
./h5_mount
echo " "
echo " ...Done..."
echo " "
echo " ...Running h5_group..."
echo " "
./h5_group
echo " "
echo " ...Done..."
echo " "
echo " ...Running h5_select..."
echo " "
./h5_select
echo " "
echo " ...Done..."
echo " "
echo " ...Running h5_reference..."
echo " "
./h5_reference
echo " "
echo " ...Done..."
