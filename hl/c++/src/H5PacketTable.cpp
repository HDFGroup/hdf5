/* Packet Table wrapper classes
 *
 * Wraps the H5PT Packet Table C functions in C++ objects
 *
 * Nat Furrer and James Laird
 * February 2004
 */

#include "H5PacketTable.h"

    /********************************/
    /* PacketTable superclass       */
    /********************************/

    /* "Open" Constructor
     * Opens an existing packet table, which can contain either fixed-length or
     * variable-length records.
     */
    PacketTable::PacketTable(hid_t fileID, char* name)
    {
        table_id = H5PTopen( fileID, name);
    }

    /* Destructor
     * Cleans up the packet table
     */
    PacketTable::~PacketTable()
    {
        H5PTclose( table_id);
    }

    /* IsValid
     * Returns true if this packet table is valid, false otherwise.
     * Use this after the constructor to ensure HDF did not have
     * any trouble making or opening the packet table.
     */
    bool PacketTable::IsValid()
    {
        if (H5PTis_valid(table_id) == 0)
            return true;
        else
            return false;
    }

    /* IsVariableLength
     * Return 1 if this packet table is a Variable Length packet table,
     * return 0 if it is Fixed Length.  Returns -1 if the table is
     * invalid (not open).
     */
    int PacketTable::IsVariableLength()
    {
        return H5PTis_varlen(table_id);
    }

    /* ResetIndex
     * Sets the "current record" to the first record in the packet table
     */
    void PacketTable::ResetIndex()
    {
        H5PTcreate_index(table_id);
    }

    /* SetIndex
     * Sets the current record to point to the record specified by index.
     * Returns 0 on success, negative on failure (if index is out of bounds)
     */
    int PacketTable::SetIndex(unsigned int index)
    {
        return H5PTset_index(table_id, index);
    }

    /* GetPacketCount
     * Returns the number of records in the packet table.  Error
     * is set to 0 on success.  On failure, returns 0 and
     * error is set to negative.
     */
    unsigned int PacketTable::GetPacketCount(int& error)
    {
        hsize_t nrecords;

        error = H5PTget_num_packets( table_id, (hsize_t *)&nrecords);
        return nrecords;
    }

    /********************************/
    /* Fixed-Length Packet Table    */
    /********************************/

    /* Constructor
     * Creates a packet table in which to store fixed length records.
     * Takes the ID of the file the packet table will be created in, the name of
     * the packet table, the ID of the datatype of the set, and the size
     * of a memory chunk used in chunking.
     */
    FL_PacketTable::FL_PacketTable(hid_t fileID, char* name, hid_t dtypeID, int chunkSize)
    {
        table_id = H5PTcreate_fl ( fileID, name, dtypeID, chunkSize);
    }

    /* "Open" Constructor
     * Opens an existing fixed-length packet table.
     * Fails if the packet table specified is variable-length.
     */
    FL_PacketTable::FL_PacketTable(hid_t fileID, char* name) : PacketTable(fileID, name)
    {
        if( H5PTis_varlen(table_id) != 0 )    // If this is not a fixed-length table
        {
            H5PTclose(table_id);
            table_id = NULL;
        }
    }

    /* AppendPacket
     * Adds a single packet to the packet table.  Takes a pointer
     * to the location of the data in memory.
     * Returns 0 on success, negative on failure
     */
    int FL_PacketTable::AppendPacket(void * data)
    {
        return H5PTappend(table_id, 1, data);
    }

    /* AppendPackets (multiple packets)
     * Adds multiple records to the packet table.  Takes the number of records to be
     * added and a pointer to their location in memory.
     * Returns 0 on success, -1 on failure.
     */
    int FL_PacketTable::AppendPackets(unsigned int numRecords, void * data)
    {
        return H5PTappend(table_id, numRecords, data);
    }

    /* GetPacket (indexed)
     * Gets a single packet from the packet table.  Takes the index
     * of the record (with 0 being the first record) and a pointer
     * to memory where the data should be stored.
     * Returns 0 on success, negative on failure
     */
    int FL_PacketTable::GetPacket(unsigned int index, void * data)
    {
        return H5PTread_packets(table_id, index, 1, data);
    }

    /* GetPackets (multiple packets)
     * Gets multiple packets at once, all records between
     * startIndex and endIndex inclusive.  Also takes a pointer to
     * the memory where these records should be stored.
     * Returns 0 on success, negative on failure.
     */
    int FL_PacketTable::GetPackets(unsigned int startIndex, unsigned int endIndex, void * data)
    {
        // Make sure the range of indexes is valid
        if (startIndex > endIndex)
            return -1;

        return  H5PTread_packets(table_id, startIndex, endIndex-startIndex+1, data);
    }

    /* GetNextPacket (single packet)
     * Gets the next record in the packet table.  Takes a pointer to
     * memory where the record should be stored.
     * Returns 0 on success, negative on failure.  Current record
     * is not advanced to the next record on failure.
     */
    int FL_PacketTable::GetNextPacket(void * data)
    {
        return H5PTget_next(table_id, 1, data);
    }

    /* GetNextPackets (multiple packets)
     * Gets the next numRecords records in the packet table.  Takes a
     * pointer to memory where these records should be stored.
     * Returns 0 on success, negative on failure.  Current record
     * is not advanced on failure.
     */
    int FL_PacketTable::GetNextPackets(unsigned int numRecords, void * data)
    {
        return H5PTget_next(table_id, numRecords, data);
    }


    /********************************/
    /* Variable-Length Packet Table */
    /********************************/

    /* Constructor
     * Creates a packet table in which to store variable length records.
     * Takes the ID of the file the packet table will be created in, the name of
     * the packet table, and the size of a memory chunk used in chunking.
     */
    VL_PacketTable::VL_PacketTable(hid_t fileID, char* name, int chunkSize)
    {
        table_id = H5PTcreate_vl ( fileID, name, chunkSize);
    }

    /* "Open" Constructor
     * Opens an existing variable-length packet table.
     * Fails if the packet table specified is fixed-length.
     */
    VL_PacketTable::VL_PacketTable(hid_t fileID, char* name) : PacketTable(fileID, name)
    {
        if( H5PTis_varlen(table_id) != 1 )    // If this is not a variable-length table
        {
            H5PTclose(table_id);
            table_id = NULL;
        }
    }

    /* AppendPacket (variable-length)
     * Adds a single variable-length packet to the packet table.
     * Takes a pointer to the location of the data in memory and the length of the data
     * in bytes.
     * Returns 0 on success, negative on failure.
     */
       int VL_PacketTable::AppendPacket(void * data, unsigned int length)
    {
        hvl_t packet;

        packet.len = length;
        packet.p = data;

        return H5PTappend(table_id, 1, &packet);
    }

    /* AppendPackets (multiple packets)
     * Adds multiple variable-length records to the packet table.  Takes the number of
     * records to be added and a pointer to an array of hvl_t structs in memory.
     * Returns 0 on success, negative on failure.
     */
    int VL_PacketTable::AppendPackets(unsigned int numRecords, hvl_t * data)
    {
        return H5PTappend(table_id, numRecords, data);
    }

    /* GetPacket (indexed)
     * Gets a single variable-length record from the packet table.  Takes the index
     * of the record (with 0 being the first record) and a pointer
     * to a hvl_t struct in which to store the record's size and location.
     * Returns 0 on success, negative on failure.
     */
    int VL_PacketTable::GetPacket(unsigned int index, hvl_t * data)
    {
        return H5PTread_packets(table_id, index, 1, data);
    }

    /* GetPackets (multiple packets)
     * Gets multiple variable-length records at once, all records between
     * startIndex and endIndex inclusive.  Takes a pointer to an array
     * of hvl_t structs in memory in which to store pointers to the records.
     * Returns 0 on success, negative on failure.
     */
    int VL_PacketTable::GetPackets(unsigned int startIndex, unsigned int endIndex, hvl_t * data)
    {
        // Make sure the range of indexes is valid
        if (startIndex > endIndex)
            return -1;

        return  H5PTread_packets(table_id, startIndex, endIndex-startIndex+1, data);
    }

    /* GetNextPacket (single packet)
     * Gets the next record in the packet table.  Takes a pointer to
     * an hvl_t struct where the record should be stored.
     * Returns 0 on success, negative on failure.  Current record
     * is not advanced to the next record on failure.
     */
    int VL_PacketTable::GetNextPacket(hvl_t * data)
    {
        return H5PTget_next(table_id, 1, data);
    }

    /* GetNextPackets (multiple packets)
     * Gets the next numRecords records in the packet table.  Takes a
     * pointer to an array of hvl_t structs where pointers to the records
     * should be stored.
     * Returns 0 on success, negative on failure.  Current record
     * is not advanced on failure.
     */
    int VL_PacketTable::GetNextPackets(unsigned int numRecords, hvl_t * data)
    {
        return H5PTget_next(table_id, numRecords, data);
    }

    /* FreeReadbuff
     * Frees the buffers created when variable-length records are read.
     * Takes the number of hvl_t structs to be freed and a pointer to their location
     * in memory.
     * Returns 0 on success, negative on error.
     */
    int VL_PacketTable::FreeReadbuff(unsigned int numStructs, hvl_t * buffer)
    {
        return H5PTfree_vlen_readbuff( table_id, numStructs, buffer);
    }
