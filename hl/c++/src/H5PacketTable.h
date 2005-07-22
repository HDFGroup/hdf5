/* Packet Table wrapper classes
 *
 * Wraps the H5PT Packet Table C functions in C++ objects
 *
 * Nat Furrer and James Laird
 * February 2004
 */

#ifndef H5PTWRAP_H
#define H5PTWRAP_H

#include "H5PT.h"

class H5_HLCPPDLL  PacketTable
{
public:
    /* Null constructor
     * Sets table_id to "invalid"
     */
    PacketTable() {table_id = H5I_BADID;}

    /* "Open" Constructor
     * Opens an existing packet table, which can contain either fixed-length or
     * variable-length records.
     */
    PacketTable(hid_t fileID, char* name);

    /* Destructor
     * Cleans up the packet table
     */
    ~PacketTable();

    /* IsValid
     * Returns true if this packet table is valid, false otherwise.  
     * Use this after the constructor to ensure HDF did not have
     * any trouble making or opening the packet table.
     */
    bool IsValid();

    /* IsVariableLength
     * Return 1 if this packet table is a Variable Length packet table,
     * return 0 if it is Fixed Length.  Returns -1 if the table is
     * invalid (not open).
     */
    int IsVariableLength();

    /* ResetIndex
     * Sets the "current record" to the first record in the packet table
     */
    void ResetIndex();

    /* SetIndex
     * Sets the current record to point to the record specified by index.
     * Returns 0 on success, negative on failure (if index is out of bounds)
     */
    int SetIndex(unsigned int index);

    /* GetPacketCount
     * Returns the number of records in the packet table.  Error
     * is set to 0 on success.  On failure, returns 0 and
     * error is set to negative.
     */
    unsigned int GetPacketCount(int& error);

    unsigned int GetPacketCount()
    {
        int ignoreError;
        return GetPacketCount(ignoreError);
    }

protected:
    hid_t table_id;
};

class H5_HLCPPDLL FL_PacketTable : virtual public PacketTable
{
public:
    /* Constructor
     * Creates a packet table in which to store fixed length records.
     * Takes the ID of the file the packet table will be created in, the name of
     * the packet table, the ID of the datatype of the set, and the size 
     * of a memory chunk used in chunking.
     */
    FL_PacketTable(hid_t fileID, char* name, hid_t dtypeID, int chunkSize);

    /* "Open" Constructor
     * Opens an existing fixed-length packet table.
     * Fails if the packet table specified is variable-length.
     */
    FL_PacketTable(hid_t fileID, char* name);

    /* AppendPacket
     * Adds a single packet to the packet table.  Takes a pointer
     * to the location of the data in memory.
     * Returns 0 on success, negative on failure
     */
    int AppendPacket(void * data);

    /* AppendPackets (multiple records)
     * Adds multiple records to the packet table.  Takes the number of records to be
     * added and a pointer to their location in memory.
     * Returns 0 on success, -1 on failure.
     */
    int AppendPackets(unsigned int numRecords, void * data);

    /* GetPacket (indexed)
     * Gets a single packet from the packet table.  Takes the index
     * of the record (with 0 being the first record) and a pointer
     * to memory where the data should be stored.
     * Returns 0 on success, negative on failure
     */
    int GetPacket(unsigned int index, void * data);

    /* GetPackets (multiple records)
     * Gets multiple packets at once, all records between
     * startIndex and endIndex inclusive.  Also takes a pointer to
     * the memory where these records should be stored.
     * Returns 0 on success, negative on failure.
     */
    int GetPackets(unsigned int startIndex, unsigned int endIndex, void * data);

    /* GetNextPacket (single record)
     * Gets the next record in the packet table.  Takes a pointer to
     * memory where the record should be stored.
     * Returns 0 on success, negative on failure.  Current record
     * is not advanced to the next record on failure.
     */
    int GetNextPacket(void * data);

    /* GetNextPackets (multiple records)
     * Gets the next numRecords records in the packet table.  Takes a
     * pointer to memory where these records should be stored.
     * Returns 0 on success, negative on failure.  Current record
     * is not advanced on failure.
     */
    int GetNextPackets(unsigned int numRecords, void * data);
};

class H5_HLCPPDLL  VL_PacketTable : virtual public PacketTable
{
public:
    /* Constructor
     * Creates a packet table in which to store variable length records.
     * Takes the ID of the file the packet table will be created in, the name of
     * the packet table, and the size of a memory chunk used in chunking.
     */
    VL_PacketTable(hid_t fileID, char* name, int chunkSize);

    /* "Open" Constructor
     * Opens an existing variable-length packet table.
     * Fails if the packet table specified is fixed-length.
     */
    VL_PacketTable(hid_t fileID, char* name);

    /* AppendPacket
     * Adds a single packet of any length to the packet table.
     * Takes a pointer to the location of the data in memory and the length of the data
     * in bytes.
     * Returns 0 on success, negative on failure.
     */
    int AppendPacket(void * data, unsigned int length);

    /* AppendPackets (multiple records)
     * Adds multiple variable-length records to the packet table.  Takes the number of
     * records to be added and a pointer to an array of hvl_t structs in memory.
     * Returns 0 on success, negative on failure.
     */
    int AppendPackets(unsigned int numRecords, hvl_t * data);

    /* GetPacket (indexed)
     * Gets a single variable-length record from the packet table.  Takes the index
     * of the record (with 0 being the first record) and a pointer
     * to a hvl_t struct in which to store the record's size and location.
     * Returns 0 on success, negative on failure.
     */
    int GetPacket(unsigned int index, hvl_t * data);

    /* GetPackets (multiple records)
     * Gets multiple variable-length records at once, all records between
     * startIndex and endIndex inclusive.  Takes a pointer to an array
     * of hvl_t structs in memory in which to store pointers to the records.
     * Returns 0 on success, negative on failure.
     */
    int GetPackets(unsigned int startIndex, unsigned int endIndex, hvl_t * data);

    /* GetNextPacket (single record)
     * Gets the next record in the packet table.  Takes a pointer to
     * an hvl_t struct where the record should be stored.
     * Returns 0 on success, negative on failure.  Current record
     * is not advanced to the next record on failure.
     */
    int GetNextPacket(hvl_t * data);

    /* GetNextPackets (multiple records)
     * Gets the next numRecords records in the packet table.  Takes a
     * pointer to an array of hvl_t structs where pointers to the records
     * should be stored.
     * Returns 0 on success, negative on failure.  Current record
     * is not advanced on failure.
     */
    int GetNextPackets(unsigned int numRecords, hvl_t * data);

    /* FreeReadbuff
     * Frees the buffers created when variable-length records are read.
     * Takes the number of hvl_t structs to be freed and a pointer to their location
     * in memory.
     * Returns 0 on success, negative on error.
     */
    int FreeReadbuff(unsigned int numStructs, hvl_t * buffer);
};

#endif /* H5PTWRAP_H */
