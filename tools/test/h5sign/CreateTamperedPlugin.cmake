#
# CMake script to create a tampered plugin for testing
# This script copies a signed plugin and modifies it to simulate tampering
#

if (NOT DEFINED FILE OR NOT DEFINED SOURCE)
  message(FATAL_ERROR "FILE and SOURCE must be defined")
endif ()

# Read the source file directly
file(READ ${SOURCE} content HEX)

# Modify a byte in the middle of the file (before the signature)
# This simulates tampering with the plugin binary
string(LENGTH "${content}" content_length)
math(EXPR modify_pos "${content_length} / 2")
math(EXPR modify_pos "${modify_pos} - ${modify_pos} % 2")  # Ensure even position

# Extract parts
string(SUBSTRING "${content}" 0 ${modify_pos} before)
string(SUBSTRING "${content}" ${modify_pos} 2 byte_to_modify)
math(EXPR after_pos "${modify_pos} + 2")
string(SUBSTRING "${content}" ${after_pos} -1 after)

# Flip the byte (XOR with FF)
if (byte_to_modify STREQUAL "00")
  set(modified_byte "FF")
elseif (byte_to_modify STREQUAL "FF")
  set(modified_byte "00")
else ()
  # Just flip the first hex digit
  string(SUBSTRING "${byte_to_modify}" 0 1 first_digit)
  string(SUBSTRING "${byte_to_modify}" 1 1 second_digit)
  if (first_digit STREQUAL "F")
    set(first_digit "0")
  else ()
    set(first_digit "F")
  endif ()
  set(modified_byte "${first_digit}${second_digit}")
endif ()

# Reassemble
set(tampered_content "${before}${modified_byte}${after}")

# Write to destination
file(WRITE ${FILE} "${tampered_content}" HEX)

message(STATUS "Created tampered plugin: ${FILE}")
message(STATUS "  Modified byte at position ${modify_pos}: ${byte_to_modify} -> ${modified_byte}")
