# An Overview of the HDF5 Library Architecture

## [Purpose, Objectives, and Values](./purpose_objectives_values.md)

## Data and Metadata

### Mapping User Data to HDF5 Files

#### The HDF5 File Format

### Mapping User Data to Storage

### Data Retained in the Library

## Functional Decomposition

### Components

#### Selection
#### Datatype Conversion
#### Filter Pipeline
#### Caching & Buffering
#### File-space Management
#### Opening a File
#### Creating an Object
#### Dataset I/O

### Use Cases

#### Parallel HDF5
#### SWMR
#### VDS
#### Paged Allocation
#### …

### Feature (In-)Compatibility Matrix

## Modular Library Organization

### Library Internals

#### Boilerplate and Misc.
#### Memory Allocation and Free Lists
#### API Contexts
#### Metadata Cache
#### Files and the Open File List
#### Platform Independence

### Modules

#### IDs
#### Property Lists
#### Error Handling
#### File Objects
#### Datasets
#### Groups and Links
#### Datatypes
#### Dataspaces
#### Attributes

### Extension Interfaces

#### Filters
#### Virtual File Layer and Virtual File Drivers
#### Virtual Object Layer

### Language Bindings

#### General Considerations
#### Fortran
#### Java

## Performance Considerations

## Library Development and Maintenance

### Build Process

### Testing

#### Macro Schemes
#### `testhdf5`
#### Other Test Programs
#### (Power)Shell Scripts
#### CMake vs Autotools
#### VOL and VFD Inclusion/Exclusion

### Versioning and Releases

## References
