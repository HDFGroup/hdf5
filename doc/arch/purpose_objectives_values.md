## Purpose, Objectives, & Values

The purpose of the HDF5 library is to ensure efficient and equitable access to science and engineering data stored in HDF5 across platforms and environments, now and forever. Toward that purpose, the two main objectives are:

1. Self-describing data
2. Portable encapsulation and access.

Self-describing data captures all information about itself necessary to reproduce and interpret it as intended by its producer. A storage representation must preserve the self-describing nature when transferring such representations over a network or to different storage. At the same time, it should be accompanied by a portable library that allows applications to access the data without knowing anything about the details of the representation.

The "marriage"[^1] of the HDF5 file format and library is a specific implementation of the primitives and operations defined by the HDF5 data model and adapted for several specific use cases.

![HDF5 models and implementations](https://docs.hdfgroup.org/hdf5/develop/Dmodel_fig1.gif)

The following values[^2][^3] guide the implementation (in no particular order):

<dl>
  <dt>Extensibility</dt>
  <dd>The degree to which behavior and appearance can be changed by users.
    <ul>
      <li>Datatypes, conversions</li>
      <li>Filters</li>
      <li>Links</li>
      <li>Data virtualization</li>
      <li>Storage types</li>
    </ul>
    Evolution of the file format
    <ul>
      <li>Micro-versioning</li>
    </ul>
  </dd>

  <dt>Compatibility, Longevity, &amp; Stability</dt>
  <dd>Things that worked before continue to work the same indefinitely
    <ul>
      <li>Quasi-fixed data model</li>
      <li>Backward- and forward compatibility</li>
      <li>API Versioning</li>
      <li>File format specification</li>
    </ul>
  </dd>
  
  <dt>Efficiency</dt>
  <dd>Effective operation as measured by a comparison of production with cost (as in time, storage, energy, etc.)
    <ul>
      <li>Algorithmic complexity</li>
      <li>Scalability</li>
    </ul>
  </dd>
  
  <dt>Maintainability</dt>
  <dd>The degree to which it can be modified without introducing fault
    <ul>
      <li>Additive software construction</li>
    </ul>
  </dd>
  
  <dt>Progressiveness</dt>
  <dd>A measure of eagerness to make progress and leverage modern storage technology</dd>
  
  <dt>Freedom</dt>
  <dd>Specifically, free software means users have the four essential freedoms:
    <ul>
      <li>The freedom to run the program as you wish, for any purpose (freedom 0).</li>
      <li>The freedom to study how the program works, and change it so it does your computing as you wish (freedom 1). Access to the source code is a precondition for this.</li>
      <li>The freedom to redistribute copies so you can help others (freedom 2).</li>
      <li>The freedom to distribute copies of your modified versions to others (freedom 3). By doing this you can give the whole community a chance to benefit from your changes. Access to the source code is a precondition for this.</li>
    </ul>
  </dd>
  
  
</dl>




[^1]: Jeffrey A. Kuehn: [Faster Libraries for Creating Network-Portable Self-Describing Datasets](https://cug.org/5-publications/proceedings_attendee_lists/1997CD/S96PROC/289_293.PDF), CUG 1996 Spring Proceedings
[^2]: Chris Hanson & Gerald Jay Sussman: [Software Design for Flexibility: How to Avoid Programming Yourself into a Corner](https://mitpress.mit.edu/9780262045490/software-design-for-flexibility/), MIT Press.
[^3]: Free Software Foundation: [What is Free Software?](https://www.gnu.org/philosophy/free-sw.en.html)
