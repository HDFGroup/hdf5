VFD SWMR Punch List

<span style="background:green">This text should be on a green background </span>

This document lives at [https://bitbucket.hdfgroup.org/users/dyoung/repos/vchoi_fork/browse/doc/VFD_SWMR_Punch_List.md?at=refs%2Fheads%2Ffeature%2Fvfd_swmr](https://bitbucket.hdfgroup.org/users/dyoung/repos/vchoi_fork/browse/doc/VFD_SWMR_Punch_List.md?at=refs%2Fheads%2Ffeature%2Fvfd_swmr)

An [interactive tutorial](https://www.markdowntutorial.com/) on Markdown
covers the basics quickly and effectively.

I know I have used this [Markdown "cheat
sheet"](https://commonmark.org/help/) before---it provides a nice visual
guide to the syntax.

22 November 2019

1.  Design optimizations in index writes. Perhaps write deltas only,
    with a full index every n ticks?

2.  Design work for NFS and object store versions.

3.  Design work for journaling variation.

4.  **Vailin, complete** Add support for opening multiple files in
    either VFD SWMR writer or VFD SWMR writer mode. See addition of
    the EOT queue in section 3.2.2 of the RFC, and related changes in
    sections 3.3 and 3.3.2.

5.  **Vailin, complete** Add the pb\_expansion\_threshold field
    to the H5F\_vfd\_swmr\_config\_t structure, and update
    H5Pset\_vfd\_swmr\_config() and H5Pget\_vfd\_swmr\_config()
    accordingly.

6.  **Vailin, in progress** Implement end tick now API call. See section
    3.1.2 of the RFC for specifications.

7.  **Vailin, in progress** Implement enable / disable EOT call. See
    section 3.1.3 of the RFC for specifications.

8.  Implement option to flush raw data as part of EOT -- write
    performance test to characterize the performance hit.

    Currently, the raw data is always flushed at EOT.  We need a knob
    that turns on and off the raw-data flush.

9.  **Vailin, complete** Move VFD SWMR specific H5F code to its own file
    -- say H5Fvfd\_swmr.c.

10. Update the new page buffer to support the pb\_expansion\_threshold,
    and trigger an early end of tick if the threshold is exceeded.

11. **David, complete** Modify metadata file write call to allow the
    location of the index to float and thus be of arbitrary size.

12. **David, complete** Add code to remove entries from the index after
    they have been written to the HDF5 file, and have not been modified
    for at least max\_lag ticks.

13. Tidy short cuts in the initial implementation. These include:

    -   **Vailin complete** Odd behavior in the superblock refresh
        routine (see comments in code). Figure out what is going on, and
        then either bypass the issue or fix it as seems appropriate.

    -   **Vailin complete** Comment H5F\_vfd\_swmr\_config\_t in
        H5Fpublic.h properly.

    -   Cleanup EOA hack in H5FD\_read().

    -   **David, complete** Address file open failures in SWMR
        tests. These appear to be cases in which the writer finishes
        before the reader opens -- causing the reader to fail as
        the metadata file no longer exists. If so, handle this more
        gracefully.

14. Add support for specifying the VFD that sits under the VFD SWMR
    reader VFD. Probably do this as part of the pluggable VFD design
    that Jake is working on.

15. Modify the metadata cache so that we don't allocate space for the
    page hash table unless the file is opened in VFD SWMR reader mode.

16. Implement the logging facility (section 3.14 RFC)

17. **David, probably fixed by various changes since September 2019**
    Test code to expose existing page buffer bugs and fix same. Note
    that they seem to appear primarily on Jelly.

    -   DLL pre remove sanity check assertion failure

    -   vfd\_swmr\_addrem\_writer: H5PB.c:2981:
        H5PB\_\_mark\_entry\_dirty: Assertion
        '(entry\_ptr)-\>delay\_write\_until \> (pb\_ptr)-\>cur\_tick'
        failed.

18. Flesh out designs for unit, integration and performance tests suites
    as outlined in the RFC. Implement same.

19. **David, complete** Fix memory leak in sparse-reader test.

    David found that the shadow index was leaked by VFD SWMR readers and
    plugged the leak.  Now the sparse reader tests do not use up all of
    the memory on `jelly`.

20. **David, complete** Test John's patch that repairs the superblock
    flags mismatch that crashes the reader.

    David found that the patch fixed the demo crashes.

21. Investigate a potential time-of-check, time-of-use race condition
    involving EOA/EOF and the skip\_read variable in some of the H5PB
    routines.

    This race condition may only affect raw-data access.

    It may be necessary to poll the superblock for the current EOA.  The
    reader's refresh routine for the superblock should propagate the new
    EOA to the VFDs.

22. Understand use of H5F\_t on branch feature/vfd\_swmr instead of
    H5C\_t as on develop branch.

23. *Temporarily* reserve a new superblock flag for VFD SWMR for
    development purposes.

24. Prior to feature merge, *permanently* reserve a new superblock flag.

25. Revisit the global heap. The global heap receives data types (a form
    of metadata) and variable-length (VL) data such as strings. It is
    translated to raw data on its way down the HDF5 software stack. If
    the global heap is modified/replaced as currently planned, then VFD
    SWMR does not have to deal with it. However, if the global heap
    overhaul does not take place, then we have more work to do.

26. **David, complete** Fix the expand/shrink test.

    The test appeared to fail because the dataset extent was enlarged
    before the data was written, so arbitrary data was present.  If a
    tick snuck in between the `H5Dset_extent` and the `H5Dwrite`, then
    the reader read the arbitrary data.

    In the `gaussian` test, I have a heuristic that avoids reading
    arbitrary data.  Replicating that in the expand/shrink test has
    fixed it.  Essentially, the reader trails the writer by a bit.

    Ultimately, we should suspend ticks over the H5Dset_extent/H5Dwrite.

27. **Vailin, complete** Change the field name "vfd\_swmr\_writer" to
    "writer" in "struct H5F_vfd_swmr_config_t" and all references to it.
    See page 11 in the RFC.

28. **Vailin, complete** Fix bug as stated on page 9 in the RFC section
    3.1.1:  Given that the VFD SWMR configuration FAPL property is
    set, the writer field must be consistent with the flags passed in
    the H5Fopen() (either H5F_ACC_RDWR for the VFD SWMR writer, or
    H5F_ACC_RDONLY for the VFD SWMR readers).

29. **Vailin, complete** Fix the FSM bug when setting the FSM
    threshold to a non-default value.  The problem was manifested
    when setting the threshold value in vfd_swmr_generator.c via
    H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE,
    1024 * 1024 * 1024) and then run the vfd_swmr_addrem_writer.c.

30. **David, merged, needs unit test** Add a delay to the FSM so that
    freed regions are not reused before max\_lag ticks are up.

31. Create a large dataset with a small chunk size to verify that page
    management in the extensible array is handled properly by VFD SWMR.

32. Make a fixed-size dataset with a small chunk size to verify ... see
    #31.

33. Make automated tests out of the `credel` and `gaussians` tests.

34. Conserve space in the shadow file.  In
    H5F_vfd_swmr_enlarge_shadow_index(), we postpone reclamation of the old
    shadow index for max\_lag ticks, but it's not necessary to wait that
    long.  David traded some space efficiency for simplicity, for now.
    See the note in the source code.

35. In the documentation, we should note that if a writer deletes an
    object that the reader has open (possesses an hid_t), and max_lag
    ticks later the reader tries to access the object by hid_t, the
    reader gets an error result, or crashes, or accesses the wrong
    object.

36. Make variable-length data work.  It cannot work in a simple and
    reliable fashion with VFD SWMR while structural metadata is mixed
    with user data and stored in raw data pages.  For a simple example,
    see the variable-length string test, `vfd_swmr_vlstr`.

37. Let a writer start writing in VFD SWMR mode after the reader has started
    reading in VFD SWMR mode.  Let the reader start reading right away.
