# How to contribute to HDF5

The HDF Group encourages community members to contribute to the HDF5 project. We accept and are very grateful for any contributions,
from minor typos and bug fixes to new features. The HDF Group is committed to work with the code contributors and make contribution
process enjoyable and straightforward.

This document describes guiding principles for the HDF5 code contributors and does not pretend to address any possible
contribution. If in doubt, please do not hesitate to ask us for guidance.
***Note that no contribution may be accepted unless the donor agrees with the HDF Group software license terms
found in the COPYING file in every branch's top source directory.***


> We will assume that you are familiar with `git` and `GitHub`.  If not, you may go through the GitHub tutorial found at
[https://guides.github.com/activities/hello-world/](https://guides.github.com/activities/hello-world/).  This tutorial should only take
around 10 minutes.

## Table of Contents

* [Workflow](#workflow)
* [Acceptance criteria for a pull request](#criteria)
* [Release Note](#releasenote)
* [Check List](#checklist)

# Workflow <A NAME="workflow"></A>

The process for contributing code to HDF5 is as follows:

* Open an issue on [HDF5 GitHub](https://github.com/HDFGroup/hdf5/issues).

> This step is ***required*** unless the change is minor (e.g., typo fix).

* Fork the [HDF5](https://github.com/HDFGroup/hdf5) repository.
* Make the desired changes to the HDF5 software.
    * New features should always go to _develop_ branch first and later should be merged to the appropriate maintenance branches.
    * Bug fixes should go to all appropriate branches (_develop_ and maintenance).
* Build and test your changes. Detailed instructions on building and testing HDF5 can be found in the `INSTALL*` files in the `release_docs` directory.
* Push your changes to GitHub.
* Issue a pull request and address any code formatting and testing issues reported.

Once a pull request is correctly formatted and passes **ALL** CI tests, it will be reviewed and evaluated by The HDF Group developers and HDF5
community members who can approve pull requests. The HDF Group developers will work with you to ensure that the pull request satisfies the acceptance
criteria described in the next section.

# Acceptance criteria for a pull request <A NAME="criteria"></A>

We appreciate every contribution we receive, but we may not accept them all.  Those that we *do* satisfy the following criteria:

* **The pull request has a clear purpose** - What does the pull request address? How does it benefit the HDF5 community?
If the pull request does not have a clear purpose and benefits, it will not be accepted.

* **The pull request is documented** - The HDF5 developers must understand not only *what* a change is doing, but *how* it is doing it.
  Documenting the code makes it easier for us to understand your patch and maintain the code in the future.

* **The pull request passes HDF5 regression testing** - Any issue fixed or functionality added should be accompanied by the corresponding
tests and pass HDF5 regression testing run by The HDF Group. We do not expect you to perform comprehensive testing across multiple platforms
before we accept the pull request. If the pull request does not pass regression testing after the merge, The HDF Group developers will work
with you on the fixes.

* **The pull request does not compromise the principles behind HDF5** - HDF5 has a 100% commitment to backward compatibility.
    * Any file ever created with HDF5 must be readable by any future version of HDF5.
   If your patch's purpose is to modify the HDF5 data model or file format,
 **please** discuss this with us first. File format changes and features required by those changes can be introduced only in a new major release.
    * HDF5 has a commitment to remaining *machine-independent*; data created on one platform/environment/architecture **must** remain readable by HDF5 on any other.
    * For binary compatibility, no changes are allowed to public APIs and data structures in the maintenance releases; new APIs can be added.

* **New features are documented** - Any new features should have proper documentation; talk to us if you have any questions.

* **When to Write a Release Note** - Generally, a release note must be written for every change that is made to the code for which
users might see a change in the way the software works. In other words, if a user might see a difference in the way the software works,
a note should be written. By code we mean the text that will be compiled into one of the company's software products. The code includes
configuration changes and changes to tools users might work with to configure and build our software.

    * Notes should be added for known problems. Known problems are issues that we know about and have not yet been able to fix.

    * Any change made to address a user-reported problem should be described in a release note.

    * A release note does not need to be written for changes to the code that users will not see. Here are some examples. If you add a
comment, you do not need to write a release note describing the comment you added. If you rewrite some code to make it read more
clearly and if there is no change in functionality or performance, then you do not need to write a release note. If you change the
process by which user software is made, you may not need to write a release note since the change was not made to the code.

    * Users. We have different kinds of users. A release note may be written to be helpful to
application developers and not system administrators. Users who may find the RELEASE.txt file helpful include the following:
application developers, library developers, and system administrators.


# Release Note <A NAME="releasenote"></A>

* **Entry Syntax**
The release note entry syntax is shown below.

```
    - Title/Problem

      Problem/Solution

```

* **Entry Elements** - The elements of the entry - title, problem, solution, and signature - are described in more detail in the table
below. Descriptions of the problem and the solution should be clear without any ambiguities and should be short without losing clarity or specifics.

    * **Title** - The title or tag should identify one or more categories that will help readers decide if the entry is something they need to study. Can be combined with the `Problem` element
    * **Problem** - Describe the problem and how users might see the problem in a paragraph.
You might also consider the following as you describe the problem:
        * Under what specific conditions does this issue arise?
        * Under what specific conditions are we sure this issue will not arise?
        * For a performance issue, instead of saying something is a performance issue, describe what the performance impact of issue is?
    * **Solution** - Describe the solution in another paragraph.
You might also consider the following as you describe the solution:
        * What was done to resolve the issue?
        * What is the functional impact?
        * Is there a workaround – a way for users design their software so as not to encounter the issue? If so, what is the workaround?
        * For a performance fix, how has the performance improved? Links to published documentation would be good.

# Checklist <A NAME="checklist"></A>

Please make sure that you check the items applicable to your pull request:

* Code
  * [ ] Does the pull request have a corresponding GitHub issue and clear purpose?
  * [ ] Does the pull request follow HDF5 best practices (naming conventions, code portability, code structure, etc.)? <<TODO: link to the document>>
  * [ ] If changes were done to Autotools build, were they added to CMake and vice versa?
  * [ ] Is the pull request applicable to any other branches? If yes, which ones? Please document it in the GitHub issue.
  * [ ] Is the new code sufficiently documented for future maintenance?
  * [ ] Does the new feature require a change to an existing API? See "API Compatibility Macros" document (https://portal.hdfgroup.org/display/HDF5/API+Compatibility+Macros)
* Documentation
  * [ ] Was the change described in the release_docs/RELEASE.txt file?
  * [ ] Was the new function documented in the corresponding public header file using [Doxygen](https://docs.hdfgroup.org/hdf5/develop/_r_m_t.html)?
  * [ ] Was new functionality documented for the HDF5 community (the level of documentation depends on the feature; ask us what would be appropriate)
* Testing
  * [ ] Does the pull request have tests?
  * [ ] Does the pull request affect HDF5 library performance?

We want as many contributions as we can get, and we are here to help. Feel free to reach out to us if you have any questions

Thank you for your contribution!
