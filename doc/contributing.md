# How to contribute to HDF5 (Draft)

The HDF Group encourges community members to contribute to the HDF5 project. We accept and are very grateful for any type of contribuitons 
from small typos and bug fixes to new features. The HDF Group is committed to work with the code contributors and make contribution process simple and enjoable.

This document describes guiding principles for the HDF5 code contributors and does not pretend to address any possible 
contribution. If in doubt, please do not hesitate to ask us for guidance. 
***Note that no contribution may be accepted unless the donor agrees with the HDF Group software license terms
found in the COPYING file in the top source directory of every branch.***


> We will assume that you are familiar with `git` and `GitHub`.  If not, you may go through the GitHub tutorial found at [https://guides.github.com/activities/hello-world/](https://guides.github.com/activities/hello-world/).  This tutorial should only take around 10 minutes.

## Table of Contents

* [Workflow](#workflow)
* [Acceptance criteria for pull request](#criteria)
* [Check List](#checklist)

# Workflow <A NAME="workflow"></A>

The process for contributing code to HDF5 is as follows:

* Open an issue on [HDF5 GitHub](https://github.com/HDFGroup/hdf5/issues).

> This step is ***required*** unless the change is minor (e.g., typo fix). If reporting an issue, please follow a template found in issue_template.md file if possible.  

* Fork [HDF5](https://github.com/HDFGroup/hdf5) repository.
* Make the desired changes to the HDF5 software.
	* New features should always go to develop branch first and later should be merged to the appropriate maintenance branches.
	* Bug fixes should go to all appropriate branches (develop and maintenance). 
* Build and test your changes. Detailed instructions on how to build and test HDF5 can be found in the `INSTALL*` files in the `release_docs` directory.
* Push your changes to GitHub.
* Issues a pull request and address any code formatting and testing issues reported.

Once a pull request is correctly formatted and passes GitHub tests, it will be reviewed and evaluated by The HDF Group developers and HDF5 community members who has approval power.
The HDF Group developers will work with you to assure that the pull request satisfies acceptance criteria described in the next section. 

# Acceptance criteria for pull request <A NAME="criteria"></A>

We appreciate every contribution we receive, but we do not accept them all.  Those that we *do* accept satisfy the following criteria:

* **Pull request has a clear purpose** - What does the pull request address? How does it benefit the HDF5 community? 
If the pull request does not have a clear purpose and benifits it will not be accepted. 

* **The pull request is documented** - The HDF5 developers must understand not only *what* a change is doing, but *how* it is doing it.  Documenting the code makes it easier for us to understand your patch and will help to maintaine the code in the future. 

* **The pull request passes HDF5 regression testing** - Any issue fixed or functionality added should be accompanied by the corresponding tests and pass HDF5 regression testing run by The HDF Group. We do not expect you to perform comprehensive testing across a multiple platforms before we accept the pull request. If the pull request does not pass regression testing after the merge, The HDF Group developers will work with you on the fixes. 

* **The pull request does not compromise the principles behind HDF5** - HDF5 has a 100% commitment to backward compatibility.  
	* Any file ever created with HDF5 must be readable by any future version of HDF5.
   If the purpose of your patch  is to modify HDF5 data model or file format,
 **please** discuss this with us first. File format changes and features required those changes can be introduced only in a new major release. 
	* HDF5 has a commitment to remaining *machine-independent*; data created on one platform/environment/architecture **must** remain readable by HDF5 on another. 
	* For binary compatibilty no changes are allowed to public APIs and data structures in the maintenance releases; new APIs can be added.

* **New features are documented** - Any new features should have proper documentation; talk to us if you have any questions.


# Checklist <A NAME="checklist"></A>

Please make sure that you check the items applicable to your pull request:

* Code 
  * [ ] Does the pull request have a coresponding GitHub issue and clear purpose?
  * [ ] Does the pull request follow HDF5 best practices (naming convensions, code portability, code structure, etc.)? <<TODO: link to the document>>
  * [ ] If changes were done to autotools build were they added to CMake and vice versa??
  * [ ] Is the pull request applicable to any other branches? If yes, which ones? Please document it in the GitHub issue.
  * [ ] Is the new code sufficiently documented for future maintenance?
* Documentation
  * [ ] Was the change described in the release_docs/RELEASE.txt file?
  * [ ] Was MANIFEST updated if new files had been added to the source?
  * [ ] Was new function documented in the corresponding public header file using Doxygen? <<TODO: link tp Doxygen instructions>>
  * [ ] Was new functionality documented for the HDF5 community (the level of documentation depends on the feature; ask us what would be appropriate)
* Testing
  * [ ] Does the pull request have tests?
  * [ ] If a new feature is added, does it affect HDF5 library perfromance?
  * [ ] Does new feature introduce backward or forward incompatibility? <<TODO: link to the document>>

We want as many contributions as we can get, and we are here to help. Feel free to reach out to us if you have any questions

Thank you for your contribution!

