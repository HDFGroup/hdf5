# How to contribute to HDF5

The HDF Group encourges community members to contribute to the HDF5 project. We accept and are very grateful for any type of contribuitons 
from small typos and bugs fixes to new features. The HDF Group is committed to work with the code contributors and make contribution process simple and enjoable.

This document describes guiding principles for the HDF5 code contribtors and does not pretend to address any possible 
contribution. If in doubt, please do not hesitate to ask us for guidance. 
***Note that no contribution may be accepted unless the donor agrees with the HDF Group software license terms
found in COPYING file in the top source directory of every branch.***


> We will assume that you are familiar with `git` and `GitHub`.  If not, you may go through the GitHub tutorial found at [https://guides.github.com/activities/hello-world/](https://guides.github.com/activities/hello-world/).  This tutorial should only take around 10 minutes.

## Table of Contents

* [Workflow](#workflow)
* [Acceptance criteria for pull request](#criteria)
* [Building and testing your contribution](#testing)
	*[Testing with Autotools](#autotools)
	*[Testing with CMake](#cmake)
* [Check List](#checklist)

# Workflow <A NAME="workflow"></A>

The process for contributing code to HDF5 is as follows:

* Open an issue on [HDF5 GitHub](https://github.com/HDFGroup/hdf5/issues).

> This step is ***required*** unless the change is minor (e.g., typo fix). If reporting an issue, please follow a template found in issue_template.md file if possible.  

* Fork [HDF5](https://github.com/HDFGroup/hdf5) repository.
* Make the desired changes to the HDF5 software.
	* New features should always go to develop branch and later merged to appropriate maintenance branches
	* Bug fixes should go to all appropriate branches (develop and maintenance). 
* Build and test your changes. See the section on building and testing below. Detailed instaructions can be found in INSTALL files in the release_docs directory.
* Push your changes to GitHub.
* Issues a pull request and address any code formatting and testing issues reported.

Once a pull request is correctly formatted and passes tests, it will be reviewed and evaluated by The HDF Group developers and HDF5 community members who has approval power.
The HDF Group developers will work with you to assure that the pull request satisfies acceptance criteria described in the next section. 

# Acceptance criteria for pull request <A NAME="criteria"></A>

We appreciate every contribution we receive, but we do not accept them all.  Those that we *do* accept satisfy the following criteria:

* **Pull request has a clear purpose** - What does the pull request address? How does it benefit the HDF5 community? 
If the pull request does not have a clear purpose and benifits it will not be accepted. 

* **The code is documented** - The HDF5 developers must understand not only *what* a change is doing, but *how* it is doing it.  Documenting the code makes it easier for us to understand the workflow of your patch and will help to maintaine the code in the future. 

* **The code passes HDF5 regression testing** - Any issue fixed or functionality add should be accompanied by corresponding tests. We do not expect you to perform comprehensive testing across a multitude of platforms, but at the very least your code should **compile** and 
no existing tests should be broken.  See "Building and testing your contribution" below for more information.

* **They do not compromise the principles behind HDF5** - HDF5 has a 100% commitment to backwards compatibility.  
	* Any file ever created with HDF5 must be readable by any future version of HDF5.  The HDF5 data model and file format are **well** defined.  If the purpose of your patch  is to modify the data model or file format, **please** discuss this with us first. File format changes and features required those changes can be introduced only in the new major releases. 
	* HDF5 has a commitment to remaining *machine-independent*; data created on one platform/environment/architecture **must** remain readable by HDF5 on another. 
	* For binary compatibilty no changes are allowed to public APIs and data structures in the maintenance releases; new APIs can be added.

* **New features are documented** - Any new features should be have proper documentation; talk to us if you have any questions.
# Testing your changes <A NAME="testing"></A>

There are several ways to test your changes before creating a pull request:

## Testing with Autotools <A NAME="autotools"></A>

Here is a list of commands (assuming you are building in place)
* `./autogen.sh`	- run the script in the top source directory to created `configure` 
* `./configure`		- run configure using appropriate flags; to see the flags run `./configure --help`.
* `make`		- build hDF5 libraries and tests
* `make check`		- run tests
* `make install`	- run tests

## Testing with CMake <A NAME="cmake"></A>
....


# Checklist <A NAME="checklist"></A>

Please make sure that you check the items applicable to your pull request:

* Documentation
  * [ ] Document change in release_docs/RELEASE.txt file
  * [ ] Update MANIFEST file (if adding new files to the source)
  * [ ] Document new function in the corresponding public header file using Doxygen
  * [ ] Document new functionality for the HDF5 community (the level of documentation depends on the feature; ask us what would be appropriate)
* Code 
  * [ ] Does the pull request have necessary tests?
  * [ ] Is the new code documented well enough for future maintenance?
  * [ ] If new feature is added, does it affect HDF5 library perfromance?
  * [ ] Is new code documented?

We want as many contributions as we can get, and are here to help!  Feel free to reach out to us if you have any questions!

