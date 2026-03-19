<div align="center">

![HDF5 Logo][u3]

[![BSD](https://img.shields.io/badge/License-BSD-blue.svg)](https://github.com/HDFGroup/hdf5/blob/develop/LICENSE)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.17808558-blue)](https://doi.org/10.5281/zenodo.17808558)
[![MIME Type](https://img.shields.io/badge/MIME%20Type-application%2Fvnd.hdfgroup.hdf5-orange)](https://www.iana.org/assignments/media-types/application/vnd.hdfgroup.hdf5)
[![develop cmake build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/call-workflows.yml?branch=develop&label=CMake%20CI)](https://github.com/HDFGroup/hdf5/actions/workflows/call-workflows.yml?query=branch%3Adevelop)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/7802/badge)](https://www.bestpractices.dev/projects/7802)

</div>

---

## What is HDF5?

This repository contains a high-performance library's source code and a file format
specification that implements the HDF5® data model. The model has been adopted across
many industries, and this implementation has become a de facto data management standard
in science, engineering, and research communities worldwide.

The HDF Group is the developer, maintainer, and steward of HDF5 software. Find more
information about The HDF Group, the HDF5 Community, and other HDF5 software projects,
tools, and services at [The HDF Group's website](https://www.hdfgroup.org/).

## Quick Start

- **New to HDF5?** Start with the [INSTALL.md](release_docs/INSTALL.md) guide for compilation and installation instructions.

- **Ready to build?** See [INSTALL_CMake.md](release_docs/INSTALL_CMake.md) for CMake-based builds.

- **Running on HPC?** Check out [README_HPC.md](release_docs/README_HPC.md) for parallel HDF5 configuration.

## Table of Contents

- [What is HDF5?](#what-is-hdf5)
- [Quick Start](#quick-start)
- [Documentation](#documentation)
- [Help and Support](#help-and-support)
- [Forum and News](#forum-and-news)
- [Release Schedule](#release-schedule)
- [Downloads and Source Code](#downloads-and-source-code)
- [Java Maven Artifacts](#java-maven-artifacts)
- [Contributing](#contributing)
- [How to Cite HDF5](#how-to-cite-hdf5)
- [Build Status](#build-status)

## Documentation

Documentation for all HDF software is available at:
- **All HDF Documentation**: https://support.hdfgroup.org/documentation/index.html
- **Latest HDF5 Library**: https://support.hdfgroup.org/documentation/hdf5/latest

See the [CHANGELOG.md][u1] file in the [release_docs/][u4] directory for information specific
to the features and updates included in this release of the library.

### Platform-Specific Guides

Several files in the [release_docs/][u4] directory provide platform-specific details:

| File | Description |
|------|-------------|
| [INSTALL](release_docs/INSTALL.md) | General compilation and installation instructions (start here) |
| [INSTALL_CMake.md](release_docs/INSTALL_CMake.md) | Building with CMake |
| [README_HPC.md](release_docs/README_HPC.md) | Building and configuring Parallel HDF5 on HPC systems |
| [INSTALL_Windows.md](release_docs/INSTALL_Windows.md) | Windows installation |
| [INSTALL_Cygwin.md](release_docs/INSTALL_Cygwin.md) | Cygwin installation |
| [USING_HDF5_CMake.md](release_docs/USING_HDF5_CMake.md) | Building HDF5 applications with CMake |
| [USING_CMake_Examples.md](release_docs/USING_CMake_Examples.md) | Building and testing HDF5 examples with CMake |

## Help and Support

The HDF Group staffs a free Help Desk accessible at https://help.hdfgroup.org and also monitors the [Forum](https://forum.hdfgroup.org). Our free support service is community-based and handled as time allows. We'll do our best to respond to your question as soon as possible, but please note that response times may vary depending on the complexity of the issue and staff availability.

If you're interested in guaranteed response and resolution times, a dedicated technical account manager, and more benefits (all while supporting the open-source work of The HDF Group), please check out [Priority Support](https://www.hdfgroup.org/solutions/priority-support/).

## Forum and News

The [HDF Forum](https://forum.hdfgroup.org) is provided for public announcements, technical questions, and discussions
of interest to the general HDF5 Community.

- [News and Announcements](https://forum.hdfgroup.org/c/news-and-announcements-from-the-hdf-group)
- [HDF5 Topics](https://forum.hdfgroup.org/c/hdf5)

These forums are provided as an open and public service for searching and reading.
Posting requires completing a simple registration and allows one to join in the
conversation. Please read the [quickstart guide](https://forum.hdfgroup.org/t/quickstart-guide-welcome-to-the-new-hdf-forum) for more information on how to get started.

## Release Schedule

![HDF5 release schedule][u2]

HDF5 does not follow a regular release schedule. Instead, updates are based on the
introduction of new features and the resolution of bugs. However, we aim to have at
least one annual release for each maintenance branch.

### Release Progress

[![Critical Priority](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/HDFGroup-Bot/0ad2eabb63b28eb90d69f5e5b2c1496f/raw/release-blocker-hdf5.json)](https://github.com/orgs/HDFGroup/projects/39/views/24)

[![High Priority](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/HDFGroup-Bot/0ad2eabb63b28eb90d69f5e5b2c1496f/raw/release-mustdo-hdf5.json)](https://github.com/orgs/HDFGroup/projects/39/views/24)

The badges above show the current progress of **critical** and **high priority** issues with colors that reflect completion status:

- 🟢 **Green (90%+)**: Readying for Deployment - most issues completed
- 🟡 **Yellow (60-89%)**: Nearing Completion - on track for release
- 🟠 **Orange (40-59%)**: In Development - attention needed
- 🔴 **Red (<40%)**: Initial Phase - significant issues remain

Click the badges to view the detailed project board with current release items.

## Downloads and Source Code

### Snapshots and Releases

- **Development Snapshots**: https://github.com/HDFGroup/hdf5/releases/tag/snapshot
- **Latest Release**: https://github.com/HDFGroup/hdf5/releases
- **Previous Releases**: https://support.hdfgroup.org/archive/support/ftp/HDF5/releases/index.html
- **Development Code**: https://github.com/HDFGroup/hdf5.git

### HPC Testing Results

[View HPC configure/build/test results on CDash](https://my.cdash.org/index.php?project=HDF5)

## Java Maven Artifacts

HDF5 Java bindings and examples are available as Maven artifacts. For detailed usage instructions including dependency configuration, repository setup, and platform-specific builds, see [HDF5Examples/JAVA/README-MAVEN.md](HDF5Examples/JAVA/README-MAVEN.md).

## Contributing

We welcome contributions to HDF5! Whether you're fixing bugs, adding features, or improving documentation, your help is appreciated.

### How to Contribute

1. **Report Issues**: Use our [GitHub Issues](https://github.com/HDFGroup/hdf5/issues) to report bugs or request features
2. **Submit Pull Requests**: Fork the repository, make your changes, and submit a PR
3. **Join Discussions**: Participate in the [HDF Forum](https://forum.hdfgroup.org)

For detailed contribution guidelines, please contact us through the [Help Desk](https://help.hdfgroup.org).

## How to Cite HDF5

If you use HDF5 in your research, please cite it. This repository includes a [`CITATION.cff`](CITATION.cff) file containing standard citation metadata.

**Quick DOI:** [10.5281/zenodo.17808558](https://doi.org/10.5281/zenodo.17808558)

## Build Status

<details>
<summary>Click to expand detailed build status</summary>

### Continuous Integration

[![HDF5 develop daily build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/daily-schedule.yml?branch=develop&label=Daily%20Build)](https://github.com/HDFGroup/hdf5/actions/workflows/daily-schedule.yml?query=branch%3Adevelop)
[![CVE regression](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/cve.yml?branch=develop&label=CVE%20Tests)](https://github.com/HDFGroup/hdf5/actions/workflows/cve.yml?query=branch%3Adevelop)
[![OSS-Fuzz Status](https://oss-fuzz-build-logs.storage.googleapis.com/badges/hdf5.svg)](https://oss-fuzz-build-logs.storage.googleapis.com/index.html#hdf5)
[![Link Checker Status](https://github.com/HDFGroup/hdf5/actions/workflows/linkchecker.yml/badge.svg)](https://github.com/HDFGroup/hdf5/actions/workflows/linkchecker.yml)

### Integration Testing

[![HDF-EOS5 build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/hdfeos5.yml?branch=develop&label=HDF-EOS5)](https://github.com/HDFGroup/hdf5/actions/workflows/hdfeos5.yml?query=branch%3Adevelop)
[![netCDF build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/netcdf.yml?branch=develop&label=netCDF)](https://github.com/HDFGroup/hdf5/actions/workflows/netcdf.yml?query=branch%3Adevelop)
[![h5py build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/h5py.yml?branch=develop&label=h5py)](https://github.com/HDFGroup/hdf5/actions/workflows/h5py.yml?query=branch%3Adevelop)

### VOL and VFD Testing

[![HDF5 VOL connectors build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/vol.yml?branch=develop&label=VOL%20Connectors)](https://github.com/HDFGroup/hdf5/actions/workflows/vol.yml?query=branch%3Adevelop)
[![HDF5 VFD build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/vfd.yml?branch=develop&label=VFD%20Tests)](https://github.com/HDFGroup/hdf5/actions/workflows/vfd.yml?query=branch%3Adevelop)

</details>

---

[u1]: https://github.com/HDFGroup/hdf5/blob/develop/release_docs/CHANGELOG.md
[u2]: https://github.com/HDFGroup/hdf5/blob/develop/release_docs/img/release-schedule.png
[u3]: https://github.com/HDFGroup/hdf5/blob/develop/doxygen/img/HDF5.png
[u4]: https://github.com/HDFGroup/hdf5/blob/develop/release_docs
