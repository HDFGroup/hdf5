# HDF5 Security Policy

<!-- SHINE:CONTROL-SET=HDF5; VERSION=2026-02; REPO=HDFGroup/hdf5 -->
<!-- SHINE:MAP=SHINE-HDF5-VULN-01, SHINE-HDF5-BUILD-03, SHINE-HDF5-DIST-02, SHINE-HDF5-PRIV-01 -->

This document covers HDF5-specific security information. For the general HDF Group security governance framework — including vulnerability management processes, triage SLAs, disclosure timelines, build provenance standards, testing policies, and researcher safe harbor — see the [HDF Group Security & Sustainability Policies](https://ssp.hdfgroup.org/policy/).

## Security Scope

HDF5 is a complex ecosystem involving core libraries, command-line tools, and a plugin architecture. To help researchers focus their efforts, we define the following scope for security reports.

### In Scope
We are interested in vulnerabilities that affect the confidentiality, integrity, or availability of the HDF5 library and its official tools.
*   **Core Library (`libhdf5`):** Memory corruption (buffer overflows, out-of-bounds reads/writes, use-after-free, double-free), integer overflows leading to memory issues.
*   **Command-line Tools:** Issues in `h5dump`, `h5repack`, `h5diff`, etc., particularly where they can be triggered by malformed input files.
*   **Remote Code Execution (RCE):** Any mechanism allowing arbitrary code execution via file parsing or API abuse.
*   **Supply Chain:** Compromise of release artifacts, signing keys, or build infrastructure.

### Usually In Scope (Triaged)
*   **Denial of Service (DoS):** Issues causing crashes or excessive resource consumption (CPU/RAM) via malformed HDF5 files.
    *   *Note:* While we treat these as bugs, severity is assessed based on impact. DoS in a command-line tool is often lower severity than DoS in the core library linked to a long-running service.

### Out of Scope
*   **Self-Inflicted Misconfigurations:** Security issues arising from insecure application-level usage of the library (e.g., applications setting weak file permissions).
*   **Third-Party Plugins:** Vulnerabilities in Filters, VOL (Virtual Object Layer) connectors or VFD (Virtual File Driver) plugins not maintained by The HDF Group. Please report these to the respective maintainers.
*   **Experimental Features:** Features that are:
    *   Enabled explicitly via build flags (e.g., `HDF5_ALLOW_UNSUPPORTED`).
    *   Marked as "Experimental" or "Unsupported" in the official API documentation, or in the release notes.

## Supported Versions

We adhere to a specific support matrix for security updates. We strongly advise all users to upgrade to the latest release to ensure they receive all current security patches.

**Current Version List:** For the exact version numbers currently supported (e.g., 1.14.x vs 1.12.x), please refer to our [Release Support Page](https://www.hdfgroup.org/solutions/hdf5) (or the `README.md` in this repository).

| Branch | Example | Security Support |
| :--- | :--- | :--- |
| **Current Release** | 2.11.x | All severity levels (Critical, High, Medium) |
| **Previous Major (if < 1 year old)** | 1.14.x | Critical vulnerabilities only (at maintainer discretion) |
| **All Other Versions** | 2.0-2.10, <1.14 | None (EOL - End of Life) |

### Support Model Example

If the current release is **2.11.3**:
- **2.11.x** (Active) - All security patches
- **1.14.x** (Maintenance) - Critical vulnerabilities only
- **2.0.x - 2.10.x** - EOL, no patches
- **1.12.x and older** - EOL, no patches

**Important:** We do not maintain every minor version. Users on intermediate releases (e.g., 2.7.x) must upgrade to a supported branch to receive security patches.

### Upgrade Path
Security patches are **NOT** backported to intermediate minor versions. Users must upgrade to a supported branch (current or previous major) to receive security updates.

### End of Life (EOL) Policy
We provide a minimum **6-month notice** before a Major version line enters EOL status. These announcements are made via our [GitHub Discussions](https://github.com/HDFGroup/hdf5/discussions).

### Backport Policy
1.  **Development First:** Fixes are applied to the `develop` branch first.
2.  **Backporting:**
    *   Fixes are backported to the current **Active** release branch.
    *   **Critical** vulnerabilities may be backported to the **previous** major release branch (if still in maintenance).
    *   We do not produce patches for EOL versions or intermediate minor releases.

## Reporting a Vulnerability

If you discover a security vulnerability in HDF5, please report it privately. **Do not disclose it publicly.**

**Primary Reporting Method:** Please report vulnerabilities via our [GitHub Security Advisory](https://github.com/HDFGroup/hdf5/security/advisories/new) page.

**Alternative Contact:** If you cannot use GitHub Security Advisories, you may email us at: `security@hdfgroup.org`.

Please include a clear description, reproduction steps, affected versions, and a proof of concept (e.g., the malformed HDF5 file) if possible.

For details on triage timelines, severity assessment (CVSS), disclosure deadlines, CVE issuance, and researcher safe harbor, see the [Vulnerability Management & Disclosure (PSIRT) Policy](https://ssp.hdfgroup.org/policy/Vulnerability%20Management%20%26%20Disclosure%20(PSIRT)%20Policy) and [Vulnerability Disclosure Policy](https://ssp.hdfgroup.org/policy/Vulnerability_Disclosure_Policy).

<!-- SHINE:CONTROL=SHINE-HDF5-TRIAGE-02; RUBRIC=docs/security/severity-rubric.md -->

## Security Expectations & Limitations

### What Users Can Expect
- **Memory Safety:** Ongoing efforts to prevent buffer overflows and leaks.
- **File Format Validation:** Basic validation of HDF5 file structures.

### Security Limitations
- **Untrusted Input:** HDF5 is a complex binary format. Reading HDF5 files from untrusted sources entails risk. We recommend running parsers in sandboxed environments if the data source is not trusted.
- **Thread Safety:** The library is not thread-safe by default unless built with `HDF5_ENABLE_THREADSAFE`, and even then, limitations apply.
- **Plugins:** Custom VFD/VOL plugins run with the application's privileges. Only use trusted plugins.

## Security Development Practices

For the full HDF Group security development framework, see the [SSP Policies](https://ssp.hdfgroup.org/policy/), which cover [Secure SDLC & Code Review](https://ssp.hdfgroup.org/policy/Secure%20SDLC%20%26%20Code%20Review%20Policy), [Security Testing & Fuzzing](https://ssp.hdfgroup.org/policy/Security%20Testing%20%26%20Fuzzing%20Policy), and [Secure Build, Provenance, & Distribution](https://ssp.hdfgroup.org/policy/Secure%20Build%2C%20Provenance%2C%20%26%20Distribution%20Policy).

HDF5-specific practices:
- **Artifact Verification:** All binaries and source tarballs on [GitHub Releases](https://github.com/HDFGroup/hdf5/releases) include SHA-256 checksums. Users should verify downloaded artifacts against these checksums.
- **SBOM:** We provide CycloneDX SBOM fragments for core, tools, and plugins where applicable.
- **Fuzzing:** Continuous fuzzing of HDF5 file parsing and API entry points via OSS-Fuzz.
- **Build from Source:** General build instructions are available in [docs/INSTALL.md](docs/INSTALL.md).

---

**Last Updated:** 2026-03-09

**Note:** The HDF Group maintains this project with community contributions. While we make every good faith effort to address security issues promptly, please understand that response times may vary based on available resources.
