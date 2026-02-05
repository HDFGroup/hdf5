# HDF5 Security Policy

<!-- SHINE:CONTROL-SET=HDF5; VERSION=2026-02; REPO={owner}/{repo} -->
<!-- SHINE:MAP=SHINE-HDF5-VULN-01, SHINE-HDF5-BUILD-03, SHINE-HDF5-DIST-02, SHINE-HDF5-PRIV-01 -->

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
    *   Enabled explicitly via build flags (e.g., `--enable-unsupported`).
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

### Reporting Process

If you discover a security vulnerability in HDF5, please report it privately. **Do not disclose it publicly.** This allows us to collaborate with you to address the issue before it is exposed to the public.

**Primary Reporting Method:** Please report vulnerabilities via our [GitHub Security Advisory](https://github.com/HDFGroup/hdf5/security/advisories/new) page.

**Alternative Contact:** If you cannot use GitHub Security Advisories, you may email us at: `security@hdfgroup.org`.

### What to Include in Your Report
To help us understand and address the vulnerability quickly, please include:
- **Description:** A clear description of the vulnerability and its potential impact.
- **Reproduction Steps:** Detailed steps to reproduce the issue.
- **Affected Versions:** Which versions of HDF5 are affected.
- **Proof of Concept:** Sample code, files (e.g., the malformed HDF5 file), or commands demonstrating the vulnerability.
- **Proposed Fix:** If you have suggestions for fixing the issue, please include them.

## Severity, Triage, and SLAs

We use CVSS v3.1 to assess severity. See our triage rubric for details. We distinguish between internal triage time and public release targets.

| Severity | Internal Triage & Plan | Public Release Target |
| :--- | :--- | :--- |
| **Critical** | ≤ 5 Business Days | ≤ 30 Days (Expedited) |
| **High** | ≤ 10 Business Days | ≤ 60 Days |
| **Medium/Low** | Standard Cycle | Next Regular Release |

<!-- SHINE:CONTROL=SHINE-HDF5-TRIAGE-02; RUBRIC=docs/security/severity-rubric.md -->

### Severity Definitions (Context)
To assist in triage, we define severity in the context of a file format library:
*   **Critical (CVSS 9.0+):** Vulnerabilities exploitable remotely **without** user interaction (e.g., network service exposure) or issues leading to Privilege Escalation.
*   **High (CVSS 7.0-8.9):** Remote Code Execution (RCE) or Memory Corruption that requires user interaction (e.g., a user must open a malicious HDF5 file).
*   **Medium/Low:** Denial of Service (DoS) or minor information leaks.

### Disclosure Deadline
We adhere to a standard **90-day disclosure deadline** that applies to all severity levels. Public disclosure occurs after a fix is released OR 90 days have elapsed, whichever comes first.

**Clarification on Release Targets vs. Disclosure Deadline:**
- The "Public Release Target" times in the table above (30 days for Critical, 60 days for High) are our **goals** for releasing fixes
- However, the **90-day cap applies to all severities** - if we cannot release a fix within our target timeframe, public disclosure will still occur at 90 days maximum
- Example: A Critical vulnerability targets a 30-day fix, but if unforeseen issues delay the fix, disclosure will occur at 90 days even if the fix is not ready

### CVE Issuance
For vulnerabilities that meet CVE criteria (exploitable by untrusted input, memory corruption, DoS, etc.), we will:
1.  Request a CVE identifier from GitHub's CVE Numbering Authority.
2.  Publish CVE details to NVD, GitHub Security Advisories, and Release Notes.

## Security Patch Management

### Applying Security Updates

1.  **Release Announcement:** Security fixes are announced through GitHub releases and security advisories.
2.  **Version Numbering:** Security patches are generally released as patch releases (e.g., 1.14.0 → 1.14.1).
3.  **Upgrade Recommendations:**
    *   **Critical/High:** Upgrade immediately (or within 30 days).
    *   **Medium/Low:** Upgrade during next maintenance window.

### Emergency Security Releases
For critical vulnerabilities actively being exploited, we may issue emergency releases outside the regular schedule. These will be clearly marked as security updates.

## Security Expectations & Limitations

### What Users Can Expect
- **Memory Safety:** Ongoing efforts to prevent buffer overflows and leaks.
- **File Format Validation:** Basic validation of HDF5 file structures.

### Security Limitations
- **Untrusted Input:** HDF5 is a complex binary format. Reading HDF5 files from untrusted sources entails risk. We recommend running parsers in sandboxed environments if the data source is not trusted.
- **Thread Safety:** The library is not thread-safe by default unless built with `HDF5_ENABLE_THREADSAFE`, and even then, limitations apply.
- **Plugins:** Custom VFD/VOL plugins run with the application's privileges. Only use trusted plugins.

## Security Development Practices

### Build Provenance & Artifact Verification
To ensure supply chain integrity:
- **Artifact Integrity:** All binaries and source tarballs distributed via GitHub Releases are generated with associated SHA-256 checksums.
- **Verification:** Users should verify the hash of downloaded artifacts against the checksums provided on the [GitHub Releases](https://github.com/HDFGroup/hdf5/releases) page to ensure file integrity.
- **SBOM:** We provide CycloneDX SBOM fragments for core, tools, and plugins where applicable.
- **Build from Source:** General build instructions are available in [release_docs/INSTALL](release_docs/INSTALL).

### TODO -CHECKINTO ::: Binary Hardening
We aim to produce secure binaries by leveraging the security features of our build infrastructure.
- **Compiler Flags:** Official binaries distributed via GitHub are built using standard compiler security options (e.g., stack protection) supported by the target platform and GitHub Actions environment.

### Testing Processes
- **Sanitizers:** CI builds with ASan, MSan, and UBSan.
- **Fuzzing:** Continuous fuzzing of file parsing and API entry points (OSS-Fuzz).
- **Static Analysis:** Automated scanning for common vulnerability patterns.
- **Code Review:** Peer review required for all changes.

## Security Researcher Recognition

We value the security research community and recognize responsible disclosure.

### Bug Bounty Program
We do not currently offer a bug bounty program. However, we deeply appreciate responsible disclosure and will publicly acknowledge researchers who help improve HDF5 security.

### Hall of Thanks & Safe Harbor
Researchers who responsibly disclose vulnerabilities will be credited in our advisories and release notes.

We support good-faith security research. We will not pursue legal action against researchers who:
- Report vulnerabilities promptly and do not exploit them beyond proof-of-concept.
- Do not perform denial of service attacks against production infrastructure.
- Keep vulnerability details confidential until a fix is issued or the disclosure deadline passes.
- Act in good faith and follow this policy.

---

**Last Updated:** 2026-02-04

**Note:** The HDF Group maintains this project with community contributions. While we make every good faith effort to address security issues promptly, please understand that response times may vary based on available resources.
