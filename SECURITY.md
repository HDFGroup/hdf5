








# HDF5 Security Policy

## Supported Versions

Security updates are provided only for the most recent version. We strongly advise all users to upgrade to the latest release to ensure they receive all current security patches.

| Version | Supported          |
| ------- | ------------------ |
| Latest  | :white_check_mark: |
| Older   | :x:                |

## Reporting a Vulnerability

### Reporting Process

If you discover a security vulnerability in HDF5, please report it privately. **Do not disclose it publicly.** This allows us to collaborate with you to address the issue before it is exposed to the public, reducing the likelihood that the vulnerability will be exploited before a patch is released.

**Primary Reporting Method:** Please report vulnerabilities via our [GitHub Security Advisory](https://github.com/HDFGroup/hdf5/security/advisories/new) page.

**Alternative Contact:** If you cannot use GitHub Security Advisories, you may email us at: `help@hdfgroup.org` with the subject line "SECURITY VULNERABILITY REPORT".

**Security Point of Contact:** For urgent security matters requiring immediate attention, please get in touch with the HDF Group security team via the channels above. We monitor these channels regularly.

### What to Include in Your Report

To help us understand and address the vulnerability quickly, please include:

- **Description:** A clear description of the vulnerability and its potential impact
- **Reproduction Steps:** Detailed steps to reproduce the issue
- **Affected Versions:** Which versions of HDF5 are affected
- **Proof of Concept:** Sample code, files, or commands demonstrating the vulnerability (if applicable)
- **Proposed Fix:** If you have suggestions for fixing the issue, please include them
- **Credit Information:** How you would like to be credited (if at all) when we publish the fix

### Response Timeline

We take security vulnerabilities seriously and will respond as follows:

- **Initial Response:** We will acknowledge receipt of your vulnerability report within **5 business days**
- **Status Updates:** We will provide status updates at least every **7 days** during our investigation
- **Triage:** We will assess the severity and impact within **14 days** of the initial report
- **Resolution:** We aim to release a fix within **90 days** for critical vulnerabilities, though timelines may vary based on complexity

### Disclosure Policy

We follow a **coordinated disclosure** approach:

- We will work with you to understand and validate the vulnerability
- We will develop and test a fix before public disclosure
- We follow a **90-day disclosure deadline** from the initial report
- If a fix is ready sooner, we may release it earlier with your agreement
- You will be credited in our release notes and security advisory (unless you prefer to remain anonymous)
- Public disclosure will occur after:
  - A fix has been released, OR
  - 90 days have elapsed, whichever comes first

### Security Advisory Publication

When a security vulnerability is fixed:

- We will publish a security advisory on [GitHub Security Advisories](https://github.com/HDFGroup/hdf5/security/advisories)
- The advisory will be included in our release notes
- We will assign a CVE identifier when appropriate
- We will credit the reporter unless anonymity is requested

### CVE Issuance and Process

For vulnerabilities that meet CVE criteria, we follow this process:

**CVE Assignment Criteria:**
- Security vulnerabilities that affect the confidentiality, integrity, or availability of HDF5
- Issues that can be exploited by untrusted input or unauthorized users
- Memory corruption vulnerabilities (buffer overflows, use-after-free, etc.)
- Denial of service vulnerabilities
- Information disclosure issues

**CVE Process:**
1. **Request:** We will request a CVE identifier from GitHub's CVE Numbering Authority (CNA) or MITRE
2. **Assignment:** CVE IDs are typically assigned within 72 hours of request
3. **Severity Assessment:** We use the Common Vulnerability Scoring System (CVSS) v3.1 to assess severity:
   - **Critical (9.0-10.0):** Immediate action required
   - **High (7.0-8.9):** Fix prioritized for next release
   - **Medium (4.0-6.9):** Fix included in regular release cycle
   - **Low (0.1-3.9):** Fix when feasible
4. **Publication:** CVE details are published to:
   - National Vulnerability Database (NVD)
   - GitHub Security Advisories
   - HDF5 release notes and security page
   - Relevant security mailing lists and databases

**CVE Information Included:**
- CVE identifier (e.g., CVE-2025-XXXXX)
- Affected versions and platforms
- CVSS score and vector string
- Description of the vulnerability
- Mitigation steps and workarounds (if available)
- Fixed version information
- Credit to the reporter
- References to patches and commits

### Encrypted Communication

For sensitive vulnerability reports, you may request encrypted communication. Please indicate this in your initial report, and we will provide a secure communication channel.

### Security Notification Channels

Users and integrators can stay informed about HDF5 security updates through:

- **GitHub Security Advisories:** [https://github.com/HDFGroup/hdf5/security/advisories](https://github.com/HDFGroup/hdf5/security/advisories)
- **GitHub Watch:** Enable "Security alerts" notifications on the HDF5 repository
- **Release Notes:** Security fixes are documented in the release notes for each version
- **National Vulnerability Database (NVD):** Search for "HDF5" at [https://nvd.nist.gov/](https://nvd.nist.gov/)
- **HDF Group Announcements:** Check [https://www.hdfgroup.org/](https://www.hdfgroup.org/) for security announcements

## Security Patch Management

### Applying Security Updates

When security fixes are released:

1. **Release Announcement:** Security fixes are announced through GitHub releases and security advisories
2. **Version Numbering:** Security patches are released as:
   - Patch releases (e.g., 2.1.1 → 2.1.2) for the current major version
   - In exceptional cases, critical security fixes may be backported to recent prior versions at the maintainers' discretion
3. **Upgrade Recommendations:**
   - **Critical vulnerabilities:** Upgrade immediately
   - **High vulnerabilities:** Upgrade within 30 days
   - **Medium vulnerabilities:** Upgrade during next maintenance window
   - **Low vulnerabilities:** Upgrade during normal update cycle

### Update Verification

After applying security updates:

- Verify the version using `H5get_libversion()` or `h5dump --version`
- Review the release notes to confirm the fix is included
- Re-run your test suite to ensure compatibility
- Check for any deprecated functionality or API changes

### Emergency Security Releases

For critical vulnerabilities actively being exploited:

- We may issue emergency releases outside the regular release schedule
- These releases will be clearly marked as security updates
- We will provide expedited documentation and migration guidance

## Security Expectations

### What Users Can Expect

HDF5 is designed to provide:

- **Data Integrity:** Protection against unintended data corruption
- **File Format Validation:** Basic validation of HDF5 file structures to detect corruption
- **Memory Safety:** Ongoing efforts to prevent buffer overflows and memory leaks
- **API Stability:** Security fixes maintain backward compatibility whenever possible

### Security Limitations

Users should be aware of the following security considerations:

- **Untrusted Input:** HDF5 is not designed to handle maliciously crafted files safely. Reading untrusted HDF5 files may pose security risks. Continuously validate and sanitize input from untrusted sources
- **Thread Safety:** The library is not thread-safe by default unless built with thread-safety enabled (`HDF5_ENABLE_THREADSAFE`). Even with this option, its thread-safety capabilities are severely limited
- **Parallel I/O:** When using parallel I/O features, ensure your MPI implementation and file system are properly secured
- **File Permissions:** HDF5 respects system file permissions but does not implement additional access control mechanisms
- **Compression Filters:** Security of compression depends on the underlying compression library (zlib, szip, etc.)

### Known Security Considerations

- **File Parsing:** Complex file formats can contain parsing vulnerabilities. We recommend using HDF5 only with trusted data sources or implementing additional validation
- **Plugin Architecture:** Custom VFD (Virtual File Driver) and VOL (Virtual Object Layer) plugins run with the same privileges as your application. Only use trusted plugins
- **External Links:** HDF5 files can contain links to external files. Be cautious when opening files from untrusted sources, as they may reference unexpected file system locations

## Security Development Practices

The HDF5 project follows these security practices:

- **Code Review:** All code changes undergo peer review before merging
- **Testing:** Comprehensive test suite including unit tests, integration tests, and regression tests
- **Continuous Integration:** Automated testing on multiple platforms and compilers
- **Static Analysis:** Regular use of static analysis tools to detect potential vulnerabilities
- **Address Sanitizer:** Testing with AddressSanitizer, MemorySanitizer, and UndefinedBehaviorSanitizer
- **Fuzzing:** Ongoing fuzzing efforts to discover parsing and memory safety issues
- **Dependency Management:** Regular updates to third-party dependencies
- **Security Advisories:** Monitoring security advisories for all dependencies

### Security Testing Tools and Processes

We employ multiple layers of security testing:

**Automated Testing:**
- **Sanitizer Builds:** Regular CI builds with ASan, MSan, UBSan to detect memory issues
- **Static Analysis:** Automated scanning for common vulnerability patterns
- **Dependency Scanning:** Automated checks for vulnerable dependencies
- **Fuzzing Infrastructure:** Continuous fuzzing of file parsing and API entry points

**Manual Security Reviews:**
- Code review focused on security-sensitive changes
- Periodic security audits of critical components
- Review of new features for security implications

**Compliance and Standards:**
- Following CERT C Coding Standards where applicable
- Adhering to CWE (Common Weakness Enumeration) guidelines
- OpenSSF Best Practices Badge compliance
- Regular security training for core maintainers

## Contributing Securely

If you are contributing to HDF5, please:

- Follow secure coding practices (see our [CONTRIBUTING.md](CONTRIBUTING.md))
- Run the test suite before submitting changes: `ctest`
- Report any security concerns you discover during development
- Do not commit sensitive information (credentials, keys, etc.) to the repository
- Sign your commits when possible to verify authenticity

## Security Resources

- **GitHub Security:** [https://github.com/HDFGroup/hdf5/security](https://github.com/HDFGroup/hdf5/security)
- **Release Notes:** Check release notes for security-related fixes
- **HDF Group Website:** [https://www.hdfgroup.org/](https://www.hdfgroup.org/)
- **OSSF Best Practices:** This project strives to meet [OpenSSF Best Practices Badge](https://bestpractices.coreinfrastructure.org/) criteria

## Security Researcher Recognition

We value the security research community and recognize responsible disclosure:

### Hall of Thanks

Security researchers who responsibly disclose vulnerabilities will be:
- Credited in our security advisories (unless anonymity is preferred)
- Listed in our release notes for the version containing the fix
- Acknowledged in our project documentation
- Provided with confirmation when fixes are released

### Safe Harbor

We support good-faith security research and will not pursue legal action against researchers who:

- Make a reasonable faith effort to comply with this security policy
- Report vulnerabilities promptly and do not exploit them beyond what is necessary to demonstrate the issue
- Do not access, modify, or delete data beyond what is necessary to demonstrate the vulnerability
- Do not perform denial of service attacks or intentionally degrade service performance
- Do not violate privacy by accessing or exfiltrating user data
- Keep vulnerability details confidential until we have issued a fix or 90 days have elapsed

**Legal Protection:** If your security research activities comply with this policy, we will:

- Not pursue or support legal action against you
- Work with you to understand and resolve the issue quickly
- Consider your activities to be authorized security research conducted under this policy

This policy is designed to be compatible with standard vulnerability disclosure good practices. It does not give you permission to act in ways inconsistent with the law, or test third-party services or systems. If a third party initiates legal action against you and you have complied with this security policy, we will take steps to make it clear that your actions were conducted in compliance with this policy.

### What We Expect from Researchers

To maintain a collaborative security research environment and qualify for Safe Harbor protection:
- **Do not** exploit vulnerabilities beyond proof-of-concept demonstration
- **Do not** access, modify, or delete data that does not belong to you
- **Do not** perform denial of service attacks against production systems
- **Do not** publicly disclose vulnerabilities before we have released a fix (unless the 90-day deadline has passed)
- **Do** provide sufficient detail for us to reproduce and fix the issue
- **Do** give us a reasonable time to respond and develop fixes
- **Do** act in good faith and follow this security policy

### What Researchers Can Expect from Us

We commit to:
- Acknowledge your report promptly (within 5 business days)
- Keep you informed of our progress
- Credit your responsible disclosure
- Work collaboratively to understand and fix the issue
- Treat all researchers with respect and professionalism

## Acknowledgments

We thank the security research community for their responsible disclosure of vulnerabilities and their contributions to making HDF5 more secure.

### Past Security Contributors

We maintain a list of security researchers who have contributed to improving HDF5 security. Thank you to all contributors who have responsibly disclosed vulnerabilities.

---

**Last Updated:** 2025-12-30

**Note:** The HDF Group maintains this project with community contributions. While we make every effort to address security issues promptly, please understand that response times may vary based on available resources.

