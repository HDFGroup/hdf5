# HDF5 Plugin Digital Signature Guide

## Table of Contents

1. [Overview](#overview)
2. [Quick Start](#quick-start)
3. [For Plugin Developers](#for-plugin-developers)
4. [For Plugin Users](#for-plugin-users)
5. [Security Considerations](#security-considerations)
6. [Troubleshooting](#troubleshooting)
7. [Technical Details](#technical-details)

---

## Overview

HDF5 plugin digital signatures provide cryptographic verification of plugin authenticity and integrity. This feature:

- **Prevents tampering**: Detects if a plugin binary has been modified
- **Ensures authenticity**: Verifies the plugin comes from a trusted source
- **Maintains compatibility**: Optional feature that doesn't affect unsigned plugins (when not required)

### Key Features

✅ RSA-based digital signatures (4096-bit recommended, 2048-bit minimum)
✅ Multiple hash algorithms (SHA-256, SHA-384, SHA-512)
✅ PSS padding support for enhanced security
✅ Multi-key keystore (accept plugins from multiple trusted developers)
✅ Plugins verified once per load (already cached by plugin loader)
✅ Detailed diagnostic error messages

---

## Quick Start

### For Plugin Users (5 minutes)

```bash
# 1. Obtain the public key from your plugin developer
#    (usually named something like: developer_public.pem)

# 2. Create a keystore directory
mkdir -p ~/.hdf5/keystore

# 3. Copy the public key to the keystore
cp developer_public.pem ~/.hdf5/keystore/

# 4. Set the environment variable
export HDF5_PLUGIN_KEYSTORE=~/.hdf5/keystore

# 5. Use HDF5 normally - signed plugins will be verified automatically!
```

### For Plugin Developers (10 minutes)

```bash
# 1. Generate an RSA key pair (one-time setup)
openssl genrsa -out my_private_key.pem 4096
openssl rsa -in my_private_key.pem -pubout -out my_public_key.pem

# 2. Build your plugin
gcc -shared -fPIC -o my_plugin.so my_plugin.c -lhdf5

# 3. Sign your plugin
h5sign -p my_plugin.so -k my_private_key.pem

# 4. Distribute both the signed plugin and public key to users
#    IMPORTANT: Keep my_private_key.pem secret!
```

---

## For Plugin Developers

### Step 1: Generate Your Signing Keys (One-Time Setup)

#### Generate a Private Key (4096-bit RSA recommended)

```bash
# Generate private key (KEEP THIS SECRET!)
openssl genrsa -out my_private_key.pem 4096

# Secure the private key (Unix/Linux/macOS)
chmod 600 my_private_key.pem
```

**IMPORTANT**: Store `my_private_key.pem` securely:

- Use a password manager or secure vault
- Never commit it to version control
- Never share it publicly
- Consider using a hardware security module (HSM) for production

#### Generate the Public Key

```bash
# Extract public key from private key
openssl rsa -in my_private_key.pem -pubout -out my_public_key.pem
```

This creates `my_public_key.pem` which you'll distribute to users.

### Step 2: Build Your Plugin

Build your plugin as usual:

```bash
# Example compilation
gcc -shared -fPIC -o my_filter_plugin.so my_filter.c -I/usr/include/hdf5 -lhdf5

# Or using CMake
cmake --build . --target my_plugin
```

### Step 3: Sign Your Plugin

Use the `h5sign` tool to add a digital signature:

```bash
# Basic signing
h5sign -p my_filter_plugin.so -k my_private_key.pem

# Verbose output (recommended for first-time)
h5sign -p my_filter_plugin.so -k my_private_key.pem -v
```

**What happens**:

1. h5sign computes a SHA-512 hash of your plugin binary
2. Signs the hash with your private key using RSA
3. Appends the signature and metadata to the end of the plugin file

**Output**:

```text
HDF5 Plugin Signature Tool
===========================

Using default hash algorithm: sha512
Reading private key from 'my_private_key.pem'...
Private key loaded successfully

Signing plugin 'my_filter_plugin.so'...

Plugin signed successfully!
  File:           my_filter_plugin.so
  Original size:  45,632 bytes
  Hash algorithm: SHA-512 (0x03)
  Signature size: 512 bytes
  Footer size:    12 bytes
  Final size:     46,156 bytes

SECURITY REMINDERS:
  - Keep your private key secure (chmod 600 my_private_key.pem)
  - Never share or commit your private key
  - Test the signed plugin before deployment
```

### Step 4: Verify Your Signature (Optional but Recommended)

Test that the signature was applied by inspecting the end of the plugin file:

```bash
# The last 12 bytes are the footer; the first 4 bytes of the footer are the magic number
# On-disk the magic 0x48444635 is stored little-endian, so the bytes appear as: 35 46 44 48
hexdump -C my_filter_plugin.so | tail -5
# Look for the byte sequence "35 46 44 48" starting 12 bytes from the end

# To do a quick sanity-check with OpenSSL, verify the signature manually:
openssl dgst -sha512 -verify my_public_key.pem \
  -signature <(dd if=my_filter_plugin.so bs=1 skip=<original_size> count=512 2>/dev/null) \
  <(dd if=my_filter_plugin.so bs=1 count=<original_size> 2>/dev/null)
```

> **Note**: `h5signverifytest` is an internal HDF5 test harness, not a general-purpose verification tool. It uses hardcoded test filenames and requires pre-generated test data — do not use it to verify your own plugins.

### Step 5: Distribute Your Plugin

Provide users with:

1. **The signed plugin**: `my_filter_plugin.so` (or .dll on Windows)
2. **The public key**: `my_public_key.pem`
3. **Installation instructions** (see user section below)

#### Example Distribution README

````markdown
# MyFilter Plugin Installation

## Files

- my_filter_plugin.so - The signed plugin binary
- my_public_key.pem - Public key for signature verification

## Installation

1. Copy the plugin to your HDF5 plugin directory:

   ```bash
   cp my_filter_plugin.so /usr/local/hdf5/lib/plugin/
   ```

2. Set up signature verification:

   ```bash
   mkdir -p ~/.hdf5/keystore
   cp my_public_key.pem ~/.hdf5/keystore/
   export HDF5_PLUGIN_KEYSTORE=~/.hdf5/keystore
   ```

3. Test the plugin:

   ```bash
   # Your test command here
   ```

````

### Advanced: Passphrase-Protected Private Keys

You can generate a private key encrypted with a passphrase for an additional layer of
protection. If the key file is ever copied or stolen, the attacker cannot use it without
the passphrase.

#### Generating a Passphrase-Protected Key

```bash
# Generate a 4096-bit RSA key encrypted with AES-256
# OpenSSL will prompt you to set a passphrase
openssl genrsa -aes256 -out private_enc.pem 4096

# Extract the public key (public key is never encrypted)
openssl rsa -in private_enc.pem -pubout -out my_public_key.pem
```

#### Using a Passphrase-Protected Key with h5sign

h5sign passes `NULL` as the OpenSSL password callback, which causes OpenSSL to use
its default behavior: **prompt the terminal for the passphrase**.

**Interactive use** (terminal present) — works automatically:

```bash
h5sign -p my_plugin.so -k private_enc.pem
# OpenSSL will prompt:
#   Enter PEM pass phrase: ****
```

**Non-interactive use** (CI/CD pipelines, scripts, shell redirection) — the terminal
prompt has nowhere to go, so OpenSSL fails with a `bad decrypt` or
`Could not read private key` error.

The recommended workaround for automation is to decrypt to a temporary file, sign,
and then securely delete it:

```bash
# 1. Decrypt to a temp file (permissions set before writing)
TMPKEY=$(mktemp)
chmod 600 "$TMPKEY"
openssl rsa -in private_enc.pem -out "$TMPKEY" -passin env:KEY_PASSPHRASE

# 2. Sign the plugin
h5sign -p my_plugin.so -k "$TMPKEY"

# 3. Securely delete the temporary plaintext key
shred -vfz -n 3 "$TMPKEY" 2>/dev/null || rm -f "$TMPKEY"
unset TMPKEY
```

Store the passphrase as a CI secret (e.g., GitHub Actions `secrets.KEY_PASSPHRASE`)
and never log or echo it.

#### Trade-offs

| Approach | Protection at rest | Works non-interactively | Complexity |
| --- | --- | --- | --- |
| Unprotected key (`chmod 600`) | Filesystem permissions only | ✅ Yes | Low |
| Passphrase-protected key | Passphrase + filesystem | ⚠️ Requires temp-file workaround | Medium |
| HSM | Hardware-enforced | ✅ Yes (with PKCS#11) | High |

**Recommendation**: For developer workstations, a passphrase-protected key is a good
default. For CI/CD pipelines, store either an unprotected key or the passphrase as a
protected secret, and ensure the signing host itself is trusted.

> **Note**: h5sign enforces that the key file is not group- or world-readable
> (`chmod 600`). This check is performed on the open file descriptor to prevent
> TOCTOU attacks. Both encrypted and unencrypted keys must satisfy this requirement.

---

### Advanced: Using Different Hash Algorithms

The `h5sign` tool supports multiple algorithms:

```bash
# SHA-512 (default, strongest)
h5sign -p plugin.so -k private.pem

# SHA-384 (strong, slightly smaller signature)
h5sign -p plugin.so -k private.pem -a sha384

# SHA-256 (good, smallest signature)
h5sign -p plugin.so -k private.pem -a sha256

# PSS padding (enhanced security)
h5sign -p plugin.so -k private.pem -a sha512-pss
```

---

## For Plugin Users

### Step 1: Obtain the Public Key

Get the public key from your plugin developer through a trusted channel:

- Official website (HTTPS)
- Package manager
- Direct communication with developer
- Official source code repository

**Verify the key authenticity** (if possible):

- Check SHA-256 fingerprint published on developer's website
- Verify PGP signature on the key file
- Contact developer to confirm fingerprint

```bash
# Check the key fingerprint
openssl rsa -pubin -in developer_public.pem -outform DER | sha256sum
```

### Step 2: Set Up Your Keystore

#### Option A: User-Specific Keystore (Recommended)

```bash
# Create keystore directory
mkdir -p ~/.hdf5/keystore

# Set restrictive permissions (Unix/Linux/macOS)
chmod 700 ~/.hdf5/keystore

# Copy public key(s) to keystore
cp developer1_public.pem ~/.hdf5/keystore/
cp developer2_public.pem ~/.hdf5/keystore/

# Set environment variable
export HDF5_PLUGIN_KEYSTORE=~/.hdf5/keystore
```

Add to your shell profile (`.bashrc`, `.zshrc`, etc.):

```bash
echo 'export HDF5_PLUGIN_KEYSTORE=~/.hdf5/keystore' >> ~/.bashrc
```

#### Option B: System-Wide Keystore (Requires Admin)

```bash
# Create system keystore (as root/admin)
sudo mkdir -p /etc/hdf5/keystore
sudo chmod 755 /etc/hdf5/keystore

# Copy public keys
sudo cp developer_public.pem /etc/hdf5/keystore/

# Set system-wide environment (add to /etc/environment or similar)
echo 'HDF5_PLUGIN_KEYSTORE=/etc/hdf5/keystore' | sudo tee -a /etc/environment
```

#### Option C: Project-Specific Keystore

```bash
# For a specific project
cd /path/to/my_project
mkdir keystore
cp developer_public.pem keystore/

# Use in your scripts
export HDF5_PLUGIN_KEYSTORE=/path/to/my_project/keystore
./run_analysis.sh
```

### Step 3: Install the Signed Plugin

Copy the signed plugin to your HDF5 plugin directory:

```bash
# Find your HDF5 plugin directory (h5cc may not be present in all installations)
h5cc -showconfig | grep "Default plugin path"

# Common locations:
# Linux:   /usr/local/hdf5/lib/plugin
# macOS:   /usr/local/hdf5/lib/plugin
# Windows: C:\Program Files\HDF_Group\HDF5\1.14.x\bin\plugin

# Copy plugin
cp my_filter_plugin.so /usr/local/hdf5/lib/plugin/
```

### Step 4: Verify Setup

Test that signature verification works:

```bash
# Set keystore environment variable
export HDF5_PLUGIN_KEYSTORE=~/.hdf5/keystore

# Try opening a file that uses the plugin
# If successful, you should see no errors
h5dump -H file_using_plugin.h5
```

### Step 5: Use HDF5 Normally

Once set up, HDF5 will automatically verify signatures when loading plugins:

```python
# Python example with h5py
import h5py
import os

# Ensure keystore is set
os.environ['HDF5_PLUGIN_KEYSTORE'] = os.path.expanduser('~/.hdf5/keystore')

# Use HDF5 normally - plugins are verified automatically
with h5py.File('data.h5', 'r') as f:
    data = f['dataset'][:]  # Plugin verified on first access
```

```c
// C example
#include "hdf5.h"

int main() {
    // Set keystore programmatically (optional, can use environment)
    setenv("HDF5_PLUGIN_KEYSTORE", "/home/user/.hdf5/keystore", 1);

    // Use HDF5 normally - plugins verified automatically
    hid_t file_id = H5Fopen("data.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
    // ... work with file ...
    H5Fclose(file_id);
    return 0;
}
```

### Managing Multiple Developers

The keystore can contain public keys from multiple developers:

```bash
# Keystore structure
~/.hdf5/keystore/
├── developer_alice_public.pem
├── developer_bob_public.pem
├── organization_xyz_public.pem
└── trusted_vendor_public.pem
```

HDF5 will try all keys and accept the plugin if ANY key verifies successfully.

---

## Security Considerations

Plugin signatures work seamlessly in air-gapped environments since all
cryptographic operations are performed locally using OpenSSL. No internet
connectivity is required for signing or verification.

### Keystore Security Requirements

The keystore directory contains the public keys used to verify plugin
signatures. Protecting its integrity is critical: if an attacker can add a
key to the keystore, they can make HDF5 trust their malicious plugins.

**The administrator is responsible for securing the keystore directory.**
HDF5 does not enforce filesystem permissions.

Recommended permissions:

```bash
# Unix/Linux/macOS
# User-specific keystore (owner-only access):
chmod 700 ~/.hdf5/keystore

# System-wide keystore (root-writable, world-readable):
sudo chmod 755 /etc/hdf5/keystore
sudo chown root:root /etc/hdf5/keystore
```

```powershell
# Windows — restrict write access to Administrators:
icacls "C:\ProgramData\HDF_Group\keystore" /inheritance:r /grant:r Administrators:F /grant:r Users:RX
```

**Key principles:**
- Only trusted administrators should have write access to the keystore directory
- Public keys are not secret, but their integrity must be protected
- Use system-protected paths for shared installations
- Audit keystore contents periodically

### Security Model Overview

HDF5 plugin signatures use **RSA-based digital signatures** with **multi-key trust management** (KeyStore) to verify plugin authenticity and integrity.

#### What This System Protects Against

✅ **Fully Protected:**

- **Malicious unsigned plugins**: Blocks execution of plugins without valid signatures
- **Tampered plugins**: Detects any modification to signed plugin binaries
- **Supply chain attacks**: Prevents loading of plugins from untrusted sources (if keys are protected)
- **Multi-vendor scenarios**: Supports plugins from multiple trusted developers simultaneously

#### Known Limitations

⚠️ **Not Protected (By Design):**

1. **No Automatic Revocation**

   - **Issue**: If a developer's private key is compromised, there's no automatic way to revoke it
   - **Response**: Manual removal from keystore + community notification
   - **Timeline**: Days to weeks (vs. instant with PKI/CRL)
   - **Mitigation**: Strong key protection, regular key rotation, incident response plan

2. **No Rollback Protection**

   - **Issue**: System doesn't enforce version constraints (could load old vulnerable version)
   - **Mitigation**: Use trusted distribution channels (GitHub releases), verify checksums, maintain version policies
   - **Note**: Signature proves authenticity, not freshness

3. **No Expiration Dates**

   - **Issue**: Signed plugins remain valid indefinitely
   - **Mitigation**: Regular plugin updates, organizational policies for maximum plugin age

4. **Manual Trust Management**

   - **Issue**: Users must manually add/remove trusted keys from keystore
   - **Mitigation**: Clear documentation, automation scripts, periodic keystore audits

#### Threat Coverage Comparison

| Attack Vector | Protected? | How? |
| --------------- | ----------- | ------ |
| Unsigned malicious plugin | ✅ Yes | Signature required |
| Modified legitimate plugin | ✅ Yes | Signature invalidates |
| Plugin from untrusted source | ✅ Yes | KeyStore verification |
| Compromised developer key | ⚠️ Delayed | Manual revocation |
| Rollback to old version | ❌ No | Use checksums + policies |
| MITM during download | ⚠️ Partial | Use HTTPS + checksums |

#### Security vs. Cost Trade-offs

**Why Not Full PKI?**

The current system makes conscious trade-offs:

| Feature | Current (RSA + KeyStore) | Full PKI/CA |
| ------- | ------------------------- | ----------- |
| Operational cost | $0/year | $50K-120K/year |
| Staffing | 0 FTE | 0.5-1 FTE |
| Air-gap support | Perfect | Difficult |
| Revocation speed | Manual (days) | Automatic (instant) |
| Setup complexity | Low | High |
| Maintenance | Minimal | Significant |

**Decision Rationale:**

- HDF5 plugin ecosystem is small (~20-30 plugins)
- No incidents requiring revocation to date
- Air-gapped environments are common (CRL problematic)
- Limited funding and staffing available
- Primary threats (malicious/tampered plugins) are fully mitigated

**Escalation Triggers** (when to consider PKI):

- Key compromise incident occurs
- Multiple security vulnerabilities in plugins
- Regulatory requirements mandate revocation capability
- Ecosystem grows significantly (100+ plugins)
- Dedicated security funding secured

#### Best Practices Summary

**For Maximum Security:**

1. **Developers**: Protect private keys (HSM preferred), rotate regularly, use strong keys (4096-bit)
2. **Users**: Verify public key fingerprints, use checksums, trust official channels only
3. **Organizations**: Maintain approved plugin lists, audit keystores quarterly, have incident response plan
4. **Offline/air-gapped**: All operations are local; no internet required for signing or verification

**Security is a shared responsibility** between plugin developers, distributors, and users.

---

### Developer Security

1. **Private Key Security**

   **Storage Best Practices:**

   ```bash
   # Generate key with strong passphrase
   openssl genrsa -aes256 -out private.pem 4096

   # Store in secure location (in order of preference):
   # 1. Hardware Security Module (HSM) - BEST
   # 2. Encrypted filesystem with strong passphrase - GOOD
   # 3. GitHub Secrets (for CI/CD automation) - ACCEPTABLE
   # 4. Password manager (1Password, LastPass, etc.) - MINIMUM

   # NEVER:
   # - Commit to version control (git, SVN, etc.)
   # - Store unencrypted on disk
   # - Share between developers/organizations
   # - Email or send via chat
   # - Use same key for multiple projects
   ```

   **Key Rotation Policy:**

   ```text
   # Rotate every 1-2 years, or immediately if:
   # - Developer leaves organization
   # - System compromise suspected
   # - Security incident occurs
   # - Regulatory compliance requires

   # Rotation process:
   1. Generate new key pair
   2. Sign NEW releases with new key only
   3. Keep old public key in keystores for 6-12 months (transition period)
   4. Notify all users of key change (email, website, docs)
   5. Securely destroy old private key: shred -vfz -n 10 old_private.pem
   6. Document rotation in changelog/security log
   ```

2. **Build Environment Security**

   - ✅ Sign plugins on a trusted, isolated system (air-gapped preferred)
   - ✅ Verify build toolchain integrity (verify compiler, dependencies)
   - ✅ Use reproducible builds when possible
   - ✅ Scan plugins for vulnerabilities before signing
   - ✅ Maintain audit log of all signing operations
   - ✅ Use CI/CD with protected secrets (GitHub Actions, GitLab CI)

3. **Distribution Security**

   **Secure Distribution Checklist:**

   ```bash
   # 1. Sign the plugin
   h5sign -p my_plugin.so -k private.pem -v

   # 2. Generate checksum
   sha256sum my_plugin.so > my_plugin.so.sha256

   # 3. Create release package
   tar -czf my_plugin_v1.0.0.tar.gz my_plugin.so my_plugin.so.sha256 public.pem

   # 4. Publish to trusted channel (GitHub Releases)
   gh release create v1.0.0 my_plugin_v1.0.0.tar.gz \
     --title "My Plugin v1.0.0" \
     --notes "Release notes here..."

   # 5. Publish public key fingerprint (for verification)
   openssl rsa -pubin -in public.pem -outform DER | sha256sum
   # Document this fingerprint in README, website, security.txt
   ```

   **Additional Measures:**

   - ✅ Use HTTPS for all downloads
   - ✅ Provide checksums alongside plugins (SHA-256 minimum)
   - ✅ Publish public key fingerprint on multiple channels (website, README, DNS TXT record)
   - ✅ Sign releases with PGP/GPG in addition to plugin signatures (defense in depth)
   - ✅ Use official distribution channels only (GitHub Releases, organization website)
   - ✅ Never distribute plugins via email, chat, or untrusted file shares

4. **Incident Response Plan**

   **If Private Key Compromised:**

   ```text
   Hour 0:    Discovery
              - Immediately stop using compromised key
              - Alert security team

   Hour 1-4:  Notification
              - Notify all plugin users via email, GitHub, website
              - Document compromised key fingerprint
              - Provide timeline and scope of potential impact

   Day 1:     Emergency Response
              - Generate new key pair
              - Re-sign all legitimate plugins with new key
              - Publish new public key and updated keystores
              - Remove old public key from documentation

   Week 1-2:  Cleanup
              - Verify all users have updated keystores
              - Monitor for suspicious signed plugins
              - Document incident and lessons learned
              - Update security procedures
   ```

### User Security

1. **Keystore Security**

   **Verifying Public Key Authenticity (Critical!):**

   ```bash
   # NEVER trust a public key without verification!
   # Use out-of-band verification methods:

   # Method 1: Verify fingerprint via official website/documentation
   openssl rsa -pubin -in developer_public.pem -outform DER | sha256sum
   # Output: abc123def456...
   # Compare with fingerprint published on developer's website (HTTPS)

   # Method 2: Verify via official communication
   # - Contact developer via phone/video call
   # - Verify fingerprint through official support channel
   # - Check organization's security contact

   # Method 3: Check package signatures
   # If distributed via package manager (apt, yum, conda):
   gpg --verify developer_public.pem.asc developer_public.pem
   ```

   **Keystore Management:**

   ```bash
   # Periodic audit (quarterly recommended)
   ls -lh $HDF5_PLUGIN_KEYSTORE/

   # Review each key:
   for key in $HDF5_PLUGIN_KEYSTORE/*.pem; do
     echo "=== $key ==="
     openssl rsa -pubin -in "$key" -text -noout | head -n3
     openssl rsa -pubin -in "$key" -outform DER | sha256sum
   done

   # Remove obsolete/untrusted keys
   sudo rm $HDF5_PLUGIN_KEYSTORE/old_untrusted_key.pem

   # Add new trusted key (after verification!)
   sudo cp new_verified_key.pem $HDF5_PLUGIN_KEYSTORE/

   # Document changes in security log
   echo "$(date): Removed old_untrusted_key.pem (developer inactive)" >> /var/log/hdf5/keystore.log
   echo "$(date): Added new_verified_key.pem (fingerprint: abc123...)" >> /var/log/hdf5/keystore.log
   ```

   - ✅ Verify public key authenticity BEFORE adding to keystore
   - ✅ Protect keystore directory from unauthorized writes (permissions check below)
   - ✅ Audit keystore contents quarterly (review all keys)
   - ✅ Remove keys for obsolete/untrusted developers promptly
   - ✅ Document all keystore changes in security log
   - ✅ Use system-wide keystore for organizational deployments

2. **Plugin Verification Best Practices**

   **Download and Verify Workflow:**

   ```bash
   # 1. Download from trusted source (HTTPS)
   wget https://github.com/DEVELOPER/plugin/releases/download/v1.0.0/my_plugin.so

   # 2. Verify checksum (if provided)
   sha256sum my_plugin.so
   # Compare with checksum published on release page

   # 3. Verify the public key is in your keystore
   ls -la $HDF5_PLUGIN_KEYSTORE/developer_public.pem

   # 4. Install the plugin (HDF5 will verify signature automatically at load time)
   sudo cp my_plugin.so /usr/local/hdf5/lib/plugin/
   ```

   **Monitoring and Logging:**

   ```bash
   # Monitor for verification failures in system logs
   grep "signature verification failed" /var/log/syslog

   # Check which plugins are loaded
   # (Application-specific, consult HDF5 documentation)
   ```

   - ✅ Always set `HDF5_PLUGIN_KEYSTORE` before using plugins
   - ✅ Monitor verification errors in logs (investigate failures immediately)
   - ✅ Don't disable signature verification in production (EVER)
   - ✅ Keep HDF5 library updated for security patches
   - ✅ Use HTTPS for all plugin downloads
   - ✅ Verify checksums in addition to signatures (defense in depth)
   - ✅ Only download plugins from official sources (no random websites!)

3. **Version and Update Management**

   **Maintain Approved Plugin List:**

   ```bash
   # Create approved plugins manifest
   # /etc/hdf5/approved-plugins.txt
   # Format: plugin_name version sha256sum
   libh5zzfp.so 1.0.1 abc123def456...
   libh5blosc.so 2.3.0 xyz789ghi012...
   libh5bshuf.so 1.2.0 mno345pqr678...

   # Verification script
   #!/bin/bash
   PLUGIN_FILE=$1
   APPROVED_LIST="/etc/hdf5/approved-plugins.txt"

   PLUGIN_NAME=$(basename "$PLUGIN_FILE")
   PLUGIN_SHA256=$(sha256sum "$PLUGIN_FILE" | awk '{print $1}')

   if grep -q "$PLUGIN_NAME.*$PLUGIN_SHA256" "$APPROVED_LIST"; then
     echo "✓ Plugin approved"
     exit 0
   else
     echo "✗ Plugin not in approved list or wrong version"
     exit 1
   fi
   ```

   **Update Policy:**

   - Review plugin updates monthly
   - Test updates in staging before production
   - Subscribe to security advisories for plugins you use
   - Have rollback plan (keep previous version temporarily)

4. **Security Incident Response**

   **If Suspicious Plugin Detected:**

   ```bash
   # 1. Immediately remove from plugin directory
   sudo rm /usr/local/hdf5/lib/plugin/suspicious_plugin.so

   # 2. Check if keystore is compromised
   ls -la $HDF5_PLUGIN_KEYSTORE/

   # 3. Review recent plugin loads in logs
   grep "plugin loaded" /var/log/application.log

   # 4. Report to plugin developer and HDF Group
   # Email: security@hdfgroup.org
   ```

5. **Keystore Directory Permissions**

   Unix/Linux/macOS:

   ```bash
   # User keystore: only you can write
   chmod 700 ~/.hdf5/keystore

   # System keystore: only root can write, all can read
   sudo chmod 755 /etc/hdf5/keystore
   sudo chown root:root /etc/hdf5/keystore
   ```

   Windows:

   - Ensure keystore directory is not world-writable
   - Use NTFS permissions to restrict write access

6. **Environment Variable Security (Production Deployments)**

   In multi-tenant or HPC environments where untrusted users can control environment variables, you can lock the keystore location to prevent them from overriding `HDF5_PLUGIN_KEYSTORE` with a malicious keystore.

   **Runtime Lock (No Recompilation Required):**

   ```bash
   # Unix/Linux: Create lock file to disable environment variable override
   sudo mkdir -p /etc/hdf5
   sudo touch /etc/hdf5/lock_keystore

   # Windows: Create lock file
   mkdir "C:\ProgramData\HDF_Group\HDF5"
   type nul > "C:\ProgramData\HDF_Group\HDF5\lock_keystore"
   ```

   **Compile-Time Lock (Requires Rebuild):**

   ```bash
   # Configure HDF5 with locked keystore (completely disables env var)
   cmake -DHDF5_LOCK_PLUGIN_KEYSTORE=ON \
         -DHDF5_PLUGIN_KEYSTORE_DIR=/etc/hdf5/keystore \
         /path/to/hdf5/source
   ```

   **When to Use:**

   - ✅ HPC clusters with untrusted users
   - ✅ Multi-tenant systems
   - ✅ Production servers with strict security requirements
   - ✅ Pre-built binaries distributed to security-critical environments

   **How It Works:**

   1. Without a lock file: `HDF5_PLUGIN_KEYSTORE` env var is tried first; if it successfully loads keys, the compile-time `HDF5_PLUGIN_KEYSTORE_DIR` is **not** also tried (it is not a fallback)
   2. With a lock file: `HDF5_PLUGIN_KEYSTORE` env var is ignored entirely; only `HDF5_PLUGIN_KEYSTORE_DIR` is used
   3. Prevents privilege escalation via keystore override attacks
   4. System administrators can apply this to pre-built HDF5 libraries without recompilation

   **Verification:**

   ```bash
   # Test that environment variable is ignored after locking
   export HDF5_PLUGIN_KEYSTORE=/tmp/fake_keystore

   # Enable debug output to see which keystore is used
   export HDF5_DEBUG=pl
   h5dump test_file.h5

   # Expected output: "HDF5 KeyStore: Skipping HDF5_PLUGIN_KEYSTORE environment variable (locked by sysadmin)"
   ```

---

## Troubleshooting

### Common Issues and Solutions

#### 1. "keystore is empty - no keys available for verification"

**Cause**: No public keys found in keystore directory.

**Solution**:

```bash
# Check keystore directory
ls -la $HDF5_PLUGIN_KEYSTORE

# Ensure .pem files exist
# Copy public key if missing
cp developer_public.pem $HDF5_PLUGIN_KEYSTORE/
```

#### 2. "plugin signature verification failed"

**Possible causes**:

- Plugin was tampered with
- Wrong public key in keystore
- Plugin corrupted during download

**Solution**:

```bash
# 1. Re-download the plugin
wget https://developer.com/plugins/my_plugin.so

# 2. Verify checksum (if provided by developer)
sha256sum my_plugin.so
# Compare with published checksum

# 3. Ensure you have the correct public key
# Contact developer to verify key fingerprint
openssl rsa -pubin -in developer_public.pem -outform DER | sha256sum

# 4. Check if plugin was actually signed
hexdump -C my_plugin.so | tail -20
# Look for HDF5 magic number at the end (little-endian: 35 46 44 48)
```

#### 3. "SECURITY ERROR: keystore directory is world-writable"

**Cause**: Insecure permissions on keystore directory.

**Solution**:

```bash
# Fix permissions (Unix/Linux/macOS)
chmod 755 $HDF5_PLUGIN_KEYSTORE
# or for user-only access:
chmod 700 $HDF5_PLUGIN_KEYSTORE
```

#### 4. Plugin verification is slow

**Cause**: Very large plugin file.

**Solution**:

- Plugins are verified once per process (the plugin loader caches loaded plugins)
- Verification of large plugins may take a few seconds due to file I/O

  ```bash
  stat my_plugin.so
  ```

#### 5. "invalid signature magic number - not a signed HDF5 plugin"

**Cause**: Plugin is not signed, or signature was stripped.

**Solution**:

```bash
# Check if plugin has signature magic number at the end
hexdump -C my_plugin.so | tail -20
# Look for HDF5 magic number: 48 44 46 35

# If unsigned, sign it:
h5sign -p my_plugin.so -k private_key.pem
```

#### 6. Environment variable not set

**Symptoms**: Errors about missing keystore, even though directory exists.

**Solution**:

```bash
# Verify environment variable is set
echo $HDF5_PLUGIN_KEYSTORE

# If empty, set it:
export HDF5_PLUGIN_KEYSTORE=~/.hdf5/keystore

# Make permanent by adding to shell profile:
echo 'export HDF5_PLUGIN_KEYSTORE=~/.hdf5/keystore' >> ~/.bashrc
source ~/.bashrc
```

### Getting Detailed Diagnostics

For detailed diagnostics, check system logs or application error output. The HDF5 library provides detailed error messages when signature verification fails.

### Verification Test Suite

`h5signverifytest` is the HDF5 internal test harness for the plugin signature system. It is intended for HDF5 developers, not end users. It uses hardcoded test filenames generated by `h5signgentest` and will fail if run outside the HDF5 build tree.

To run the internal tests (HDF5 developers only):

```bash
# Build and run from the HDF5 build directory
cd hdf5/build
make h5signgentest h5signverifytest
cd tools/test/h5sign
./h5signgentest    # generates test plugins
./h5signverifytest # verifies them
```

---

## Technical Details

### Signature Format

Signed plugins have this structure:

```text
┌─────────────────────────────┐
│   Original Plugin Binary    │
│         (unchanged)         │
├─────────────────────────────┤
│   RSA Signature (256-1024B) │
├─────────────────────────────┤
│   Footer (12 bytes):        │
│   - Magic: HDF5 (4B)        │
│   - Signature length (4B)   │
│   - Algorithm ID (1B)       │
│   - Format version (1B)     │
│   - Reserved (2B)           │
└─────────────────────────────┘
```

### Supported Algorithms

| Algorithm | Hash | Padding | Signature Size | Security Level |
| --------- | ---- | ------- | -------------- | -------------- |
| SHA-256 | SHA-256 | PKCS#1 v1.5 | 256-1024 bytes | Good |
| SHA-384 | SHA-384 | PKCS#1 v1.5 | 256-1024 bytes | Better |
| SHA-512 | SHA-512 | PKCS#1 v1.5 | 256-1024 bytes | Best (default) |
| SHA-256-PSS | SHA-256 | PSS | 256-1024 bytes | Enhanced security |
| SHA-384-PSS | SHA-384 | PSS | 256-1024 bytes | Enhanced security |
| SHA-512-PSS | SHA-512 | PSS | 256-1024 bytes | Maximum security |

### Performance Impact

Verification time is dominated by I/O to read the plugin file for hashing,
plus a constant ~1-5ms for the RSA signature operation.  Plugins are already
cached by the HDF5 plugin loader, so each plugin is verified only once per
process.

### Crypto-Agility

The signature system supports multiple algorithms:

- Algorithm is stored in the signature footer
- Verifier reads the algorithm from the footer
- No recompilation needed to support new algorithms
- Future algorithms can be added without breaking compatibility

---

## FAQ

**Q: Do I need to sign plugins?**
A: Only if your HDF5 library was built with the CMake option `HDF5_REQUIRE_SIGNED_PLUGINS` enabled (which defines `H5_REQUIRE_DIGITAL_SIGNATURE` at compile time). Otherwise, signing is optional but recommended for security. Note that `HDF5_REQUIRE_SIGNED_PLUGINS=ON` also requires `-DHDF5_PLUGIN_KEYSTORE_DIR=<path>` to be specified at configure time, or the build will fail.

**Q: Can I use the same key for multiple plugins?**
A: Yes! You can use one key pair to sign all your plugins. Users only need your single public key.

**Q: What happens if verification fails?**
A: HDF5 will refuse to load the plugin and return an error with detailed diagnostics.

**Q: Can I remove the signature from a plugin?**
A: No, there's no official tool to remove signatures. You would need to rebuild the plugin from source.

**Q: How do I distribute updated plugins?**
A: Sign the new version with the same private key. Users don't need to update their keystore since they already have your public key.

**Q: What if my private key is compromised?**
A: Generate a new key pair immediately, sign all plugins with the new key, notify users to update their keystore, and revoke the old key.

**Q: Can I verify a plugin without installing it?**
A: Yes, use the verification test tool with a temporary keystore (see "Step 4: Verify Your Signature" in the developer section).

**Q: Does signing increase plugin size significantly?**
A: Minimal impact: 256–512 bytes for the signature (depends on RSA key size: 256 bytes for 2048-bit, 512 bytes for 4096-bit) + 12 bytes footer (typically <0.1% for most plugins).

**Q: Are signatures platform-specific?**
A: No! A signed Linux plugin remains signed if you copy it to Windows or macOS (though the plugin code itself may not be compatible across platforms).

**Q: Can I use hardware security modules (HSMs)?**
A: Yes, as long as the HSM can export keys in PEM format compatible with OpenSSL.

**Q: Do plugin signatures work in air-gapped (offline) environments?**
A: Yes. All cryptographic operations are performed locally using OpenSSL. No internet connectivity is required for signing or verification.

---

## Additional Resources

- **HDF5 Plugin Documentation**: <https://portal.hdfgroup.org/display/support/Registered+Filter+Plugins>
- **OpenSSL Documentation**: <https://www.openssl.org/docs/>
- **RSA Key Generation Best Practices**: <https://www.keylength.com/>
- **HDF5 Security**: Contact <security@hdfgroup.org> for security issues

---

## Support

For issues with plugin signatures:

1. Check the [Troubleshooting](#troubleshooting) section above
2. Review HDF5 logs with debug output enabled
3. Contact your plugin developer for plugin-specific issues
4. Report HDF5 library issues: <https://github.com/HDFGroup/hdf5/issues>

---

**Document Version**: 1.1
**Last Updated**: 2026-03-19
**HDF5 Version**: 2.2.0+
