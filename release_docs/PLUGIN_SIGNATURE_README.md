# HDF5 Plugin Digital Signature Guide

## Table of Contents
1. [Overview](#overview)
2. [Quick Start](#quick-start)
3. [For Plugin Developers](#for-plugin-developers)
4. [For Plugin Users](#for-plugin-users)
5. [Air-Gapped Environments](#air-gapped-environments)
6. [Security Considerations](#security-considerations)
7. [Troubleshooting](#troubleshooting)
8. [Technical Details](#technical-details)

---

## Overview

HDF5 plugin digital signatures provide cryptographic verification of plugin authenticity and integrity. This feature:

- **Prevents tampering**: Detects if a plugin binary has been modified
- **Ensures authenticity**: Verifies the plugin comes from a trusted source
- **Improves performance**: Caches verification results to avoid redundant checks
- **Maintains compatibility**: Optional feature that doesn't affect unsigned plugins (when not required)

### Key Features

✅ RSA-based digital signatures (2048-bit or higher recommended)
✅ Multiple hash algorithms (SHA-256, SHA-384, SHA-512)
✅ PSS padding support for enhanced security
✅ Multi-key keystore (accept plugins from multiple trusted developers)
✅ Signature caching with automatic invalidation
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
1. h5sign computes a SHA-256 hash of your plugin binary
2. Signs the hash with your private key using RSA
3. Appends the signature and metadata to the end of the plugin file

**Output**:
```
Signing plugin: my_filter_plugin.so
  Original size: 45,632 bytes
  Signature size: 512 bytes
  Algorithm: SHA-256 with RSA
  Total size: 46,160 bytes
Successfully signed: my_filter_plugin.so
```

### Step 4: Verify Your Signature (Optional but Recommended)

Test that the signature works before distributing:

```bash
# Set up a temporary keystore
mkdir -p /tmp/test_keystore
cp my_public_key.pem /tmp/test_keystore/

# Run the verification test
HDF5_PLUGIN_KEYSTORE=/tmp/test_keystore h5signverifytest
```

### Step 5: Distribute Your Plugin

Provide users with:

1. **The signed plugin**: `my_filter_plugin.so` (or .dll on Windows)
2. **The public key**: `my_public_key.pem`
3. **Installation instructions** (see user section below)

#### Example Distribution README

```markdown
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
```

### Advanced: Using Different Hash Algorithms

The `h5sign` tool supports multiple algorithms:

```bash
# SHA-256 (default, recommended for most cases)
h5sign -p plugin.so -k private.pem -a sha256

# SHA-384 (stronger, slightly larger signature)
h5sign -p plugin.so -k private.pem -a sha384

# SHA-512 (strongest, largest signature)
h5sign -p plugin.so -k private.pem -a sha512

# PSS padding (enhanced security)
h5sign -p plugin.so -k private.pem -a sha256-pss
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
# Find your HDF5 plugin directory
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

# For verbose verification output (if available)
HDF5_PLUGIN_DEBUG=1 h5dump -H file_using_plugin.h5
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

## Air-Gapped Environments

Air-gapped environments are systems isolated from external networks (including the internet) for security purposes. These are common in classified, high-security, or critical infrastructure environments.

### Overview

Plugin signatures work seamlessly in air-gapped environments since:
- ✅ No internet connectivity required for signing or verification
- ✅ All cryptographic operations are performed locally using OpenSSL
- ✅ Keys and plugins are transferred via approved physical media
- ✅ Signature verification cache works normally (no external dependencies)

### For Plugin Developers in Air-Gapped Environments

#### Step 1: Generate Keys on Secure Offline System

Generate your RSA key pair on a secure, air-gapped system dedicated to cryptographic operations:

```bash
# On secure offline workstation
openssl genrsa -out my_private_key.pem 4096
openssl rsa -in my_private_key.pem -pubout -out my_public_key.pem

# Secure the private key
chmod 600 my_private_key.pem

# CRITICAL: This private key should NEVER leave the air-gapped environment
```

**Best Practice**: Use a dedicated Hardware Security Module (HSM) or offline signing system that never connects to any network.

#### Step 2: Transfer Build Artifacts to Signing System

Transfer unsigned plugin binaries to the air-gapped signing system using approved methods:

**Approved Transfer Methods**:
1. **Write-once media**: CD-R, DVD-R (cannot be modified after burning)
2. **Inspected USB drives**: Sanitized, scanned for malware
3. **Dedicated data diodes**: One-way transfer devices
4. **Secure file transfer protocols**: When connecting isolated networks temporarily

**Security Checklist**:
```bash
# On development system (may be internet-connected)
# 1. Build plugin
gcc -shared -fPIC -o my_plugin.so my_plugin.c -lhdf5

# 2. Calculate checksum for integrity verification
sha256sum my_plugin.so > my_plugin.so.sha256

# 3. Transfer both files to air-gapped system via approved media
#    - my_plugin.so
#    - my_plugin.so.sha256
```

#### Step 3: Verify Integrity and Sign on Air-Gapped System

On the air-gapped signing system:

```bash
# Verify integrity after transfer
sha256sum -c my_plugin.so.sha256
# Should output: my_plugin.so: OK

# Sign the plugin
h5sign -p my_plugin.so -k my_private_key.pem -v

# Generate checksums of signed plugin
sha256sum my_plugin.so > my_plugin.so.signed.sha256
```

#### Step 4: Transfer Signed Plugin Back

Transfer the signed plugin and public key back to distribution system:

**Files to transfer OUT of air-gapped environment**:
- `my_plugin.so` (now signed)
- `my_plugin.so.signed.sha256` (checksum)
- `my_public_key.pem` (for distribution to users)

**Security Note**: The private key (`my_private_key.pem`) must NEVER leave the air-gapped signing system.

#### Step 5: Distribute to Air-Gapped User Environments

Package for distribution to air-gapped user sites:

```bash
# Create distribution package
mkdir my_plugin_v1.0
cp my_plugin.so my_plugin_v1.0/
cp my_public_key.pem my_plugin_v1.0/
cp my_plugin.so.signed.sha256 my_plugin_v1.0/

# Create installation instructions
cat > my_plugin_v1.0/INSTALL.txt << 'EOF'
HDF5 Plugin Installation (Air-Gapped)

1. Verify integrity:
   sha256sum -c my_plugin.so.signed.sha256

2. Install public key:
   mkdir -p /etc/hdf5/keystore
   cp my_public_key.pem /etc/hdf5/keystore/

3. Install plugin:
   cp my_plugin.so /usr/local/hdf5/lib/plugin/

4. Set keystore (if not system-wide):
   export HDF5_PLUGIN_KEYSTORE=/etc/hdf5/keystore
EOF

# Create distribution archive
tar -czf my_plugin_v1.0_airgapped.tar.gz my_plugin_v1.0/

# Burn to write-once media or transfer via approved method
```

### For Plugin Users in Air-Gapped Environments

#### Step 1: Receive and Verify Distribution Package

```bash
# Receive plugin package via approved transfer method
# (CD-R, inspected USB, data diode, etc.)

# Extract package
tar -xzf my_plugin_v1.0_airgapped.tar.gz
cd my_plugin_v1.0/

# Verify integrity using provided checksum
sha256sum -c my_plugin.so.signed.sha256
# Should output: my_plugin.so: OK

# If checksum fails, DO NOT INSTALL - request new copy
```

#### Step 2: Verify Public Key Authenticity

Since you can't download the key from a website, use out-of-band verification:

**Recommended Methods**:

1. **Published Fingerprint**: Compare key fingerprint with value published in official documentation received through approved channels
   ```bash
   openssl rsa -pubin -in my_public_key.pem -outform DER | sha256sum
   # Compare output with published fingerprint
   ```

2. **Physical Verification**: Verify fingerprint directly with developer/organization contact via phone, secure video call, or in-person meeting

3. **Chain of Trust**: If distributed by trusted internal security team, verify their approval/signature on the distribution package

4. **Internal Certificate Authority**: If your organization has an internal CA, verify the public key is signed by your CA

#### Step 3: Set Up Keystore

```bash
# Create keystore directory (system-wide recommended for air-gapped systems)
sudo mkdir -p /etc/hdf5/keystore
sudo chmod 755 /etc/hdf5/keystore

# Install public key
sudo cp my_public_key.pem /etc/hdf5/keystore/

# Set restrictive permissions
sudo chmod 644 /etc/hdf5/keystore/my_public_key.pem

# Make keystore permanent (add to system profile)
echo 'export HDF5_PLUGIN_KEYSTORE=/etc/hdf5/keystore' | sudo tee /etc/profile.d/hdf5.sh
```

#### Step 4: Install Signed Plugin

```bash
# Find HDF5 plugin directory
h5cc -showconfig | grep "Default plugin path"

# Install plugin
sudo cp my_plugin.so /usr/local/hdf5/lib/plugin/
sudo chmod 755 /usr/local/hdf5/lib/plugin/my_plugin.so
```

#### Step 5: Verify Installation

```bash
# Set keystore if not system-wide
export HDF5_PLUGIN_KEYSTORE=/etc/hdf5/keystore

# Test plugin verification
# (Create a test HDF5 file that uses the plugin, or use existing test file)
h5dump -H test_file_using_plugin.h5

# If verification succeeds, you'll see no errors
# If verification fails, check logs for detailed error messages
```

### Air-Gap Specific Security Considerations

#### Key Management in Air-Gapped Environments

1. **Dedicated Signing System**
   - Use a dedicated, air-gapped workstation for all signing operations
   - Never connect this system to any network
   - Physical security: locked room, access controls, audit logs
   - Minimal software: Only OpenSSL, h5sign, and essential OS components

2. **Private Key Storage**
   - Store on encrypted volume with passphrase
   - Consider HSM for critical keys (e.g., FIPS 140-2 Level 3+)
   - Backup keys to encrypted offline media stored in physically separate location
   - Implement key split custody (key parts held by different people)

3. **Transfer Media Security**
   - Use write-once media (CD-R, DVD-R) when possible
   - Scan all incoming media for malware before use
   - Sanitize all outgoing media
   - Maintain log of all transfers (what, when, who, why)

4. **Public Key Distribution**
   - Include key fingerprints in printed, signed documentation
   - Publish fingerprints through multiple independent channels
   - Consider using your organization's internal PKI
   - Maintain key fingerprint verification registry

#### Operational Procedures

**Plugin Update Workflow**:
```
[Development Network]
    → Build plugin
    → Generate checksum
    → Transfer to air-gapped signer (CD-R)

[Air-Gapped Signing System]
    → Verify checksum
    → Sign plugin with h5sign
    → Generate new checksum
    → Transfer signed plugin out (CD-R)

[Distribution Network]
    → Package with public key and checksums
    → Create distribution archive
    → Transfer to air-gapped sites (approved method)

[Air-Gapped User Site]
    → Receive package
    → Verify checksums
    → Verify public key fingerprint
    → Install to keystore and plugin directory
    → Test verification
```

**Emergency Key Rotation** (e.g., suspected compromise):
```bash
# On air-gapped signing system:
# 1. Generate new key pair
openssl genrsa -out my_new_private_key.pem 4096
openssl rsa -in my_new_private_key.pem -pubout -out my_new_public_key.pem

# 2. Re-sign all plugins with new key
for plugin in *.so; do
    h5sign -p "$plugin" -k my_new_private_key.pem
done

# 3. Distribute new public key and re-signed plugins to all sites
# 4. Document old key as revoked

# On user sites:
# 1. Remove old public key from keystore
sudo rm /etc/hdf5/keystore/my_old_public_key.pem

# 2. Install new public key (verify fingerprint!)
sudo cp my_new_public_key.pem /etc/hdf5/keystore/

# 3. Update plugins with newly signed versions
```

#### Audit and Compliance

For regulated air-gapped environments:

1. **Signing Audit Trail**
   ```bash
   # On signing system, maintain signature log
   echo "$(date -Iseconds) | $USER | $PLUGIN | $(sha256sum $PLUGIN)" >> /var/log/h5sign/signatures.log
   ```

2. **Installation Verification**
   ```bash
   # On user systems, log successful verifications
   # (HDF5 library can log verification events to syslog if enabled)
   ```

3. **Periodic Key Audits**
   - Quarterly review of keystores across all air-gapped sites
   - Verify only approved keys are present
   - Check for unauthorized modifications

4. **Compliance Documentation**
   - Maintain records of all key generation events
   - Document chain of custody for signing keys
   - Keep transfer logs for all plugin distributions

### Troubleshooting Air-Gapped Installations

**Issue**: Cannot verify key fingerprint (no internet access)

**Solution**:
- Request fingerprint via official communication channel (classified email, courier, etc.)
- Cross-reference with printed documentation received with physical media
- Contact security office for verification through approved process

**Issue**: Transfer media fails integrity check

**Solution**:
```bash
# DO NOT INSTALL if integrity check fails
# Request new distribution via approved channels
# Report incident to security team
```

**Issue**: Need to update OpenSSL but air-gapped

**Solution**:
```bash
# Download OpenSSL on internet-connected system
# Transfer via approved method
# Verify package signatures before installation
# Install offline:
sudo dpkg -i openssl_*.deb  # Debian/Ubuntu
# or
sudo rpm -ivh openssl-*.rpm  # RHEL/CentOS
```

### Air-Gap Checklist

**Developer Checklist**:
- [ ] Generate keys on dedicated air-gapped signing system
- [ ] Never connect signing system to any network
- [ ] Verify checksums before and after signing
- [ ] Use write-once media for transfers when possible
- [ ] Maintain signing audit log
- [ ] Backup keys to encrypted offline media
- [ ] Document and publish key fingerprints through approved channels

**User Checklist**:
- [ ] Verify package integrity using provided checksums
- [ ] Verify public key fingerprint through out-of-band method
- [ ] Use system-wide keystore with proper permissions
- [ ] Document installed keys and plugins
- [ ] Test plugin verification after installation
- [ ] Maintain audit trail of installations
- [ ] Have key revocation procedure in place

---

## Security Considerations

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
|---------------|-----------|------|
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
|---------|-------------------------|-------------|
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
4. **Air-gapped**: Follow strict transfer procedures, verify integrity at each step, use write-once media

**Security is a shared responsibility** between plugin developers, distributors, and users.

---

### For Plugin Developers

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
   ```bash
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
   ```
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

### For Plugin Users

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

   # 3. Verify signature (HDF5 does this automatically, but can test manually)
   h5sign -v my_plugin.so -k $HDF5_PLUGIN_KEYSTORE/developer_public.pem

   # 4. Install only if BOTH checks pass
   sudo cp my_plugin.so /usr/local/hdf5/lib/plugin/
   ```

   **Monitoring and Logging:**
   ```bash
   # Enable verification logging (if needed)
   export HDF5_PLUGIN_DEBUG=1

   # Monitor for verification failures
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

3. **Keystore Directory Permissions**

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

#### 2. "plugin signature verification failed - signature cryptographically invalid with ALL keys"

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
# Look for H5PLSIG magic number: 53 49 47 35 4C 50 48
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

**Cause**: Signature cache not working, or very large plugin.

**Solution**:
- Verification is cached after first load (instant on subsequent loads)
- First verification of large plugins may take a few seconds
- Check file modification time isn't changing unexpectedly:
  ```bash
  stat my_plugin.so
  ```

#### 5. "invalid signature magic number - not a signed HDF5 plugin"

**Cause**: Plugin is not signed, or signature was stripped.

**Solution**:
```bash
# Check if plugin was signed
h5sign --verify my_plugin.so

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

Enable debug output for troubleshooting:

```bash
# Enable HDF5 plugin debugging
export HDF5_PLUGIN_DEBUG=1

# Run your application
h5dump file.h5
```

### Verification Test Suite

Run the comprehensive test suite to verify your setup:

```bash
# Build test suite
cd hdf5/build
make h5signverifytest

# Run tests
cd tools/test/h5sign
./h5signverifytest
```

---

## Technical Details

### Signature Format

Signed plugins have this structure:

```
┌─────────────────────────────┐
│   Original Plugin Binary    │
│         (unchanged)         │
├─────────────────────────────┤
│   RSA Signature (256-512B)  │
├─────────────────────────────┤
│   Footer (12 bytes):        │
│   - Signature length (4B)   │
│   - Algorithm ID (1B)       │
│   - Format version (1B)     │
│   - Reserved (2B)           │
│   - Magic: H5PLSIG (4B)     │
└─────────────────────────────┘
```

### Supported Algorithms

| Algorithm | Hash | Padding | Signature Size | Security Level |
|-----------|------|---------|----------------|----------------|
| SHA-256   | SHA-256 | PKCS#1 v1.5 | 256-512 bytes | Good (recommended) |
| SHA-384   | SHA-384 | PKCS#1 v1.5 | 256-512 bytes | Better |
| SHA-512   | SHA-512 | PKCS#1 v1.5 | 256-512 bytes | Best |
| SHA-256-PSS | SHA-256 | PSS | 256-512 bytes | Enhanced security |
| SHA-384-PSS | SHA-384 | PSS | 256-512 bytes | Enhanced security |
| SHA-512-PSS | SHA-512 | PSS | 256-512 bytes | Maximum security |

### Signature Cache

HDF5 caches verification results for performance:

- **Cache key**: Plugin path + file modification time
- **Cache invalidation**: Automatic when plugin file is modified
- **Cache scope**: Per-process (not persisted across runs)
- **Cache benefits**:
  - First load: ~100ms verification time
  - Cached load: <1ms (instant)

Cache behavior:
```
Load #1 → Full verification (slow) → Cache result
Load #2 → Cache hit (instant) → Skip verification
Plugin modified → Cache invalidated
Load #3 → Full verification (slow) → Update cache
```

### Performance Impact

| Operation | Time (First Load) | Time (Cached) |
|-----------|------------------|---------------|
| 1 MB plugin | ~50ms | <1ms |
| 10 MB plugin | ~200ms | <1ms |
| 100 MB plugin | ~1500ms | <1ms |

*Note: Times are approximate and depend on hardware.*

### Crypto-Agility

The signature system supports multiple algorithms:
- Algorithm is stored in the signature footer
- Verifier reads the algorithm from the footer
- No recompilation needed to support new algorithms
- Future algorithms can be added without breaking compatibility

---

## FAQ

**Q: Do I need to sign plugins?**
A: Only if your HDF5 library was built with `H5_REQUIRE_DIGITAL_SIGNATURE` enabled. Otherwise, signing is optional but recommended for security.

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
A: Minimal impact: ~512 bytes for the signature + 12 bytes footer (typically <0.1% for most plugins).

**Q: Are signatures platform-specific?**
A: No! A signed Linux plugin remains signed if you copy it to Windows or macOS (though the plugin code itself may not be compatible across platforms).

**Q: Can I use hardware security modules (HSMs)?**
A: Yes, as long as the HSM can export keys in PEM format compatible with OpenSSL.

**Q: Do plugin signatures work in air-gapped environments?**
A: Yes! Plugin signatures are designed to work seamlessly in air-gapped environments. All cryptographic operations are performed locally using OpenSSL. No internet connectivity is required for signing or verification. See the [Air-Gapped Environments](#air-gapped-environments) section for detailed procedures.

**Q: How do I transfer signed plugins to air-gapped systems?**
A: Use approved transfer methods for your environment: write-once media (CD-R, DVD-R), inspected USB drives, data diodes, or secure file transfer protocols. Always verify integrity using checksums before and after transfer. See air-gapped section for complete workflow.

**Q: How do I verify public key authenticity in air-gapped environments?**
A: Use out-of-band verification: compare key fingerprints with values published in official documentation, verify via phone/secure call with developer, or use your organization's internal PKI/CA. Never trust keys without independent verification.

**Q: Can I sign plugins offline?**
A: Yes! In fact, signing offline on a dedicated air-gapped system is the most secure approach. Generate keys on the offline system, transfer unsigned plugins in, sign them, and transfer signed plugins out. The private key never leaves the air-gapped signing system.

**Q: What if I need to rotate keys in an air-gapped environment?**
A: Generate new keys on your air-gapped signing system, re-sign all plugins with the new key, and distribute the new public key and re-signed plugins to all sites via approved channels. Remove old keys from keystores and document the old key as revoked. See air-gapped section for emergency rotation procedure.

---

## Additional Resources

- **HDF5 Plugin Documentation**: https://portal.hdfgroup.org/display/support/Registered+Filter+Plugins
- **OpenSSL Documentation**: https://www.openssl.org/docs/
- **RSA Key Generation Best Practices**: https://www.keylength.com/
- **HDF5 Security**: Contact security@hdfgroup.org for security issues

---

## Support

For issues with plugin signatures:

1. Check the [Troubleshooting](#troubleshooting) section above
2. Review HDF5 logs with debug output enabled
3. Contact your plugin developer for plugin-specific issues
4. Report HDF5 library issues: https://github.com/HDFGroup/hdf5/issues

---

**Document Version**: 1.0
**Last Updated**: 2026-01-28
**HDF5 Version**: 1.15.0+
