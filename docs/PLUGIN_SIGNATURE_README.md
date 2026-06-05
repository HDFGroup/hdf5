# HDF5 Plugin Digital Signature Guide

## Table of Contents

1. [Overview](#overview)
2. [Quick Start](#quick-start)
3. [For Plugin Users](#for-plugin-users)
4. [For Plugin Developers](#for-plugin-developers)
5. [Security Considerations](#security-considerations)
6. [Troubleshooting](#troubleshooting)
7. [Technical Details](#technical-details)

---

## Overview

HDF5 plugin digital signatures provide cryptographic verification of plugin authenticity and integrity. When enabled, HDF5 verifies that each plugin was signed by a trusted developer before loading it.

### Key Features

- RSA-based digital signatures (4096-bit recommended, 2048-bit minimum)
- Multiple hash algorithms (SHA-256, SHA-384, SHA-512) with PSS padding support
- Multi-key keystore for accepting plugins from multiple trusted developers
- Plugins verified once per load (already cached by plugin loader)

---

## Quick Start

### For Plugin Users

1. Obtain the public key from your plugin developer
2. Create a keystore directory and place the public key in it
3. Set the `HDF5_PLUGIN_KEYSTORE` environment variable to the keystore directory path
4. Use HDF5 normally — signed plugins are verified automatically

### For Plugin Developers

1. Generate an RSA key pair (see OpenSSL documentation)
2. Build your plugin as usual
3. Sign your plugin with `h5sign -p my_plugin.so -k my_private_key.pem`
4. Distribute the signed plugin and public key to users

---

## For Plugin Users

### Setting Up the Keystore

1. Obtain the public key from your plugin developer through a trusted channel
2. Create a directory to serve as your keystore
3. Place the public key `.pem` file(s) in the keystore directory
4. Set the `HDF5_PLUGIN_KEYSTORE` environment variable to the keystore path

The keystore can contain public keys from multiple developers. HDF5 will try
all keys and accept the plugin if any key verifies successfully.

### Compile-Time Keystore

Alternatively, the keystore path can be set at compile time:

```bash
cmake -DHDF5_REQUIRE_SIGNED_PLUGINS=ON \
      -DHDF5_PLUGIN_KEYSTORE_DIR=/path/to/keystore \
      /path/to/hdf5/source
```

If `HDF5_PLUGIN_KEYSTORE_DIR` is set at compile time, it is used as the
default. The `HDF5_PLUGIN_KEYSTORE` environment variable takes precedence
at runtime unless the keystore is locked (see below).

### Locking the Keystore

To prevent runtime override of the keystore path via environment variable,
build with `-DHDF5_LOCK_PLUGIN_KEYSTORE=ON`. When locked, only the
compile-time `HDF5_PLUGIN_KEYSTORE_DIR` is used.

---

## For Plugin Developers

### Key Generation

Generate an RSA key pair using OpenSSL (4096-bit recommended). Refer to the
[OpenSSL documentation](https://www.openssl.org/docs/) for key generation
commands and best practices.

**Key security**: Store your private key securely and never share it.

### Signing Plugins

Use the `h5sign` tool to add a digital signature:

```bash
# Basic signing (defaults to SHA-512)
h5sign -p my_plugin.so -k my_private_key.pem

# Choose a specific algorithm
h5sign -p my_plugin.so -k my_private_key.pem -a sha256

# PSS padding variant
h5sign -p my_plugin.so -k my_private_key.pem -a sha512-pss

# Verbose output
h5sign -p my_plugin.so -k my_private_key.pem -v
```

The tool appends the RSA signature and a 14-byte footer to the end of the
plugin file. The binary loader ignores trailing data, so the signed plugin
loads normally on all platforms.

### Re-signing a Plugin

To update the signature (e.g., after rebuilding or to change the algorithm):

```bash
h5sign -p my_plugin.so -k my_private_key.pem -f
```

The `-f` / `--force` flag strips the existing signature before re-signing.

### Distributing Plugins

Provide users with:

1. The signed plugin binary
2. Your public key (`.pem` file)
3. Instructions to add the public key to their keystore

### Passphrase-Protected Keys

h5sign supports passphrase-protected private keys. OpenSSL will prompt for the
passphrase interactively.

---

## Security Considerations

### Key Management

Public keys in the keystore are not secret, but their integrity must be
protected — anyone who can add a key to the keystore can make HDF5 trust
their plugins.

Plugin developers are responsible for keeping their private keys secure.

### Security Model

HDF5 plugin signatures protect against:

- **Unsigned malicious plugins**: Blocked (signature required)
- **Tampered plugins**: Detected (signature invalidates)
- **Untrusted sources**: Rejected (keystore verification)

### Signature Revocation

Individual signatures can be revoked without removing the entire public key.
Place a file named `revoked_signatures.txt` in the keystore directory. Each
line is the 64-character hex-encoded SHA-256 hash of the raw signature bytes
to revoke. Lines starting with `#` are comments; empty lines are ignored.

Note: the revocation entry is **not** the raw signature itself hex-encoded.
It is the SHA-256 digest of the raw signature bytes, represented as 64 hex
characters (32 bytes). This provides a fixed-length identifier regardless of
RSA key size. To compute the hash for a signed plugin:

```bash
# Extract the raw signature, then hash it
h5sign -p my_plugin.so -v   # displays signature details
# Use OpenSSL to compute SHA-256 of the raw signature bytes
```

```text
# Example revoked_signatures.txt
# SHA-256 hash of a compromised plugin's signature
a1b2c3d4e5f6...  (64 hex characters)
```

The revocation file is optional. If absent, no signatures are revoked.

### Known Limitations

- **No rollback protection**: Signatures prove authenticity, not freshness
- **No expiration**: Signed plugins remain valid indefinitely
- **Manual trust management**: Users must manage keystore contents

### Air-Gapped Environments

All cryptographic operations are performed locally using OpenSSL. No
internet connectivity is required for signing or verification.

---

## Troubleshooting

### Common Error Messages

| Error | Cause | Solution |
| ----- | ----- | -------- |
| "keystore is empty" | No public keys in keystore directory | Add the developer's public key to the keystore |
| "plugin signature verification failed" | Wrong key, tampered plugin, or corrupted download | Verify you have the correct public key; re-download the plugin |
| "plugin signature has been revoked" | Signature listed in `revoked_signatures.txt` | Remove the hash from the revocation file, or re-sign the plugin |
| "invalid signature magic number" | Plugin is not signed | Sign the plugin with `h5sign` |
| Keystore not found | `HDF5_PLUGIN_KEYSTORE` not set or path does not exist | Set the environment variable to a valid keystore directory |

### Verification Test Suite

`h5signverifytest` is the HDF5 internal test harness for the plugin signature
system. It is intended for HDF5 developers only and requires pre-generated test
data from the HDF5 build tree.

---

## Technical Details

### Signature Format

Signed plugins have this structure:

```text
+-----------------------------+
|   Original Plugin Binary    |
|         (unchanged)         |
+-----------------------------+
|   RSA Signature (256-1024B) |
+-----------------------------+
|   Footer (14 bytes):        |
|   - Algorithm ID (1B)       |
|   - Signature length (4B)   |
|   - Magic (8B, non-ASCII)   |
|   - Format version (1B)     |
+-----------------------------+
```

### Supported Algorithms

| Algorithm | Padding | Security Level |
| --------- | ------- | -------------- |
| SHA-256 | PKCS#1 v1.5 | Good |
| SHA-384 | PKCS#1 v1.5 | Better |
| SHA-512 (default) | PKCS#1 v1.5 | Best |
| SHA-256-PSS | PSS | Enhanced |
| SHA-384-PSS | PSS | Enhanced |
| SHA-512-PSS | PSS | Maximum |

### Performance

Verification time is dominated by I/O to read the plugin file for hashing,
plus ~1-5ms for the RSA operation. Plugins are cached by the HDF5 plugin
loader, so each plugin is verified only once per process.

---

## FAQ

**Q: Do I need to sign plugins?**
A: Only if HDF5 was built with `HDF5_REQUIRE_SIGNED_PLUGINS=ON`. Otherwise,
signing is optional but recommended.

**Q: Can I use the same key for multiple plugins?**
A: Yes. Users only need your single public key.

**Q: What happens if verification fails?**
A: HDF5 refuses to load the plugin and returns an error.

**Q: Does signing increase plugin size?**
A: Minimally — 256-512 bytes for the signature plus 14 bytes for the footer.

**Q: Are signatures platform-specific?**
A: No. A signed plugin retains its signature across platforms (though the
plugin binary itself may be platform-specific).

**Q: Do signatures work in air-gapped environments?**
A: Yes. All operations are local; no internet required.

---

## Additional Resources

- **OpenSSL Documentation**: <https://www.openssl.org/docs/>
- **HDF5 Plugin Documentation**: <https://portal.hdfgroup.org/display/support/Registered+Filter+Plugins>

---

**Document Version**: 1.2
**Last Updated**: 2026-03-20
**HDF5 Version**: 2.2.0+
