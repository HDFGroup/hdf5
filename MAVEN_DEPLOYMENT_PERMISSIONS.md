# Maven Deployment Permissions Setup

This document outlines the permissions and secrets required for Maven artifact deployment to work properly.

## Current Issues

The Maven deployment workflow is failing with `403 Forbidden` errors, indicating missing or incorrect permissions.

## Required Permissions

### GitHub Packages Deployment

For deployment to `https://maven.pkg.github.com/HDFGroup/hdf5`:

#### Repository Settings
1. **Package Permissions**: The repository must have GitHub Packages enabled
2. **Workflow Permissions**: Must allow workflows to write to packages

#### Secrets Required
- `GITHUB_TOKEN`: Should have `packages:write` permission (auto-provided in workflows)
- Repository must be configured to allow package publishing

#### User/Organization Permissions
- The user/organization must have permissions to publish packages to the `HDFGroup/hdf5` repository
- For forks, packages cannot be published to the upstream repository

### Maven Central Deployment

For deployment to Maven Central via `https://s01.oss.sonatype.org/`:

#### Secrets Required
- `MAVEN_CENTRAL_USERNAME`: Sonatype OSSRH username
- `MAVEN_CENTRAL_PASSWORD`: Sonatype OSSRH password/token
- `GPG_PRIVATE_KEY`: GPG private key for signing artifacts (required for Maven Central)
- `GPG_PASSPHRASE`: Passphrase for the GPG private key

## Setup Steps

### For GitHub Packages

1. **Enable GitHub Packages** in repository settings
2. **Configure Workflow Permissions**:
   - Go to Settings → Actions → General
   - Under "Workflow permissions", select "Read and write permissions"
   - Check "Allow GitHub Actions to create and approve pull requests"

3. **Add Repository Secrets**:
   ```
   # These are automatically available:
   GITHUB_TOKEN (auto-provided)
   ```

### For Maven Central

1. **Create Sonatype OSSRH Account**:
   - Sign up at https://issues.sonatype.org/
   - Create a ticket to request publishing rights for `org.hdfgroup`

2. **Generate GPG Key**:
   ```bash
   gpg --gen-key
   gpg --list-secret-keys --keyid-format LONG
   gpg --armor --export-secret-keys KEY_ID
   ```

3. **Add Repository Secrets**:
   ```
   MAVEN_CENTRAL_USERNAME: your-sonatype-username
   MAVEN_CENTRAL_PASSWORD: your-sonatype-password
   GPG_PRIVATE_KEY: -----BEGIN PGP PRIVATE KEY BLOCK-----...
   GPG_PASSPHRASE: your-gpg-passphrase
   ```

## Testing Deployment

The workflows are currently configured with `dry_run: true` to test permissions without actual deployment:

1. **Run Release Workflow** with `deploy_maven: true`
2. **Check Logs** for permission/authentication issues
3. **Fix Issues** based on debug output
4. **Set `dry_run: false`** once permissions are working

## Troubleshooting

### 403 Forbidden Errors

1. **Check repository permissions**: Ensure the repository allows package publishing
2. **Verify token permissions**: Ensure `GITHUB_TOKEN` has `packages:write`
3. **Check organization settings**: Some organizations restrict package publishing
4. **Verify repository ownership**: Forks cannot publish to upstream repositories

### Authentication Issues

1. **Test credentials manually**:
   ```bash
   curl -u "username:token" -I https://maven.pkg.github.com/HDFGroup/hdf5
   ```

2. **Check secret values**: Ensure secrets are properly set and not empty

### GPG Signing Issues

1. **Verify GPG key format**: Must be armored private key
2. **Test GPG import locally**:
   ```bash
   echo "$GPG_PRIVATE_KEY" | gpg --batch --import
   ```

## Next Steps

1. **Review Current Permissions**: Check repository and organization settings
2. **Test with Dry Run**: Run workflows with debug output enabled
3. **Fix Permission Issues**: Based on debug output and error messages
4. **Enable Live Deployment**: Set `dry_run: false` after successful testing

## Recent Updates (September 22, 2025)

### Artifact Naming Fix
- **Issue**: Deploy workflow was only finding Linux artifacts due to naming mismatch
- **Fix**: Updated artifact download names in `maven-deploy.yml` to match staging workflow output
- **Impact**: All platform artifacts (Linux, Windows, macOS x86_64, macOS aarch64) now properly downloaded

### Workflow Integration
- **Added**: Multi-platform artifact generation in release workflow
- **Enhanced**: Debug output and error handling for troubleshooting permission issues
- **Implemented**: Dry run mode for safe permission testing

## Files Modified

- `.github/workflows/maven-deploy.yml`: Fixed artifact naming, added debug output and error handling
- `.github/workflows/release.yml`: Enabled dry run mode for testing
- `.github/workflows/maven-staging.yml`: Made reusable for release workflow integration
- `MAVEN_DEPLOYMENT_FIXES_SUMMARY_2025-09-22.md`: Complete technical summary of all fixes