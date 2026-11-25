# Installing HDF5 from The HDF Group's Linux Package Repositories

The HDF Group publishes HDF5 packages for openSUSE, Fedora, RHEL-compatible
distributions (AlmaLinux, Rocky, RHEL), Debian and Ubuntu through the
[openSUSE Build Service](https://build.opensuse.org/project/show/isv:HDFGroup).
The packages follow each distribution's own HDF5 packaging, so they replace
the distribution's HDF5 with the current release. Programs built against the
distribution's HDF5 keep working, because the old library stays installed.

- No account or login is needed.
- Each repository holds only the latest HDF5 release. When a new release is
  published, the previous one is removed. If you need to stay on a specific
  version, pin it with your package manager or keep your own copy of the
  packages.
- The packages include serial and parallel (MPICH and Open MPI) builds, with
  C, C++, Fortran and the high-level libraries. Parallel builds include the
  subfiling VFD.

## Supported distributions

In the commands below, set `REPO` to the repository name for your
distribution:

| Distribution | `REPO` | Architectures |
| --- | --- | --- |
| openSUSE Tumbleweed | `openSUSE_Tumbleweed` | x86_64, aarch64 |
| openSUSE Slowroll | `openSUSE_Slowroll` | x86_64 |
| openSUSE Leap 16.1 | `16.1` | x86_64, aarch64 |
| openSUSE Leap 16.0 | `16.0` | x86_64, aarch64 |
| openSUSE Leap 15.7 | `15.7` | x86_64, aarch64 |
| openSUSE Leap 15.6 | `15.6` | x86_64, aarch64 |
| Fedora 44 | `Fedora_44` | x86_64, aarch64 |
| Fedora 43 | `Fedora_43` | x86_64, aarch64 |
| AlmaLinux, Rocky, RHEL 10 | `AlmaLinux_10` | x86_64, aarch64 |
| AlmaLinux, Rocky, RHEL 9 | `AlmaLinux_9` | x86_64, aarch64 |
| Debian 13 | `Debian_13` | x86_64 |
| Debian 12 | `Debian_12` | x86_64 |
| Ubuntu 26.04 | `xUbuntu_26.04` | x86_64 |
| Ubuntu 24.04 | `xUbuntu_24.04` | x86_64 |
| Ubuntu 22.04 | `xUbuntu_22.04` | x86_64 |

All repositories are listed at
<https://download.opensuse.org/repositories/isv:/HDFGroup/>.

## Signing key

Every repository is signed with the same key. Before you trust it, check
that its fingerprint matches the one below. This page is the reference copy
of the fingerprint.

| | |
| --- | --- |
| Name | `isv:HDFGroup OBS Project` |
| Type | RSA 4096 |
| Fingerprint | `EC85 B74C A713 0A35 1D88 97BA 00A3 9254 20A6 A686` |
| Expires | 2028-12-29 (the key will be extended before then; the fingerprint will not change) |

The key is published with each repository:

- RPM-based distributions: `https://download.opensuse.org/repositories/isv:/HDFGroup/$REPO/repodata/repomd.xml.key`
- Debian and Ubuntu: `https://download.opensuse.org/repositories/isv:/HDFGroup/$REPO/Release.key`

The setup steps below show how to check the fingerprint on each
distribution.

## openSUSE

Add the repository. `zypper refresh` shows the key's fingerprint and asks
whether to trust it:

```sh
REPO=openSUSE_Tumbleweed
sudo zypper addrepo -f https://download.opensuse.org/repositories/isv:/HDFGroup/$REPO/ hdfgroup
sudo zypper refresh
```

Switch the HDF5 packages you already have to the new release:

```sh
sudo zypper dup --from hdfgroup --allow-vendor-change
```

Or install HDF5 if you don't have it yet:

```sh
sudo zypper install --from hdfgroup hdf5-devel
```

## Fedora, AlmaLinux, Rocky and RHEL

On AlmaLinux, Rocky and RHEL, enable [EPEL](https://docs.fedoraproject.org/en-US/epel/)
first. The packages depend on libraries from EPEL.

Check the key's fingerprint, then add the repository:

```sh
REPO=Fedora_44
curl -fsSL https://download.opensuse.org/repositories/isv:/HDFGroup/$REPO/repodata/repomd.xml.key | gpg --show-keys
sudo curl -fsSL -o /etc/yum.repos.d/hdfgroup.repo \
  https://download.opensuse.org/repositories/isv:/HDFGroup/$REPO/isv:HDFGroup.repo
sudo rpm --import https://download.opensuse.org/repositories/isv:/HDFGroup/$REPO/repodata/repomd.xml.key
```

Upgrade the HDF5 packages you already have:

```sh
sudo dnf upgrade
```

Or install HDF5 if you don't have it yet:

```sh
sudo dnf install hdf5-devel
```

During the upgrade, `dnf` also installs `hdf5-compat`, which keeps the
distribution's original HDF5 library so programs built against it keep
working.

## Debian and Ubuntu

Check the key's fingerprint, then add the key and the repository:

```sh
REPO=Debian_13
curl -fsSL https://download.opensuse.org/repositories/isv:/HDFGroup/$REPO/Release.key | gpg --show-keys
sudo install -d -m 0755 /etc/apt/keyrings
curl -fsSL https://download.opensuse.org/repositories/isv:/HDFGroup/$REPO/Release.key \
  | gpg --dearmor | sudo tee /etc/apt/keyrings/hdfgroup.gpg > /dev/null
echo "deb [signed-by=/etc/apt/keyrings/hdfgroup.gpg] https://download.opensuse.org/repositories/isv:/HDFGroup/$REPO/ /" \
  | sudo tee /etc/apt/sources.list.d/hdfgroup.list
sudo apt update
```

Upgrade the HDF5 packages you already have:

```sh
sudo apt upgrade
```

Use `apt upgrade`, not `apt-get upgrade`. A new HDF5 release can come with
a new library package (for example `libhdf5-320`), and `apt-get upgrade`
never installs new packages, so it leaves HDF5 at the old version.

Or install HDF5 if you don't have it yet:

```sh
sudo apt install libhdf5-dev hdf5-tools
```

## Package names

| | openSUSE | Fedora, AlmaLinux, Rocky, RHEL | Debian, Ubuntu |
| --- | --- | --- | --- |
| Serial development files | `hdf5-devel` | `hdf5-devel` | `libhdf5-dev` |
| Command-line tools | `hdf5` | `hdf5` | `hdf5-tools` |
| MPICH | `hdf5-mpich-devel` | `hdf5-mpich-devel` | `libhdf5-mpich-dev` |
| Open MPI | `hdf5-openmpi4-devel`, `hdf5-openmpi5-devel` | `hdf5-openmpi-devel` | `libhdf5-openmpi-dev` |
| Java | | `java-hdf5` | `libhdf5-java` |

Open MPI 5 packages are not available on openSUSE Leap 15.6 and 15.7, which
don't ship Open MPI 5.

## How these packages differ from other HDF5 downloads

- **Source releases and the CMake-built binaries** on
  [GitHub](https://github.com/HDFGroup/hdf5/releases) and the
  [HDF Group website](https://www.hdfgroup.org/download-hdf5/) install HDF5
  on their own, outside the distribution's package system. They don't
  replace the distribution's HDF5 and don't receive updates through it.
- **These repositories** integrate with the distribution: they use its file
  layout and package names, update through `zypper`, `dnf` or `apt`, and
  work with the distribution's other packages that use HDF5, such as netCDF.

## Reporting problems

Report problems with these packages as
[HDF5 GitHub issues](https://github.com/HDFGroup/hdf5/issues). Include the
distribution, the `REPO` name and the package versions.
