# ROS3 VFD: 403 on Hive-style S3 Keys — Diagnosis, Workarounds, and Fix Plan

**Date:** 2026-06-10
**Stack:** HDF5 2.0.x ROS3 VFD (aws-c-s3 backend) · macOS client · `h5stat -S` over `s3://`
**Symptom:** `h5stat -S` (and any ROS3 open) returns HTTP **403** on objects whose keys contain Hive-style partition separators (`key=value`), e.g. `src=noaa-hrrr/fmt=conc4/…`, while `aws s3 ls`, `aws s3api get-object`, `s3fs`, etc. all work on the same object.

---

## TL;DR

- The HDF5 **2.0** ROS3 VFD's new **aws-c-s3** backend does **not** URI-encode the object key when building the request path. The `=` characters are sent **literal** instead of percent-encoded as `%3D`.
- AWS SigV4 (and S3's server-side recomputation) expect the canonical URI to be percent-encoded. A literal `=` produces a **canonical-request mismatch → `SignatureDoesNotMatch`**, which S3 returns as a **bodyless 403** (no `<Code>` on a HEAD).
- This is **not** an AWS-side permissions, KMS, or existence problem. It is a **client-side signing/encoding defect**, specific to the ROS3 aws-c-s3 path. `botocore`/CLI/`s3fs` work because they encode `=` → `%3D`.
- **Immediate options:** pre-encode `=`→`%3D` in the URI handed to ROS3, *or* rename keys to drop `=`, *or* read via `fsspec` + `h5py`.
- **Proper fix:** in the ROS3 s3comms layer, encode the key path once via the CRT's own `aws_byte_buf_append_encoding_uri_path()` (already in the link closure). Clean upstream PR.

---

## Environment Detail

- Object: `s3://prod-ingestify-data-130695430698-us-east-1-an/src=noaa-hrrr/fmt=conc4/AL00039__20240609__20260608__std.nc`
  - 64 MiB (67108864 bytes), `ServerSideEncryption: AES256` (**SSE-S3, not KMS**), multipart ETag (`…-2`), full-object CRC64NVME checksum.
  - File is healthy: `H5F_FSPACE_STRATEGY_PAGE`, 8 MiB page size (paged-aggregation writer working as intended).
- Credentials: AWS profile `sdi`, **long-lived `AKIA…` static keys** (no session token needed/involved).
- HDF5 2.0.0 (Nov 2025) replaced the ROS3 VFD's libcurl S3 backend with one based on `aws-c-s3`; the VFD now *requires* aws-c-s3 to build. (HDF Group 2.0 release notes.)

---

## Diagnostic Path (and why each step mattered)

These are the reusable reasoning hooks for next time.

1. **`aws s3 ls` ≠ read.** `ls s3://…/key` is a `ListObjectsV2` (prefix match), needing only `s3:ListBucket`. It never touches the object, so a clean `ls` says nothing about read access. The apples-to-apples comparison for a ROS3 open is `aws s3api head-object` / `get-object`.

2. **Read the CRT debug log; the request reached S3.** The 403 carried a real `x-amz-request-id` / `x-amz-id-2`, so it's an **auth/authz** decision, not connectivity. The log also showed:
   - Credentials resolved cleanly from profile `sdi` (default chain: web-identity → ECS → … → profile provider).
   - Signed `Credential=AKIA…/us-east-1/s3/aws4_request` — static key, region matches bucket, host correct (virtual-hosted), clock fine, TLS OK. **Signing succeeded; S3 rejected it.**

3. **The failing op was `HeadObject`, and a HEAD 403 is ambiguous.** A HEAD has no response body, so S3 collapses two cases into one 403: (a) caller lacks `s3:GetObject`, or (b) object missing **and** caller lacks `s3:ListBucket` (404 masked as 403). Cannot distinguish from the HEAD alone.

4. **Profile parity + existence check.** `aws s3 ls --profile sdi` on the exact key returned it (64 MiB) → same identity has `ListBucket`, object exists. Rules out existence, ListBucket, profile mismatch.

5. **`get-object --profile sdi` succeeded.** Proves `s3:GetObject` is granted (HEAD and GET share that action) and that encryption is **AES256/SSE-S3** (no `kms:Decrypt` in play). So it's **not** permissions and **not** KMS.

6. **`head-object --profile sdi` (botocore) succeeded.** Same op, same creds → the 403 is **not** the operation and **not** authorization. The only remaining variable is the **client/signer**: botocore vs the aws-c-s3 / aws-c-auth signer.

7. **The canonical request was the smoking gun.** The CRT log's canonical URI showed the key with **literal `=`**:
   ```
   HEAD
   /src=noaa-hrrr/fmt=conc4/AL00039__20240609__20260608__std.nc
   ```
   AWS `UriEncode` percent-encodes `=` (it's a sub-delimiter, not unreserved), so a bare `=` is the anomaly. botocore puts `%3D` in its canonical and S3 accepts it.

8. **Causation test — copy to a key without `=`.** `h5stat -S` on a copied plain key (`tmp/plain_*.nc`) **succeeded**. Single variable removed → `=` confirmed as the cause.

9. **Confirmation — pre-encoded URI.** Passing `…/src%3Dnoaa-hrrr/fmt%3Dconc4/…` to `h5stat` **works**. This shows the ROS3 path is a verbatim passthrough: hand it `%3D` and both the wire request and the canonical carry `%3D`, matching S3's server-side recomputation. (An initial "didn't work" was a mistest.)

---

## Root Cause

The HDF5 2.0 ROS3 VFD's aws-c-s3 backend builds the S3 request path from the object key **without URI-encoding it**. Sub-delimiter characters that AWS SigV4 requires to be percent-encoded — notably `=` in Hive-style `key=value` partitions — are left literal. The signed canonical request therefore disagrees with what S3 reconstructs server-side, yielding `SignatureDoesNotMatch`, surfaced as a **bodyless 403** (and indistinguishable from a permissions error on a HEAD).

Other clients (botocore/AWS CLI, s3fs/fsspec, aws-sdk-go) percent-encode the key, which is why everything **except** the ROS3 path works on these keys.

**Mechanism note:** with `use_double_uri_encode = false` (the correct S3 setting), the signer uses the request path verbatim, so the **caller** must pre-encode. botocore does; the ROS3 s3comms layer does not. S3 always recomputes its canonical with the key encoded (`=` → `%3D`), so only an encoded client canonical matches.

---

## Immediate Workarounds

### A. Pre-encode `=` → `%3D` in the URI (no rebuild)

Generate the URI programmatically so encoding is never hand-typed; route **all** ROS3 / h5py-ros3 / netCDF-4 opens through it:

```python
from urllib.parse import quote
uri = f"s3://{bucket}/{quote(key, safe='/')}"   # '=' -> '%3D', '/' preserved
```

**Caveats:** ROS3 is the *only* accessor that needs this (CLI/s3fs/boto take raw `=` fine), so hand-typed access is inconsistent — wrap it. And it **inverts once the VFD is fixed**: a patched VFD that encodes internally would turn a passed `%3D` into `%253D`. Treat this as a stopgap / diagnostic, not a permanent convention.

### B. Rename keys to drop `=` (server-side, parallel)

S3 "rename" = copy + delete; `s5cmd mv` does both server-side. No native regex rename, so generate the commands and feed `s5cmd run`:

```bash
export AWS_PROFILE=sdi
BUCKET=prod-ingestify-data-130695430698-us-east-1-an
PREFIX='src=noaa-hrrr/'

aws s3api list-objects-v2 --bucket "$BUCKET" --prefix "$PREFIX" \
      --query 'Contents[].Key' --output text | tr '\t' '\n' |
while IFS= read -r k; do
  [ -z "$k" ] && continue
  nk=$(printf '%s' "$k" | tr '=' '-')
  printf "mv 's3://%s/%s' 's3://%s/%s'\n" "$BUCKET" "$k" "$BUCKET" "$nk"
done > rename.txt

head rename.txt                  # eyeball mapping
s5cmd --dry-run run rename.txt    # confirm
s5cmd run rename.txt              # execute (parallel)
```

**Caveats:** `mv` needs `s3:PutObject` + `s3:DeleteObject` (use `cp` + separate delete if only put is granted); test one object first; guard against collisions (`=`→`-` merging two keys: `awk` the dest column → `sort | uniq -d` should be empty); SSE-S3 (AES256) and metadata are preserved by the default COPY directive. Also fix the **writer's key template** so new objects don't reintroduce `=`. **Downside:** loses literal Hive/Athena/Glue partition semantics.

### C. Read via fsspec + h5py

`s3fs`/`fsspec` (botocore signing) + `h5py` handles `=` correctly and sidesteps the CRT signer entirely. Good hedge if Athena/Glue partition discovery must stay on the `=` scheme.

---

## Proper Fix: Encode the Key in the ROS3 VFD (preferred)

**Goal:** make the VFD itself encode the key, reusing the CRT's own encoder — no hand-rolled logic, no new dependency.

### Where
- HDF5 2.0 source: `src/H5FDros3.c`, `src/H5FDs3comms.c`, `src/H5FDs3comms.h` (s3comms implements the AWS REST API for the aws-c-s3 backend).
- The defect is the single spot where the parsed object key (URI path component) is set as the request path. Grep `H5FDs3comms.c` for:
  - parse side: `aws_uri_init_parse`, `aws_uri_path`
  - set side: `aws_http_message_set_request_path` (or the message build feeding `aws_s3_meta_request` / `aws_s3_meta_request_options`)

### What to call
The encoder lives in **aws-c-common** at `aws/common/uri.h` (note: `aws/io/uri.h` is now just a forwarding shim that `#include`s it). Because ROS3 links `aws-c-s3`, `aws-c-common` is already transitively available — **nothing new to link**.

```c
/* key_cur = object-key path from the parsed URI (currently used raw) */
struct aws_byte_buf path_buf;
aws_byte_buf_init(&path_buf, alloc, key_cur.len * 3 + 2);
aws_byte_buf_append_byte_dynamic(&path_buf, (uint8_t)'/');        /* leading slash; host/bucket untouched */
aws_byte_buf_append_encoding_uri_path(&path_buf, &key_cur);       /* '/' preserved, '=' -> %3D, etc. */
/* set path_buf as the aws_http_message request path, then aws_byte_buf_clean_up(&path_buf) */
```

Related helpers in the same header: `aws_byte_buf_append_encoding_uri_param` (query values) and `aws_byte_buf_append_decoding_uri` (decode).

### Keep `use_double_uri_encode = false`
That's the correct S3 setting and almost certainly already in the signing config. Pre-encoding the path is the AWS-intended design — with it, **both** the wire request and the signed canonical carry `%3D`.

*Rejected alternative:* flipping `use_double_uri_encode = true` and leaving the path raw fixes the **signature** for `=` but still ships a raw path on the wire, which breaks for characters that are genuinely illegal in an HTTP request-target (space, `?`, `#`, control chars). Encoding once at construction is the complete fix; the flag flip is a half measure.

### Cautions to build into the patch / PR
1. **Determine whether `aws_uri_path` returns the raw (still-encoded) or decoded path** so the key is encoded **exactly once**. Post-fix contract should be "caller passes a raw key; the VFD encodes it" — matching every other S3 client.
2. **Double-encode transition:** once the VFD encodes internally, the `%3D` workaround (Option A) must be dropped or it becomes `%253D`. Sequence the rollout.
3. **Verify the encoder emits `%3D` for `=`** before relying on it — a 2-line unit test on `src=noaa-hrrr/fmt=conc4`. It *should* (since `=` isn't unreserved), but this exact character class is the seam where SigV4 vs RFC-3986 encoders have historically diverged — prove it.

---

## Diagnostic Command Reference

```bash
# Identity / account the profile actually resolves to
aws sts get-caller-identity --profile sdi
aws configure export-credentials --profile sdi   # is the key ASIA (temp) or AKIA (static)?

# List (ListBucket) vs read (GetObject) — different permissions, different code paths
aws s3 ls --profile sdi s3://BUCKET/PREFIX/
aws s3api head-object  --profile sdi --bucket BUCKET --key 'KEY'   # mirrors ROS3's first call
aws s3api get-object   --profile sdi --bucket BUCKET --key 'KEY' /tmp/out   # GET returns a body (real <Code>)

# Compare canonical requests: botocore should show %3D where the CRT log shows '='
aws s3api head-object --debug --profile sdi --bucket BUCKET --key 'KEY' 2>&1 | grep -A2 CanonicalRequest

# Causation isolation: copy to a key without '=' and retry ROS3
aws s3 cp --profile sdi s3://BUCKET/'src=noaa-hrrr/.../obj.nc' s3://BUCKET/tmp/plain_obj.nc
AWS_PROFILE=sdi h5stat -S s3://BUCKET/tmp/plain_obj.nc

# Pre-encoded URI confirmation (verbatim passthrough → works)
AWS_PROFILE=sdi h5stat -S 's3://BUCKET/src%3Dnoaa-hrrr/fmt%3Dconc4/obj.nc'
```

**HEAD-403 interpretation:** if `get-object` succeeds where the HEAD failed, suspect a client encoding/signing difference (not permissions); if `get-object`'s `AccessDenied` names `kms:Decrypt`, it's the KMS key grant; if it names `s3:GetObject` / is a bare deny, it's the S3 policy; `NoSuchKey` means the HEAD 403 was a masked 404.

---

## Key Facts / References

- **HDF5 2.0.0** (Nov 2025): ROS3 VFD S3 backend moved from libcurl to **aws-c-s3** (now a build requirement). Auto-sources AWS config/credentials per AWS spec; adds `--endpoint-url`.
  - Release notes: https://support.hdfgroup.org/documentation/hdf5/latest/rel_spec_20.html
  - Forum overview: https://forum.hdfgroup.org/t/hdf5-2-0-0-high-performance-hdf5-data-access-directly-from-s3/13529
- **h5py:** for HDF5 < 2.0 it rewrote `s3://` to path-style; for **2.0+** the library handles `http(s)://` and `s3://` natively. https://docs.h5py.org/en/stable/high/file.html
- **URI parser/encoder:** `struct aws_uri` / `aws_uri_init_parse` and `aws_byte_buf_append_encoding_uri_path` live in **aws-c-common** (`aws/common/uri.h`); `aws/io/uri.h` forwards to it. https://github.com/awslabs/aws-c-common
- **SigV4 canonical URI:** AWS `UriEncode` percent-encodes everything except unreserved `A-Za-z0-9-._~` (and `/` in paths); **S3 uses single encoding and does not normalize the path.** https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
- **S3-URI bucket/key splitters exist only at the SDK level** (Java `S3Uri`/`parseUri()`, PHP `S3UriParser`, .NET `AmazonS3Uri`) — **not** in the C CRT. At the CRT layer: parse with `aws_uri`, split bucket/key yourself (trivial for `s3://` and virtual-hosted; peel first path segment for path-style).
- **Precedent for this bug class** (path canonicalization divergence → `SignatureDoesNotMatch`): envoyproxy/envoy #16918, smarty-archives/go-aws-auth #28, peak/s5cmd #279 ("S3 key needs to be URL-encoded before download").
- **HeadObject 403 vs 404:** with `s3:ListBucket`, a missing object returns 404; without it, 403 (existence masked). With the object present, 403 means missing object-read authorization — or, as here, a signing mismatch.

---

## Open Questions / Future Exploration

- [ ] Confirm from `aws-c-common` source that `aws_byte_buf_append_encoding_uri_path` encodes `=` → `%3D` (and check `+`, space, `:`, `@`, `~`).
- [ ] Confirm whether `aws_uri_path` returns raw vs decoded → drives single-encode correctness in the patch.
- [ ] Check installed `aws-c-auth` / `aws-c-s3` versions against their changelogs for any URI-encoding fixes already landed.
- [ ] File HDFGroup/hdf5 issue + PR against the 2.0 ROS3/aws-c-s3 backend. Repro: CRT canonical shows literal `=`; botocore `--debug` canonical shows `%3D` on the identical key. Fix: insert `aws_byte_buf_append_encoding_uri_path` on the key in the s3comms request-path build.
- [ ] Strategic decision for the CONUS pipeline: **keep `=` + wrap-encode (and/or upstream fix)** vs **rename to drop `=`**. Keep `=` if Athena/Glue partition projection is on the roadmap; otherwise dropping `=` removes a whole class of special-character signing risk.
- [ ] Once a patched ROS3 build exists, retire Option A's `%3D` pre-encoding to avoid double-encoding.
