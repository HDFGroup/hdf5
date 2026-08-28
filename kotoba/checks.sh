#!/usr/bin/env bash
# Compile hdf5.kotoba with Kotoba 0.7.2 (wasm32, i64-v1) and assert
# signature + superblock fields against the vendored fixture.
# Fail closed. Do not print success unless every comparison ran.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
FIXTURE="$ROOT/fixtures/tiny-superblock.h5"
SRC="$ROOT/hdf5.kotoba"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/kotoba-hdf5-v1.XXXXXX")"
trap 'rm -rf "$WORKDIR"' EXIT

fail() {
  printf 'kotoba/checks.sh: %s\n' "$*" >&2
  exit 1
}

command -v python3 >/dev/null 2>&1 || fail "python3 is required"
command -v node >/dev/null 2>&1 || fail "node is required to run the wasm32 module"

if ! command -v kotoba >/dev/null 2>&1; then
  fail "kotoba is not on PATH (need CLI 0.7.2)"
fi

KOTOBA_BIN="$(command -v kotoba)"
printf 'kotoba binary: %s\n' "$KOTOBA_BIN"

CURRENT_LINK="${KOTOBA_HOME:-$HOME/.local/share/kotoba}/current"
if [ -L "$CURRENT_LINK" ]; then
  INSTALLED="$(readlink "$CURRENT_LINK")"
  printf 'kotoba install current: %s\n' "$INSTALLED"
  if [ "$INSTALLED" != "v0.7.2" ]; then
    fail "refusing kotoba $INSTALLED (need v0.7.2)"
  fi
fi

[ -f "$FIXTURE" ] || fail "missing fixture $FIXTURE"
[ -f "$SRC" ] || fail "missing module $SRC"

# Independent field read from the fixture. These numbers come from the
# file bytes, not from the .kotoba source.
eval "$(python3 - "$FIXTURE" "$SRC" <<'PY'
import sys
from pathlib import Path

fixture = Path(sys.argv[1])
src = Path(sys.argv[2]).read_text()
raw = fixture.read_bytes()
sig = bytes([0x89, 0x48, 0x44, 0x46, 0x0D, 0x0A, 0x1A, 0x0A])
if len(raw) < 48:
    sys.exit("fixture shorter than signature + version-0 address table")
if raw[:8] != sig:
    sys.exit("fixture does not start with HDF5 signature \\211HDF\\r\\n\\032\\n")
for i, b in enumerate(raw):
    needle = f"(= i {i}) {b}"
    if needle not in src:
        sys.exit(f"hdf5.kotoba is missing fixture byte {i} = {b}")
version = raw[8]
sizeof_addr = raw[13]
sizeof_size = raw[14]
if version != 0:
    sys.exit(f"this v1 check only reads superblock version 0 (got {version})")
if sizeof_addr != 8:
    sys.exit(f"this v1 check only reads 8-byte offsets (got {sizeof_addr})")
base = int.from_bytes(raw[24:32], "little")
eof = int.from_bytes(raw[40:48], "little")
packed = 1 + 10 * version + 100 * sizeof_addr + 1000 * sizeof_size + 10000 * base + 1000000 * eof
print(f"FIXTURE_LEN={len(raw)}")
print(f"FIELD_VERSION={version}")
print(f"FIELD_SIZEOF_ADDR={sizeof_addr}")
print(f"FIELD_SIZEOF_SIZE={sizeof_size}")
print(f"FIELD_BASE={base}")
print(f"FIELD_EOF={eof}")
print(f"PACKED_EXPECT={packed}")
print(f"EMBEDDED_BYTES={len(raw)}")
PY
)"

printf 'fixture: %s (%s bytes)\n' "$FIXTURE" "$FIXTURE_LEN"
printf 'fixture fields: version=%s sizeof_addr=%s sizeof_size=%s base=%s eof=%s\n' \
  "$FIELD_VERSION" "$FIELD_SIZEOF_ADDR" "$FIELD_SIZEOF_SIZE" "$FIELD_BASE" "$FIELD_EOF"
printf 'embedded fixture bytes in hdf5.kotoba: %s\n' "$EMBEDDED_BYTES"
printf 'packed expect (from fixture bytes): %s\n' "$PACKED_EXPECT"

COMPILE_JSON="$WORKDIR/compile.json"
WASM="$WORKDIR/hdf5.wasm"
set +e
kotoba compile "$SRC" --target wasm -o "$WASM" --json >"$COMPILE_JSON" 2>"$WORKDIR/compile.err"
compile_rc=$?
set -e
if [ "$compile_rc" -ne 0 ]; then
  cat "$COMPILE_JSON" "$WORKDIR/compile.err" >&2 || true
  fail "kotoba compile failed (exit $compile_rc)"
fi

python3 - "$COMPILE_JSON" "$WASM" <<'PY'
import json
import sys
from pathlib import Path

WASM_IMPORT_SECTION = 2


def read_uleb128(buf, i):
    shift = 0
    value = 0
    while True:
        if i >= len(buf):
            raise ValueError("truncated uleb128")
        byte = buf[i]
        i += 1
        value |= (byte & 0x7F) << shift
        if byte & 0x80 == 0:
            return value, i
        shift += 7
        if shift > 35:
            raise ValueError("uleb128 too long")


def wasm_import_section(buf):
    if buf[:4] != b"\x00asm":
        raise ValueError(f"artifact magic {buf[:4]!r} is not wasm")
    if len(buf) < 8:
        raise ValueError("truncated wasm header")
    i = 8
    found = False
    import_count = None
    while i < len(buf):
        section_id = buf[i]
        i += 1
        size, i = read_uleb128(buf, i)
        end = i + size
        if end > len(buf):
            raise ValueError("truncated wasm section")
        payload = buf[i:end]
        i = end
        if section_id == WASM_IMPORT_SECTION:
            found = True
            import_count, _ = read_uleb128(payload, 0) if payload else (0, 0)
    return found, import_count


# Fail closed on the checker itself before trusting the artifact.
_no_import = b"\x00asm\x01\x00\x00\x00"
_empty_import = b"\x00asm\x01\x00\x00\x00\x02\x01\x00"
if wasm_import_section(_no_import) != (False, None):
    sys.exit("import-section checker failed on a no-section wasm")
if wasm_import_section(_empty_import) != (True, 0):
    sys.exit("import-section checker failed to see an import section")

report = json.loads(Path(sys.argv[1]).read_text())
wasm = Path(sys.argv[2])
if report.get("kotoba.cli/ok?") is not True:
    sys.exit(f"compile JSON ok? is {report.get('kotoba.cli/ok?')!r}")
if report.get("kotoba.cli/code") != "emitted":
    sys.exit(f"compile JSON code is {report.get('kotoba.cli/code')!r}")
data = report.get("kotoba.cli/data") or {}
profile = data.get("value-profile")
compat = data.get("compatibility") or {}
target = compat.get("target")
features = data.get("wasm-features") or []
if profile != "i64-v1":
    sys.exit(f"value-profile {profile!r} is not i64-v1")
if target != "wasm32-kotoba-v1":
    sys.exit(f"target {target!r} is not wasm32-kotoba-v1")
blocked = [f for f in features if f in ("simd", "floats", "float", "nontrapping-fptoint")]
if blocked:
    sys.exit(f"unexpected floating/SIMD wasm features: {blocked}")
if not wasm.is_file() or wasm.stat().st_size == 0:
    sys.exit("compile did not write a wasm artifact")
raw = wasm.read_bytes()
has_imports, import_count = wasm_import_section(raw)
if has_imports:
    sys.exit(f"wasm has import section (count={import_count}); FFI is out of v1 scope")
print(
    f"compile: value-profile={profile} target={target} "
    f"wasm-features={features} bytes={len(raw)} import-section=absent"
)
PY

GOT="$(node --input-type=module - "$WASM" <<'JS'
import fs from "node:fs";
const wasm = fs.readFileSync(process.argv[2]);
const { instance } = await WebAssembly.instantiate(wasm);
if (!instance.exports.main) {
  throw new Error("wasm module has no exported main");
}
const value = instance.exports.main();
const n = typeof value === "bigint" ? value : BigInt(value);
process.stdout.write(n.toString());
JS
)"

printf 'wasm main returned: %s\n' "$GOT"
if [ "$GOT" != "$PACKED_EXPECT" ]; then
  fail "packed result $GOT != fixture-derived $PACKED_EXPECT"
fi

printf 'kotoba/checks.sh: compile i64-v1 wasm32 and fixture fields matched\n'
