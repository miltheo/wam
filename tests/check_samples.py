"""Verify optional native examples; the study master-to-assets run does not use them."""
from pathlib import Path
import csv, gzip, hashlib, argparse

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--require-all", action="store_true", help="Require all nine release attachments locally.")
args = parser.parse_args()
checked = 0
missing = []

ROOT = Path(__file__).resolve().parents[1]
SAMPLES = ROOT / 'data/samples/native'
with (SAMPLES / 'sample_index.csv').open(newline='', encoding='utf-8-sig') as stream:
    rows = list(csv.DictReader(stream))
assert len(rows) == 9
assert len({(r['device'], r['type']) for r in rows}) == 9

def digest(stream):
    result = hashlib.sha256()
    size = 0
    for block in iter(lambda: stream.read(4 * 1024 * 1024), b''):
        result.update(block)
        size += len(block)
    return result.hexdigest(), size

for row in rows:
    for field in ('storage_file', 'file_name'):
        assert Path(row[field]).name == row[field] and '/' not in row[field] and '\\' not in row[field]
    path = SAMPLES / row['storage_file']
    if not path.is_file():
        missing.append(row['storage_file'])
        continue
    checked += 1
    with path.open('rb') as stream:
        observed, size = digest(stream)
    assert observed == row['storage_sha256'] and size == int(row['storage_bytes'])
    with gzip.open(path, 'rb') as stream:
        observed, size = digest(stream)
    assert observed == row['native_sha256'] and size == int(row['native_bytes'])
# An adjacent development checkout can also verify the package's pinned catalogue.
package_catalogue = ROOT.parent / 'Metronautica/inst/extdata/native/sample_index.csv'
if package_catalogue.exists():
    assert package_catalogue.read_bytes() == (SAMPLES / 'sample_index.csv').read_bytes()
if args.require_all and missing:
    raise SystemExit('Missing release attachments: ' + ', '.join(missing))
print(f'PASS: sample catalogue identity and {checked} compressed/restored recordings verified; {len(missing)} optional release attachments absent.')
