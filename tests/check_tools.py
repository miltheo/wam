"""Fast layout/export contracts; no study figures or analyses are generated."""
from pathlib import Path
import ast,csv,hashlib,importlib.util,sys
sys.dont_write_bytecode=True
ROOT=Path(__file__).resolve().parents[1]
for p in (ROOT/'tools').glob('*.py'):ast.parse(p.read_text(encoding='utf-8-sig'))
with (ROOT/'config/inputs.csv').open(newline='') as f:
    for row in csv.DictReader(f):
        p=ROOT/'data/masters'/row['file']
        assert p.stat().st_size==int(row['bytes'])
        assert hashlib.sha256(p.read_bytes()).hexdigest()==row['sha256']
with (ROOT/'config/figures.csv').open(newline='') as f:figures=list(csv.DictReader(f))
with (ROOT/'config/tables.csv').open(newline='') as f:tables=list(csv.DictReader(f))
assert len(figures)==21 and len(tables)==13
assert len({r['number'] for r in figures})==len(figures)
assert len({r['number'] for r in tables})==len(tables)
spec=importlib.util.spec_from_file_location('export_assets',ROOT/'tools/export_assets.py')
m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
row=dict(region='50-200 RPM',pipeline='Example',p_equivalence_primary='.001',
         p_equivalence_secondary='.002',p_both='.002',p_holm_all48='.048',equivalence_established='TRUE')
s=m.table_fragment('S14','Post hoc regional equivalence',[row])
assert r'\multicolumn{6}' in s and r'\textbf{50-200 RPM}' in s
assert m.ptext('.00001')=='<0.001'
for path in (ROOT/'R').glob('*.R'):
    code=path.read_text(encoding='utf-8-sig')
    assert 'metronautica_metrology(' not in code
    assert 'inst/engine' not in code
    assert 'analysis/retained' not in code
    assert 'setwd(' not in code and 'rm(list' not in code
assert not (ROOT/'manuscript').exists()
assert not (ROOT/'R/signal_features.R').exists()
assert (ROOT/'VERSION').read_text().strip()=='0.1.0'
assert '/data/samples/native/*.gz' in (ROOT/'.gitignore').read_text()
assert 'data/masters/** -text' in (ROOT/'.gitattributes').read_text()
print('PASS: Python syntax, master hashes, asset registries, regional row groups and clean source paths.')
