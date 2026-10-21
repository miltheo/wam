"""Aggregate the five-rule diagnostic observations by unit and model."""
from pathlib import Path
import csv,argparse
from collections import defaultdict
from statistics import median
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument("master",type=Path)
parser.add_argument("output",type=Path)
args=parser.parse_args();root=args.output;root.mkdir(parents=True,exist_ok=True)
models = ['AX3','GENEActiv','GT3X-BT','GT9X Link','LEAP','SENS motion']
with (args.master).open(newline='') as f:
    records = list(csv.DictReader(f))
for r in records:
    if r['instrument']=='SENS': r['instrument']='SENS motion'
assert len(records) == 2790
def num(r,k): return float(r[k])
def truth(r,k): return r[k] == 'TRUE'
def bad(r): return any(num(r,k)>0 for k in ['missing_time','duplicate_time','backward_time','invalid_axis'])
def pattern(r): return truth(r,'all_zero_sequence') or truth(r,'equal_nonzero_sequence')
def near(r): return num(r,'near_limit_n')>0
def fmt(x):
    if x == 0: return '0'
    if abs(x)<.001: return '<0.001'
    return f'{x:.3f}'.rstrip('0').rstrip('.')
def write(name, rows):
    with (root/name).open('w',newline='',encoding='utf-8') as f:
        w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)

byunit=defaultdict(list)
for r in records:
    byunit[(r['family'],r['instrument'],r['id'])].append(r)
units=[]
for (family,model,id_),rs in byunit.items():
    u=dict(family=family,instrument=model,id=id_,records=len(rs),
        integrity=any(bad(r) for r in rs),all_zero=any(truth(r,'all_zero_sequence') for r in rs),
        equal_nonzero=any(truth(r,'equal_nonzero_sequence') for r in rs),near_limit=any(near(r) for r in rs),
        flagged_records=sum(bad(r) or pattern(r) or near(r) for r in rs),
        integrity_records=sum(bad(r) for r in rs),pattern_records=sum(pattern(r) for r in rs),
        near_limit_records=sum(near(r) for r in rs),
        median_spacing_ms=median(num(r,'median_dt_ms') for r in rs),
        highest_iqr_ms=max(num(r,'iqr_dt_ms') for r in rs),
        longest_spacing_ms=max(num(r,'longest_dt_ms') for r in rs),
        all_zero_samples=sum(num(r,'all_zero_samples') for r in rs),
        equal_nonzero_samples=sum(num(r,'equal_nonzero_samples') for r in rs))
    u['integrity_or_pattern']=u['integrity'] or u['all_zero'] or u['equal_nonzero']
    u['any_flag']=u['integrity_or_pattern'] or u['near_limit']
    for axis in 'xyz':
        u[f'zero_{axis}_pct']=100*sum(num(r,f'zero_{axis}_n') for r in rs)/sum(num(r,f'finite_{axis}_n') for r in rs)
    u['near_limit_pct']=100*sum(num(r,'near_limit_n') for r in rs)/sum(num(r,'finite_axis_n') for r in rs)
    units.append(u)
assert sum(u['family']=='static' for u in units)==90
assert sum(u['family']=='dynamic' for u in units)==90
assert all(u['records']==(50 if u['instrument']=='LEAP' else 20 if u['instrument'] in ['GT3X-BT','GT9X Link'] else 30) for u in units if u['family']=='dynamic')
write('unit_diagnostics.csv',units)
summaries=[]
for family in ['static','dynamic']:
    for m in models:
        us=[u for u in units if u['family']==family and u['instrument']==m]
        if not us: continue
        summaries.append(dict(family=family,instrument=m,units=len(us),records=sum(u['records'] for u in us),
            **{k:sum(u[k] for u in us) for k in ['integrity','all_zero','equal_nonzero','near_limit','integrity_or_pattern','any_flag']},
            flagged_records=sum(u['flagged_records'] for u in us),near_limit_records=sum(u['near_limit_records'] for u in us)))
write('model_flag_summary.csv',summaries)
flagged=[u for u in units if u['any_flag']]
if flagged: write('flagged_units.csv',flagged)


rows=[['Condition / model','Units','1. Data integrity\nUnits flagged',
       '2. Sample timing\nTypical / spread / longest (ms)',
       '3. Axis-zero proportion\nX / Y / Z (%): median; [highest]',
       '4. Unusual cross-axis patterns\nUnits flagged: all-zero / equal non-zero',
       '5. Near-limit values\nMedian [highest] %; units; records']]
presentation=[]
for s in summaries:
    group=[u for u in units if u['family']==s['family'] and u['instrument']==s['instrument']]
    timing=' / '.join([fmt(median(u['median_spacing_ms'] for u in group)),fmt(max(u['highest_iqr_ms'] for u in group)),fmt(max(u['longest_spacing_ms'] for u in group))])
    zeros=' / '.join(fmt(median(u[f'zero_{a}_pct'] for u in group)) for a in 'xyz')+'\n['+' / '.join(fmt(max(u[f'zero_{a}_pct'] for u in group)) for a in 'xyz')+']'
    row=[s['family'].capitalize()+'\n'+s['instrument'],str(s['units']),f"{s['integrity']}/{s['units']}",timing,zeros,
        f"{s['all_zero']}/{s['units']} ; {s['equal_nonzero']}/{s['units']}",
        f"{fmt(median(u['near_limit_pct'] for u in group))} [{fmt(max(u['near_limit_pct'] for u in group))}]%\n{s['near_limit']}/{s['units']} units; {s['near_limit_records']}/{s['records']} records"]
    rows.append(row); presentation.append(dict(zip(rows[0],row)))
write('combined_results_table.csv',presentation)

