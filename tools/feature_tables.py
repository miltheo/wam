"""Export four 10% feature-assessment tables from completed study statistics.

This reads scientific outputs and writes CSVs into the selected directory. It does not
edit manuscript prose, change decisions, or generate extra 5% supplement assets.
Publication table fragments are assembled by export_assets.py.
"""
from pathlib import Path
import argparse,csv

ARMS=('primary_native','primary_calibrated','secondary_native','secondary_calibrated')
REGIONS=('50-200 RPM','250-350 RPM','400-500 RPM')

def read(path):
    with path.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))

def export(features,output):
    output.mkdir(parents=True,exist_ok=True)
    for arm in ARMS:
        folder=features/arm
        qualification=[r for r in read(folder/'qualification.csv') if int(r['margin_pct'])==10]
        regional=[r for r in read(folder/'regional.csv') if int(r['margin_pct'])==10]
        response=read(folder/'device_response.csv');rows=[]
        for r in sorted(qualification,key=lambda r:int(r['performance_rank'])):
            devices=[d for d in response if d['metric']==r['metric']]
            assert len(devices)==(4 if arm.startswith('primary') else 3)
            equivalence=int(r['passing'])==int(r['total'])
            adequate=all(float(d['slope_ci_low'])>0 and float(d['inl_ci_high'])<=10 for d in devices)
            assert equivalence==(r['universal_equivalence']=='TRUE')
            assert adequate==(r['adequate_response']=='TRUE')
            assert (equivalence and adequate)==(r['both_criteria']=='TRUE')
            row={'feature_pipeline':r['metric_label'],'metric':r['metric'],'unit':r['metric_unit']}
            for region in REGIONS:
                cell=next(v for v in regional if v['metric']==r['metric'] and v['region']==region)
                row[region]=f"{cell['passing']}/{cell['total']}"
            row.update(full_range=f"{r['passing']}/{r['total']}",
                       lowest_median_r_squared=r['lowest_median_r2'],
                       highest_upper_response_nonlinearity_pct=r['highest_upper_inl_pct'],
                       criteria='E '+('met' if equivalence else 'unmet')+'; R '+('met' if adequate else 'unmet'))
            row['implementation_qualification']=('sampling treatment' if r['metric'] in {'SVMGSEC','SVMGS','ROCAM'}
                else 'SENS upper band clipped at Nyquist' if arm.startswith('primary') and r['metric']=='SVMBP_02_15' else '')
            rows.append(row)
        assert len(rows)==16
        with (output/f'{arm}_10pct.csv').open('w',encoding='utf-8',newline='') as f:
            writer=csv.DictWriter(f,fieldnames=list(rows[0]));writer.writeheader();writer.writerows(rows)
    (output/'table_notes.txt').write_text(
        'E: every device-pair comparison at all ten 50-500 RPM conditions is equivalent at +/-10% of the fixed full-range feature span. '
        'R: every model has a positive slope interval and an upper 95% bootstrap limit for median response nonlinearity <=10%. '
        'Repeated trials are averaged within each of 15 physical units per model. Regional counts retain the full-range equivalence margin. '
        'Sampling-treatment and clipped-bandwidth qualifications are retained in the manuscript table notes.\n',encoding='utf-8')
    print('Four 10% assessment tables exported; manuscript layouts retained separately.')

if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('features',type=Path);parser.add_argument('output',type=Path)
    args=parser.parse_args();export(args.features,args.output)
