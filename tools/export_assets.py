"""Export numbered result figures, full-precision CSVs and LaTeX table fragments.

No manuscript is read or edited. Authored framework/setup artwork and definition
tables belong to the manuscript and are deliberately outside this result registry.
"""
import argparse,csv,hashlib,shutil
from pathlib import Path

ROOT=Path(__file__).resolve().parents[1]
def read(path):
    with path.open(newline='',encoding='utf-8-sig') as f:return list(csv.DictReader(f))
def tex(value):
    return ''.join({'\\':r'\textbackslash{}','&':r'\&','%':r'\%','_':r'\_',
                    '#':r'\#','$':r'\$','{':r'\{','}':r'\}'}.get(c,c) for c in str(value)).replace('\n',r'\newline ')
def ptext(value):
    p=float(value)
    return '<0.001' if p<.001 else f'{p:.3f}'
def table_fragment(number,title,rows):
    if number=='S14':
        fields=['pipeline','p_equivalence_primary','p_equivalence_secondary','p_both','p_holm_all48','equivalence_established']
        labels=['Feature pipeline','Primary p','Secondary p','Across-comparison p','Holm-adjusted p','Equivalence established']
    else:
        fields=list(rows[0]);labels=[f.replace('_',' ') for f in fields]
    n=len(fields)
    landscape=n>6 or number=='S14'
    out=([r'\begin{landscape}'] if landscape else [])+[r'\begingroup',r'\small',r'\setlength{\tabcolsep}{3pt}',
         r'\begin{longtable}{@{}*{'+str(n)+r'}{>{\raggedright\arraybackslash}p{\dimexpr\linewidth/'+str(n)+r'-2\tabcolsep\relax}}@{}}',
         r'\caption{\textbf{'+tex(title)+r'}}\label{tab:'+number+r'}\\',r'\toprule',
         ' & '.join(r'\textbf{'+tex(x)+'}' for x in labels)+r'\\',r'\midrule\endhead']
    region=None
    for row in rows:
        if number=='S14' and row['region']!=region:
            region=row['region'];out.append(r'\multicolumn{6}{l}{\textbf{'+tex(region)+r'}}\\')
        cells=[]
        for f in fields:
            value=row[f]
            if number=='S14' and f.startswith('p_'):value=ptext(value)
            cells.append(tex(value))
        out.append(' & '.join(cells)+r'\\')
    out.extend([r'\bottomrule',r'\end{longtable}'])
    if number in {'4','S13'}:
        out.append(r'\noindent E: all device pairs equivalent at all tested conditions at the fixed '+
                   r'$\pm10\%$ full-range feature-span margin. R: positive slope confidence interval '+
                   r'and upper 95\% bootstrap limit for response non-linearity $\leq10\%$ for every device model.')
    if number=='S14':
        out.append(r'\noindent Pipeline-region p values are intersection-union maxima over the required '+
            r'device pairs and stages, then over both native comparisons. Holm adjustment covers all '+
            r'48 pipeline-region tests. Repeated trials are averaged within each physical unit before comparison.')
    out.append(r'\endgroup')
    if landscape:out.append(r'\end{landscape}')
    return '\n'.join(out)+'\n'
def export(analysis,results):
    figures=read(ROOT/'config/figures.csv');tables=read(ROOT/'config/tables.csv')
    missing=[r['source'] for r in figures+tables if not (analysis/r['source']).is_file()]
    if missing:raise FileNotFoundError('Run the missing analysis stages first:\n'+'\n'.join(missing))
    manifest=[]
    for row in figures:
        src=analysis/row['source'];dest=results/'figures'/f"figure-{row['number']}.png"
        dest.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(src,dest)
        manifest.append(dict(kind='Figure',number=row['number'],file=str(dest.relative_to(results)),
                             sha256=hashlib.sha256(dest.read_bytes()).hexdigest()))
        if src.with_suffix('.pdf').is_file():shutil.copy2(src.with_suffix('.pdf'),dest.with_suffix('.pdf'))
    for row in tables:
        src=analysis/row['source'];dest=results/'tables'/f"table-{row['number']}.csv"
        dest.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(src,dest)
        data=read(src)
        if row['number']=='S14':
            order={v:i for i,v in enumerate(['50-200 RPM','250-350 RPM','400-500 RPM'])}
            data.sort(key=lambda r:(order[r['region']],r['pipeline']))
        dest.with_suffix('.tex').write_text(table_fragment(row['number'],row['title'],data),encoding='utf8')
        manifest.append(dict(kind='Table',number=row['number'],file=str(dest.relative_to(results)),
                             sha256=hashlib.sha256(dest.read_bytes()).hexdigest()))
    with (results/'assets.csv').open('w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(manifest[0]));w.writeheader();w.writerows(manifest)
    print(f'Exported {len(figures)} figures and {len(tables)} result tables from this run.')
if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('analysis',type=Path);p.add_argument('results',type=Path)
    a=p.parse_args();export(a.analysis,a.results)
