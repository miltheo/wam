"""Assemble the six device model previews and their model-derived dimensions."""
import argparse,csv
from pathlib import Path
import matplotlib.pyplot as plt
from PIL import Image

def render(folder,output):
    with (folder/'dimensions.csv').open(newline='',encoding='utf-8-sig') as f:
        records=list(csv.DictReader(f))
    assert len(records)==6
    fig,axes=plt.subplots(2,3,figsize=(11.7,8.3))
    for letter,ax,row in zip('ABCDEF',axes.flat,records):
        img=Image.open(folder/'previews'/Path(row['file']).with_suffix('.png').name)
        ax.imshow(img);ax.axis('off')
        ax.set_title(f"{letter}. {row['device']}",loc='left',fontsize=12,fontweight='bold')
        ax.text(.5,-.025,f"H {row['height_mm']} × W {row['width_mm']} × D {row['depth_mm']} mm",
                transform=ax.transAxes,ha='center',fontsize=10)
    fig.tight_layout(pad=2)
    output.parent.mkdir(parents=True,exist_ok=True)
    fig.savefig(output,dpi=300,facecolor='white')
    fig.savefig(output.with_suffix('.pdf'),facecolor='white')
    plt.close(fig)

if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('folder',type=Path);p.add_argument('output',type=Path)
    a=p.parse_args();render(a.folder,a.output)
