"""Render the condition-level spectral audit and common-filter feasibility figure."""

from pathlib import Path
import os
import sys

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import numpy as np
import pandas as pd
import argparse


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("master", type=Path)
parser.add_argument("output", type=Path)
args = parser.parse_args()
args.output.parent.mkdir(parents=True, exist_ok=True)
stage = pd.read_csv(args.master)
plt.rcParams.update({
    "font.family": "Arial",
    "font.size": 9,
    "axes.titleweight": "bold",
    "axes.labelsize": 9,
    "axes.titlesize": 10,
    "xtick.labelsize": 8,
    "ytick.labelsize": 8,
})

device_order = ["AX3", "GENEActiv", "LEAP", "SENS"]
device_labels = {"AX3": "AX3", "GENEActiv": "GENEActiv", "LEAP": "LEAP", "SENS": "SENS motion"}
device_colours = {"AX3": "#56B4E9", "GENEActiv": "#009E73", "LEAP": "#CC79A7", "SENS": "#E69F00"}
axis_markers = {"ax_g": "o", "ay_g": "s", "az_g": "^"}
axis_labels = {"ax_g": "X axis", "ay_g": "Y axis", "az_g": "Z axis"}
cutoffs = [9, 10, 11, 26]
rates = stage.groupby("instrument")["fs_hz"].median()
q95 = {device: 0.95 * float(rates[device]) / 2 for device in device_order}
if not all(np.isfinite(value) and value > 0 for value in q95.values()):
    raise ValueError("Audit master must contain finite positive sampling rates")

fig = plt.figure(figsize=(10.4, 7.4), constrained_layout=False)
grid = fig.add_gridspec(2, 4, height_ratios=[1.25, 0.9], hspace=0.52, wspace=0.18,
                        left=0.08, right=0.985, top=0.89, bottom=0.11)

fig.text(0.08, 0.975, "A. Condition-resolved principal spectral peaks", fontsize=11, fontweight="bold")

for i, device in enumerate(device_order):
    ax = fig.add_subplot(grid[0, i])
    x = stage.loc[stage["instrument"] == device].copy()
    expected = x["rpm"] / 60

    ax.plot([0.6, 8.6], [0.6, 8.6], color="black", linestyle="--", linewidth=1.0, zorder=1)
    for axis in ["ax_g", "ay_g", "az_g"]:
        z = x.loc[x["axis"] == axis]
        ax.scatter(z["rpm"] / 60, z["peak_hz"], marker=axis_markers[axis], s=28,
                   facecolor=device_colours[device], edgecolor="white", linewidth=0.55,
                   alpha=0.92, zorder=3)
    ax.axhline(10, color="#0072B2", linestyle="-", linewidth=1.3, zorder=2)
    ax.axhline(q95[device], color="#666666", linestyle=":", linewidth=1.0, zorder=2)
    ax.set_title(f"{device_labels[device]}\n95% Nyquist = {q95[device]:.1f} Hz", pad=6)
    ax.set_xlim(0.55, 8.65)
    ax.set_ylim(0, max(13, q95[device] + 0.8 if q95[device] < 14 else 13))
    ax.set_xticks([0.83, 2.50, 4.17, 5.83, 7.50, 8.33])
    ax.set_xticklabels(["0.83", "2.50", "4.17", "5.83", "7.5", "8.33"], rotation=0)
    # Separate the two highest-frequency labels without omitting the 500-RPM endpoint.
    ax.get_xticklabels()[-2].set_ha("right")
    ax.get_xticklabels()[-1].set_ha("left")
    ax.grid(True, color="#E5E5E5", linewidth=0.6)
    ax.set_axisbelow(True)
    if i == 0:
        ax.set_ylabel("Observed principal peak (Hz)")
    ax.set_xlabel("Programmed frequency (Hz)")

fig.text(0.08, 0.405, "B. Universal cut-off feasibility across device sampling rates", fontsize=11, fontweight="bold")
ax = fig.add_subplot(grid[1, :])
ypos = np.arange(len(device_order))[::-1]
for y, device in zip(ypos, device_order):
    ax.hlines(y, 0, q95[device], color=device_colours[device], linewidth=8, alpha=0.75, zorder=2)
    ax.plot(q95[device], y, marker="|", markersize=15, color="black", markeredgewidth=1.2, zorder=4)
    ax.text(q95[device] + 0.9, y, f"{q95[device]:.1f} Hz", va="center", fontsize=8)

ax.axvspan(0.83, 8.33, color="#D9D9D9", alpha=0.55, zorder=0)
ax.axvline(9, color="#999999", linestyle="--", linewidth=1.0, zorder=1)
ax.axvline(10, color="#0072B2", linestyle="-", linewidth=1.8, zorder=1)
ax.axvline(11, color="#999999", linestyle="--", linewidth=1.0, zorder=1)
ax.axvline(26, color="#D55E00", linestyle=":", linewidth=1.5, zorder=1)
ax.hlines(3.48, 10.2, 13.5, color="#0072B2", linewidth=0.9)
ax.text(13.8, 3.48, "Selected 10-Hz cutoff", va="center", fontsize=8, color="#0072B2")
ax.hlines(0.0, 26.2, 30.0, color="#D55E00", linewidth=0.9)
ax.text(30.3, 0.0, "26 Hz invalid for SENS motion", va="center", fontsize=8, color="#D55E00")
ax.set_xlim(0, 65)
ax.set_ylim(-0.75, 3.95)
ax.set_yticks(ypos)
ax.set_yticklabels([device_labels[d] for d in device_order])
ax.set_xlabel("Frequency (Hz)", labelpad=7)
cutoff_ticks = [0, 9, 10, 11, 20, 26, 30, 40, 50, 60]
ax.set_xticks(cutoff_ticks)
ax.set_xticklabels([str(x) for x in cutoff_ticks])
for label, value in zip(ax.get_xticklabels(), cutoff_ticks):
    if value in {9, 10, 11, 26}:
        label.set_rotation(0)
        label.set_va("top")
        label.set_fontsize(7)
ax.grid(axis="x", color="#E5E5E5", linewidth=0.6)
ax.set_axisbelow(True)

handles = [
    Line2D([0], [0], color="black", linestyle="--", lw=1.0, label="Programmed frequency"),
    Line2D([0], [0], marker="o", color="white", markerfacecolor="#777777", markeredgecolor="white", markersize=6, label="Observed axis peak"),
    Line2D([0], [0], color="#999999", linestyle="--", lw=1.0, label="9- and 11-Hz candidates"),
    Line2D([0], [0], color="#0072B2", lw=1.8, label="Selected 10-Hz cutoff"),
    Line2D([0], [0], color="#666666", linestyle=":", lw=1.0, label="95% Nyquist limit"),
    Line2D([0], [0], color="#D55E00", linestyle=":", lw=1.5, label="26-Hz sensitivity cutoff"),
]
fig.legend(handles=handles, loc="lower center", ncol=6, frameon=False, fontsize=8,
           bbox_to_anchor=(0.52, 0.005), handlelength=2.2, columnspacing=1.1)

fig.savefig(args.output, dpi=600, facecolor="white")
print(args.output)
