#!/usr/bin/env python
"""
RangePair Counting
This file is a translation from fortran Cycle-Counting-Range-Mean.for
"""

__author__ = "Nicolas Dubois"


class Flow:
    """TODO"""

    def __init__(
        self, _range: float, _mean: float, peak1: float, peak2: float, noc: float
    ):
        self.range = _range
        self.mean = _mean
        self.peak1 = peak1
        self.peak2 = peak2
        self.noc = noc


peak = []
flows = []

with open("./input.txt", "r") as inFile:

    # Simple Range Counting
    # ##########################################################################

    for line in inFile:
        peak.append(float(line))
        if len(peak) < 2:
            continue

        y = abs(peak[-1] - peak[-2])
        _mean = (peak[-2] + peak[-1]) / 2
        _range = y
        flows.append(Flow(_range, _mean, peak[-2], peak[-1], 1))

    inFile.close()

# Sorting output file
# ##########################################################################
flows.sort(key=lambda x: x.range)

maxrange = max(flows, key=lambda p: p.range).range
minrange = min(flows, key=lambda p: p.range).range
maxmean = max(flows, key=lambda p: p.mean).mean
minmean = min(flows, key=lambda p: p.mean).mean

matrixsize = 64
deltarange = abs(maxrange - minrange) / matrixsize
deltamean = abs(maxmean - minmean) / matrixsize
sumcum = 0.0
cum = 0

# init cc[matrixsize * matrixsize] filled with 0
cc = [[0 for i in range(matrixsize)] for j in range(matrixsize)]

for k in range(len(flows)):
    for i in range(matrixsize):

        if flows[k].range >= minrange + i * deltarange - deltarange / 100:
            if flows[k].range <= minrange + (i + 1) * deltarange + deltarange / 100:

                for j in range(matrixsize):
                    if flows[k].mean >= minmean + j * deltamean - deltamean / 100:
                        if (
                            flows[k].mean
                            <= minmean + (j + 1) * deltamean + deltamean / 100
                        ):

                            cc[i][j] += flows[k].noc


with open("output.txt", "w") as outFile:
    # outFile.write("Range\tMean\tR-ratio\tn\n")

    for i in range(matrixsize):
        for j in range(matrixsize):
            sumcum += cc[i][j]

    for i in range(matrixsize):
        for j in range(matrixsize):
            if cc[i][j] != 0:
                downra = minrange + i * deltarange
                upra = minrange + (i + 1) * deltarange
                mra = (upra + downra) / 2
                downme = minmean + j * deltamean
                upme = minmean + (j + 1) * deltamean
                mme = (upme + downme) / 2
                # dr = -1 + (4 * downme) / (2 * downme + downra)
                # ur = -1 + (4 * upme) / (2 * upme + upra)
                mr = -1 + (4 * mme) / (2 * mme + mra)
                cum += cc[i][j]
                pCum = abs(sumcum - cum) * 100 / sumcum
                outFile.write(f"{mra}\t{mme}\t{mr}\t{cc[i][j] / 2}\t{pCum}\n")
    outFile.close()
