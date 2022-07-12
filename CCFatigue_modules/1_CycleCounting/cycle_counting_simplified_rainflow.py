#!/usr/bin/env python
""" Simplified Rainflow Counting
This file is a translation from fortran Cycle-Counting-Simplified-Rainflow.for
 """

__author__ = "Nicolas Dubois"


# real*8 x,y,range,mean,pp1,pp2,pp3,pp4,pp5,cc(100,100)
# real*8 peak(1000000),ra(1000000),me(1000000),R(1000000)
# real*8 p1(1000000),p2(1000000),p3(1000000),p4(1000000)
# real*8 p(1000000),noc(1000000),p5(1000000)
# real*8 minrange,maxrange,minmean,maxmean,Deltarange,Deltamean
# real*8 maxpeak
# real*8 Downra,Upra,Mra,Downme,Upme,Mme,MR,DR,UR,Sumcum,Cum
# integer i,j,ierr,matrixsize


# CHARACTER*40 NAME


from cmath import inf


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
        # self.r = max(peak1, peak2) / min(peak1, peak2)

    def __str__(self):
        return "{:10.4f} {:10.4f} {:10.4f} {:10.4f} {}".format(
            self.range, self.mean, self.peak1, self.peak2, self.noc
        )


peak = []  # array('d')
flows = []
maxpeak = 0


# Simplified Rainflow Counting
# ##########################################################################

# Rainflow Counting
# ##########################################################################
with open("./input.txt", "r") as inFile:

    for line in inFile:
        peak.append(float(line))

    inFile.close()

maxpeak = max(peak)
posMaxPeak = peak.index(maxpeak)

modifiedPeak = peak[posMaxPeak:]
if peak[0] == peak[-1]:
    modifiedPeak.append(peak[1:posMaxPeak])
else:
    modifiedPeak.append(peak[0:posMaxPeak])

peak = []

for p in modifiedPeak:
    peak.append(p)

    while len(peak) >= 3:
        x = abs(peak[-1] - peak[-2])
        y = abs(peak[-2] - peak[-3])
        if x < y:
            break

        _mean = (peak[-2] + peak[-3]) / 2
        _range = y
        flows.append(Flow(_range, _mean, peak[-3], peak[-2], 2))
        peak.pop(-2)
        peak.pop(-2)


# Sorting output file
# ##########################################################################
flows.sort(key=lambda x: x.range)

# Creating 64*64 markov matrix
# ##########################################################################

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
                dr = -1 + (4 * downme) / (2 * downme + downra)
                ur = -1 + (4 * upme) / (2 * upme + upra)
                mr = -1 + (4 * mme) / (2 * mme + mra)
                cum += cc[i][j]
                pcum = abs(sumcum - cum) * 100 / sumcum
                outFile.write(f"{mra}\t{mme}\t{mr}\t{cc[i][j] / 2}\t{pcum}\n")
    outFile.close()
