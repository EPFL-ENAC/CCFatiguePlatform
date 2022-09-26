#!/usr/bin/env python
"""

CCFatigue Platform, Analysis module 1: Cycle Counting
Method: Rainflow Counting.

Rainflow counting is a method that {Insert description here}.

Input:
- Data format to be defined !!!

Output:
- Description of data output, in CYC format. (see CYC Data convention)
 """

MATRIX_SIZE = 64
INPUT_FILENAME = "input.txt"
OUTPUT_FILENAME = "CYC-Rainflow.csv"


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

    def __str__(self):
        return (
            f"{self.range:10.4f} {self.mean:10.4f} {self.peak1:10.4f} "
            "{self.peak2:10.4f} {self.noc}"
        )


peak = []
flows = []


with open(INPUT_FILENAME, "r", encoding="UTF-8") as inFile:

    # Rainflow Counting
    # ##########################################################################

    STARP = 1

    for line in inFile:
        peak.append(float(line))

        while len(peak) >= 3:

            x = abs(peak[-1] - peak[-2])
            y = abs(peak[-2] - peak[-3])

            if x < y:
                break

            _mean = (peak[-2] + peak[-3]) / 2
            _range = y

            if STARP == len(peak) - 2:  # len(peak) == 3
                flows.append(Flow(_range, _mean, peak[-3], peak[-2], 1))
                peak.pop(-2)

            else:
                flows.append(Flow(_range, _mean, peak[-3], peak[-2], 2))
                peak.pop(-2)
                peak.pop(-2)

    for j in range(len(peak) - 2):
        y = abs(peak[j + 1] - peak[j])
        halfrange = y
        _mean = (peak[j + 1] + peak[j]) / 2
        flows.append(Flow(halfrange, _mean, peak[-2], peak[-1], 1))

    inFile.close()


# Sorting output file
# ##########################################################################
# TODO: check with Tassos, seems this step could be skipped
flows.sort(key=lambda x: x.range)


# Creating 64*64 markov matrix
# ##########################################################################


maxrange = max(flows, key=lambda p: p.range).range
minrange = min(flows, key=lambda p: p.range).range
maxmean = max(flows, key=lambda p: p.mean).mean
minmean = min(flows, key=lambda p: p.mean).mean

deltarange = abs(maxrange - minrange) / MATRIX_SIZE
deltamean = abs(maxmean - minmean) / MATRIX_SIZE
sumcum = 0.0
cum = 0

# init cc[MATRIX_SIZE * MATRIX_SIZE] filled with 0
cc = [[0 for i in range(MATRIX_SIZE)] for j in range(MATRIX_SIZE)]


for flow in flows:
    for i in range(MATRIX_SIZE):
        if (
            flow.range >= minrange + i * deltarange - deltarange / 100
            and flow.range <= minrange + (i + 1) * deltarange + deltarange / 100
        ):

            for j in range(MATRIX_SIZE):
                if (
                    flow.mean >= minmean + j * deltamean - deltamean / 100
                    and flow.mean <= minmean + (j + 1) * deltamean + deltamean / 100
                ):

                    cc[i][j] += flow.noc

with open("output.txt", "w", encoding="UTF-8") as outFile:
    # outFile.write("Range\tMean\tR-ratio\tn\n")

    for i in range(MATRIX_SIZE):
        for j in range(MATRIX_SIZE):
            sumcum += cc[i][j]

    for i in range(MATRIX_SIZE):
        for j in range(MATRIX_SIZE):
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
                pcum = abs(sumcum - cum) * 100 / sumcum
                outFile.write(f"{mra}\t{mme}\t{mr}\t{cc[i][j] / 2}\t{pcum}\n")
    outFile.close()
