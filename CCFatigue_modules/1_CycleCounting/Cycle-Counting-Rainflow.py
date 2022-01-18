#!/usr/bin/env python
""" Rainflow Counting
This file is a translation from fortran Cycle-Counting-Rainflow.for
 """

__author__      = "Nicolas Dubois"


import sys
# from array import array
# import numpy as np
# import pandas as pd
import os


class Flow:
	""" TODO """
	def __init__(self, _range: float, _mean: float, peak1: float, peak2: float, noc: float):
		self.range = _range
		self.mean = _mean
		self.peak1 = peak1
		self.peak2 = peak2
		self.noc = noc
		# self.r = max(peak1, peak2) / min(peak1, peak2)

	def __str__(self):
		return "{:10.4f} {:10.4f} {:10.4f} {:10.4f} {}".format(self.range, self.mean, self.peak1, self.peak2, self.noc)

peak = []#array('d')
flows = []


#os.chdir(os.path.dirname(__file__)) # cd to this file dir
with open("./input.txt", "r") as inFile:

    #   ! Rainflow Counting
    #   ! ##########################################################################

	starp = 1

	for line in inFile:
		peak.append(float(line))

		while len(peak) >= 3:

			x = abs(peak[-1] - peak[-2])
			y = abs(peak[-2] - peak[-3])

			if x < y: break # exit while

			if starp == len(peak) - 2: # or starp == i - 1:
				halfrange = y
				_mean = (peak[-2] + peak[-3]) / 2
				flows.append(Flow(halfrange, _mean, peak[-3], peak[-2], 1))

				peak.pop(-2)
				continue # with next while iteration

			_mean = (peak[-2] + peak[-3]) / 2
			_range = y
			flows.append(Flow(_range, _mean, peak[-3], peak[-2], 2))
			peak.pop(-2)
			peak.pop(-2)


	for j in range(len(peak) - 2):
		y = abs(peak[j + 1] - peak[j])
		halfrange = y
		_mean = (peak[j + 1] + peak[j]) / 2
		flows.append(Flow(halfrange, _mean, peak[-2], peak[-1], 1))

	inFile.close()


# 	! Sorting output file
# 	! ##########################################################################
# flows.sort(key=lambda x: x.range)

# for i in range(len(flows) - 1):
# 	for j in range(i + 1, len(flows) - 1):
# 		if flows[i].range > flows[j].range:
# 			# Switch both Flows
# 			# flows.insert(i, flows.pop(j))
# 			# flows.insert(j, flows.pop(i + 1))
# 			flows[i], flows[j] = flows[j], flows[i]


# with open("output.txt", "w") as outFile:
# 	outFile.writelines("\n".join([str(x) for x in flows]))
# 	outFile.close()

# sys.exit()

# 	! Creating 64*64 markov matrix
# 	! ##########################################################################


maxrange = max(flows, key=lambda p: p.range).range
minrange = min(flows, key=lambda p: p.range).range
maxmean = max(flows, key=lambda p: p.mean).mean
minmean = min(flows, key=lambda p: p.mean).mean

matrixsize = 64
Deltarange = abs(maxrange - minrange) / matrixsize
Deltamean = abs(maxmean - minmean) / matrixsize
Sumcum = 0.0
Cum = 0

# init cc[matrixsize * matrixsize] filled with 0
cc = [ [ 0 for i in range(matrixsize) ] for j in range(matrixsize) ]

for k in range(len(flows)):
	for i in range(matrixsize):
		if flows[k].range >= minrange + i * Deltarange - Deltarange / 100:
			if flows[k].range <= minrange + (i + 1) * Deltarange + Deltarange / 100:

				for j in range(matrixsize):
					if flows[k].mean >= minmean + j * Deltamean - Deltamean / 100:
						if flows[k].mean <= minmean + (j + 1) * Deltamean + Deltamean / 100:

							cc[i][j] += flows[k].noc


with open("output.txt", "w") as outFile:
	# outFile.write("Range\tMean\tR-ratio\tn\n")

	for i in range(matrixsize):
		for j in range(matrixsize):
			Sumcum += cc[i][j]

	for i in range(matrixsize):
		for j in range(matrixsize):
			if cc[i][j] != 0:
				Downra = minrange + i * Deltarange
				Upra = minrange + (i + 1) * Deltarange
				Mra = (Upra + Downra) / 2
				Downme = minmean + j * Deltamean
				Upme = minmean + (j + 1) * Deltamean
				Mme = (Upme + Downme) / 2
				DR = -1 + (4 * Downme) / (2 * Downme + Downra)
				UR = -1 + (4 * Upme) / (2 * Upme + Upra)
				MR = -1 + (4 * Mme) / (2 * Mme + Mra)
				Cum += cc[i][j]
				PCum = abs(Sumcum - Cum) * 100 / Sumcum
				outFile.write(f"{Mra}\t{Mme}\t{MR}\t{cc[i][j] / 2}\t{PCum}\n")
	outFile.close()
