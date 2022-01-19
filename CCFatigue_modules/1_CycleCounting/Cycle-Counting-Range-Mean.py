#!/usr/bin/env python
"""
RangePair Counting 
This file is a translation from fortran Cycle-Counting-Range-Mean.for
"""

__author__      = "Nicolas Dubois"

from cmath import inf
import os


class Flow:
	""" TODO """
	def __init__(self, _range: float, _mean: float, peak1: float, peak2: float, noc: float):
		self.range = _range
		self.mean = _mean
		self.peak1 = peak1
		self.peak2 = peak2
		self.noc = noc

peak = []
flows = []

    #   ! Enter the input file name and open it for reading
with open("./input.txt", "r") as inFile:
      		
    #   ! Simple Range Counting 
    #   ! ##########################################################################
		
	for line in inFile:
		peak.append(float(line))
		if len(peak) < 2: continue

		y = abs(peak[-1] - peak[-2])						
		_mean = (peak[-2] + peak[-1]) / 2
		_range = y
		flows.append(Flow(_range, _mean, peak[-2], peak[-1], 1))

	inFile.close()
	
    #   ! Sorting output file
    #   ! ##########################################################################
flows.sort(key=lambda x: x.range)

maxrange = max(flows, key=lambda p: p.range).range
minrange = min(flows, key=lambda p: p.range).range
maxmean = max(flows, key=lambda p: p.mean).mean
minmean = min(flows, key=lambda p: p.mean).mean
	
	# OPEN (20, FILE='output.txt')
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
