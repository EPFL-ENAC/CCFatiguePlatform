## Parameters

Stress_N = ['5','6'] ## From STD csv data
Strain_N = [] 
run_out = 0 ## From STD json data
parameter = *## from extra test  ....







def calculate_creep():
	"""define what function does"""

	creep = 0 ## algorithm

	return creep

def calculate_TDE(Stress_N ,Strain_N):
	"""define here what this function does"""

	...


	return TDE

def calculate_Stress_fail(Stress_N):
	"""..."""

	failure = !run_out

	if failure:
		Stress_fail=max(Stress_N)

	else:
		print('Not implemented Stress failure calculation yet for tests without failure')
		Stress_fail = None
		...

	return Stress_fail

