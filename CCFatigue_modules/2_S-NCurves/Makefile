compile: compile_linlog compile_loglog compile_sendeckyj compile_whitney

fortran := gfortran -finit-real=zero -finit-integer=0

compile_linlog:
	$(fortran) S-N-Curve-LinLog.for -o S-N-Curve-LinLog

compile_loglog:
	$(fortran) S-N-Curve-LogLog.for -o S-N-Curve-LogLog

compile_sendeckyj:
	$(fortran) S-N-Curve-Sendeckyj.for -o S-N-Curve-Sendeckyj

compile_whitney:
	$(fortran) S-N-Curve-Whitney.for -o S-N-Curve-Whitney

run: run_linlog run_loglog run_sendeckyj run_whitney

run_linlog:
	./S-N-Curve-LinLog input.txt > output-LinLog.txt

run_loglog:
	./S-N-Curve-LogLog input.txt > output-LogLog.txt

run_sendeckyj:
	./S-N-Curve-Sendeckyj input.txt > output-Sendeckyj.txt

run_whitney:
	./S-N-Curve-Whitney input.txt > output-Whitney.txt
