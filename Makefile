install: install_backend install_frontend

compile: compile_sncurve

build: install compile build_frontend

install_backend:
	$(MAKE) -C backend install

install_frontend:
	$(MAKE) -C frontend install

compile_sncurve:
	$(MAKE) -C CCFatigue_modules/2_S-NCurves compile

build_frontend:
	$(MAKE) -C frontend build
