install: install_backend install_frontend

compile: compile_sncurve

build: install compile build_frontend

install_backend:
	$(MAKE) -C backend install

install_frontend:
	$(MAKE) -C frontend install

init-db:
	${MAKE} -C backend init-db

compile_sncurve:
	$(MAKE) -C CCFatigue_modules/2_S-NCurves compile

build_frontend:
	$(MAKE) -C frontend build

generate-api:
	$(MAKE) -C backend generate-api
	$(MAKE) -C frontend generate-api

run-preprocessing:
	$(MAKE) -C backend/preprocessing run

run-db:
	$(MAKE) -C backend run-db

run-backend:
	$(MAKE) -C backend run

run-frontend:
	$(MAKE) -C frontend run
