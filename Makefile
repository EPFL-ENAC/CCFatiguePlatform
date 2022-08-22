install: install_backend install_frontend

compile: compile_sncurve

build: install compile build_frontend

install_backend:
	$(MAKE) -C backend install

install_frontend:
	$(MAKE) -C frontend install

init-db:
	${MAKE} -C backend init-db

init-db-with-fake:
	${MAKE} -C backend init-db-with-fake

compile_sncurve:
	$(MAKE) -C CCFatigue_modules/2_S-NCurves compile

build_frontend:
	$(MAKE) -C frontend build

api:
	$(MAKE) -C backend api
	$(MAKE) -C frontend api

run-backend:
	$(MAKE) -C backend run

run-frontend:
	$(MAKE) -C frontend run

run-database:
	docker-compose up -d database

deploy-docker:
	docker-compose build --parallel --pull
	docker-compose up --remove-orphans
