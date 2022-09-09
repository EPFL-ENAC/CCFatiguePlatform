# install* -> install libs localy (without Docker)
install: install-backend install-frontend

install-backend:
	$(MAKE) -C backend install

install-frontend:
	$(MAKE) -C frontend install

# generation -> run all auto-generated things 
generation: api preprocessing

api:
	$(MAKE) -C backend api
	$(MAKE) -C frontend api

preprocessing:
	$(MAKE) -C backend/preprocessing run


# dev-* -> run things localy for dev/testing
dev-database:
	docker-compose up -d database

dev-backend:
	$(MAKE) -C backend run

dev-frontend:
	$(MAKE) -C frontend run

init-database:
	${MAKE} -C backend init-database


# run -> run the whole project on the server (dockerized)
# can be run on the laptop also ... to final check everything
run:
	docker-compose build --parallel --pull
	docker-compose up --remove-orphans


# compile* -> does all Fortran compilation
compile: compile_sncurve

compile_sncurve:
	$(MAKE) -C CCFatigue_modules/2_S-NCurves compile
