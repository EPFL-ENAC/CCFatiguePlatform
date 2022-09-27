install:
	poetry install
	poetry run pre-commit install

lint:
	poetry run pre-commit run --all-files

alembic-upgrade:
	DOTENV_PATH_FOR_DYNACONF="../secrets/.env" poetry run alembic upgrade head

init-database:
	DOTENV_PATH_FOR_DYNACONF="../secrets/.env" poetry run python init_db.py

api:
	DOTENV_PATH_FOR_DYNACONF="../secrets/.env" poetry run python api.py ../openapi.json

test:
	poetry run pytest

compile:
	poetry run python -m numpy.f2py -c fortran/sendeckyj.f -m ccfatigue.fortran.sendeckyj

compile-win:
	rm -rf ccfatigue/fortran
	$(MAKE) compile
	mv ccfatigue/.libs/* ccfatigue/fortran
	rmdir ccfatigue/.libs

run:
	DOTENV_PATH_FOR_DYNACONF="../secrets/.env" poetry run uvicorn ccfatigue.main:app --reload
