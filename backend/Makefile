install:
	poetry install

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
	poetry run python -m numpy.f2py -c ccfatigue/analysis/fortran/sendeckyj.f -m ccfatigue.analysis.fortran.sendeckyj

compile-win:
	rm -rf ccfatigue/analysis/fortran/*.dll
	$(MAKE) compile
	mv ccfatigue/.libs/*.dll ccfatigue/analysis/fortran
	rmdir ccfatigue/.libs

run:
	DOTENV_PATH_FOR_DYNACONF="../secrets/.env" poetry run uvicorn ccfatigue.main:app --reload
