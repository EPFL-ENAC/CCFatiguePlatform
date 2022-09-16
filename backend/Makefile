install:
	poetry install
	poetry run pre-commit install

lint:
	poetry run pre-commit run --all-files

alembic-upgrade:
	poetry run dotenv -f ../secrets/.env run alembic upgrade head

init-database:
	poetry run dotenv -f ../secrets/.env run python3 init_db.py

api:
	poetry run dotenv -f ../secrets/.env run python api.py ../openapi.json

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
	poetry run dotenv -f ../secrets/.env run uvicorn ccfatigue.main:app --reload
