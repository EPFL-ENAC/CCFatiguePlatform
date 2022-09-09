install:
	poetry install
	poetry run pre-commit install

lint:
	poetry run pre-commit run --all-files

alembic-upgrade:
	poetry run alembic upgrade head

init-db:
	${MAKE} alembic-upgrade
	poetry run python3 initial_inject_to_db.py

init-db-with-fake:
	${MAKE} alembic-upgrade
	poetry run python3 initial_inject_to_db.py --add-fake-data

api:
	POSTGRES_PASSWORD="" poetry run python api.py ../openapi.json

run:
	poetry run uvicorn ccfatigue.main:app --reload

test:
	poetry run pytest

compile:
	poetry run python -m numpy.f2py -c fortran/sendeckyj.f -m ccfatigue.fortran.sendeckyj
