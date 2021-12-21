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

generate-api:
	poetry run python api.py ../openapi.json

run-db:
	cd ccfatigue/DB/; docker-compose up

run:
	poetry run uvicorn ccfatigue.main:app --reload
