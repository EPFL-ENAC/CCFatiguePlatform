install:
	poetry install
	poetry run pre-commit install

lint:
	poetry run pre-commit run --all-files

alembic-upgrade:
	poetry run dotenv -f ../secrets/.env run alembic upgrade head

init-database:
	${MAKE} alembic-upgrade
	poetry run dotenv -f ../secrets/.env run python3 initial_inject_to_db.py

api:
	poetry run dotenv -f ../secrets/.env run python api.py ../openapi.json

run:
	poetry run dotenv -f ../secrets/.env run uvicorn ccfatigue.main:app --reload
