install:
	pipenv install

alembic-upgrade:
	pipenv run alembic upgrade head

init-db:
	${MAKE} alembic-upgrade
	pipenv run python3 initial_inject_to_db.py

generate-openapi:
	pipenv run python api.py ../openapi.json

run:
	pipenv run uvicorn ccfatigue.main:app --reload
