install:
	pipenv install

alembic-upgrade:
	pipenv run alembic upgrade head

run:
	pipenv run uvicorn ccfatigue.main:app --reload
