# DB backend

## Install

Do it only once

```bash
sudo snap install docker
```

## venv setup

Do it only once

```bash
pipenv install

# Check default encoding is UTF-8
pipenv run python
>>> import locale
>>> locale.getpreferredencoding()
'UTF-8'
```

## Serve DB

```bash
cd ccfatigue/DB
docker-compose up
```

## Create schema + initialize DB with alembic

```bash
make alembic-upgrade
```

## Serve web backend on localhost

```bash
make run
```
