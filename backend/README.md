# DB backend

## Install

Do it only once

```bash
sudo snap install docker
```

## venv setup

Do it only once

```bash
make install

# Check default encoding is UTF-8
poetry run python
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
make init-db
```

or only create/update schema in DB :

```bash
make alembic-upgrade
```

## Serve web backend on localhost

```bash
make run
```
