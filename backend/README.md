# DB backend

## Install

Prerequisite

- Docker

```bash
make install

# Check default encoding is UTF-8 (for Windows only)
poetry run python
>>> import locale
>>> locale.getpreferredencoding()
'UTF-8'
```

## Serve DB

```bash
make run-db
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
