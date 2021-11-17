# A) DB backend (PostgreSQL)

## Install

Do it only once

```bash
sudo snap install docker
```

## Serve DB

```bash
cd ccfatigue/DB
docker-compose up
```

## Initialize

TODO



# B) Web backend

## Env install

Do it only once

```bash
pipenv install

# Check default encoding is UTF-8
pipenv run python
>>> import locale
>>> locale.getpreferredencoding()
'UTF-8'
```

## Serve on localhost

Do it each time you open the terminal that will serve

```bash
make run
```
