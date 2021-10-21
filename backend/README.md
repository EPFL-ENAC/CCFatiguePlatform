# Copy data to localhost

Do it only once

Copy to /data/CCLAB :
GDrive -> CCLAB - Fatigue Data Platform > FatigueDataPlatform files & data > Data Description > File directory example > CCLAB



# DB backend (MongoDB)

## Install

Do it only once

```bash
sudo snap install docker
```

## Serve

```bash
cd DB
docker-compose up
```

## Initialize

```bash
pipenv run python3 ./DB/db_initialize.py
```


# Web backend

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
pipenv shell
export PYTHONPATH=../Fatigue_test_dashboard/:$PYTHONPATH
```

then simply

```bash
uvicorn main:app --reload
```
