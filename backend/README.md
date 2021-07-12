# Web backend

## Env setup

```bash
pipenv install

# Check default encoding is UTF-8
pipenv run python
>>> import locale
>>> locale.getpreferredencoding()
'UTF-8'
```

## Copy data on localhost

Copy to data/CCLAB :
GDrive -> CCLAB - Fatigue Data Platform > FatigueDataPlatform files & data > Data Description > File directory example > CCLAB


## Serve on localhost

```bash
pipenv run uvicorn main:app --reload
```
