# DB backend

Everything needed to install and run the backend is documented in the [main README.md](../README.md)

## Generate a fresh/new alembig revision

```bash
DOTENV_PATH_FOR_DYNACONF="../secrets/.env" poetry run alembic revision --autogenerate -m "initial_tables"
```
