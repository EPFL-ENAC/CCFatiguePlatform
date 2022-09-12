# DB backend

Everything needed to install and run the backend is documented in the [main README.md](../README.md)

## Generate a fresh/new alembig revision

```bash
poetry run dotenv -f ../secrets/.env run alembic revision --autogenerate -m "initial_tables"
```
