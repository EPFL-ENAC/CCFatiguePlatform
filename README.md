# CCFatigue Platform

CCFatiguePlatform is an initiative from CCLab that aims to develop a web application to faciltate manipulation and harmonized storage of composite materials testing datasets.

[Demo](https://ccfatigue-test.epfl.ch/)

# Web app: Usage

## Secrets

```bash
mkdir -p backend/ccfatigue/DB/secrets && cat <<EOF > backend/ccfatigue/DB/secrets/database.env
POSTGRES_USER=ccfatigue
POSTGRES_PASSWORD=change-it!
EOF
cat <<EOF > backend/ccfatigue/.secrets.toml
postgres_password = 'change-it!'
EOF
```

## First time setup

```bash
make install
```

## Run Preprocessing

```bash
make run-preprocessing
```

## Run Backend

```bash
make run-db
make run-backend
```

First time backend setup/init :

```bash
cd backend
make init-db  # 1st time init
make alembic-upgrade  # or only create/update schema in DB
```

## Run Frontend

```bash
make run-frontend
```

## Server provisioning

See https://github.com/EPFL-ENAC/enacit-srv-lin-sysadmin/blob/develop/test/ccfatigue-test/README.md

# App architecture

![flowchart_CCFATIGUE.png](flowchart_CCFATIGUE.png)

# License

MIT

# Contributors

Charlotte Weil, Samuel Bancal, David Tang, Scott M. Salmon, Anastasios Vassilopoulos, Shayan Khalooei
