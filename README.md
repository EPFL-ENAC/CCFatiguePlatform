# CCFatigue Platform

CCFatiguePlatform is an initiative from CCLab that aims to develop a web application to faciltate manipulation and harmonized storage of composite materials testing datasets.

[Demo](https://ccfatigue-test.epfl.ch/)

# Web app: Usage

## Setup & run locally (without CCFatigue modules)

1. run preprocessing according to [Preprocessing/README.md](Preprocessing/README.md)
2. run backend according to [backend/README.md](backend/README.md)
3. run frontend according to [frontend/README.md](frontend/README.md)

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

## Server provisioning

See https://github.com/EPFL-ENAC/SB_Sysadmin/tree/enacvm0056

# Web app structure

<img src="images/flowchart_CCFATIGUE.jpg" style="width: 650px; max-width: 100%; height: auto" title="Click to enlarge picture" />

# License

TBD.

# Contributors

Charlotte Weil, Samuel Bancal, David Tang, Scott M. Salmon, Anastasios Vassilopoulos, Shayan Khalooei
