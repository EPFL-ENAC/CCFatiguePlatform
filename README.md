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
mkdir -p secrets && cat <<EOF > secrets/.env
POSTGRES_USER=ccfatigue
POSTGRES_PASSWORD=change-it!
EOF
```

## Server provisioning

See https://github.com/EPFL-ENAC/enacit-srv-lin-sysadmin/blob/develop/test/ccfatigue-test/README.md

# App architecture

![flowchart_CCFATIGUE.png](flowchart_CCFATIGUE.png)

# License

MIT

# Contributors

Charlotte Weil, Samuel Bancal, David Tang, Scott M. Salmon, Anastasios Vassilopoulos, Shayan Khalooei
