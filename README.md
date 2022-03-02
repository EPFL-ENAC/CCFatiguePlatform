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

# App architecture

<img width="1791" alt="Screenshot 2022-03-02 at 18 37 07" src="https://user-images.githubusercontent.com/18381609/156417130-4ce6f53c-e89b-436c-a30d-ee88a760f8e8.png">


# License

TBD.

# Contributors

Charlotte Weil, Samuel Bancal, David Tang, Scott M. Salmon, Anastasios Vassilopoulos, Shayan Khalooei
