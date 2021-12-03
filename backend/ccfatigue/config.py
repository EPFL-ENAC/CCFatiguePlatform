"""
Dynaconf Settings
"""
from dynaconf import Dynaconf, Validator

settings = Dynaconf(
    envvar_prefix=False,
    settings_files=[
        "ccfatigue/settings.toml",
        "ccfatigue/.secrets.toml",
    ],
    validators=[
        Validator("cors_enabled", default=False),
        Validator("root_path", default=""),
        Validator("postgres_host", default="localhost"),
        Validator("postgres_port", default=5432),
        Validator("postgres_user", default="ccfatigue"),
        Validator("postgres_password", must_exist=True),
        Validator("postgres_db", default="ccfatigue"),
    ],
)

# `envvar_prefix` = export envvars with `export DYNACONF_FOO=bar`.
# `settings_files` = Load these files in the order.
