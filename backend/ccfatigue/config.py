"""
Dynaconf Settings
"""
from dynaconf import Dynaconf, Validator

settings = Dynaconf(
    envvar_prefix=False,
    load_dotenv=True,
    settings_files=[],
    validators=[
        Validator("cors_enabled", default=False),
        Validator("root_path", default=""),
        Validator("postgres_host", default="localhost"),
        Validator("postgres_port", default=5432),
        Validator("postgres_user", default="ccfatigue"),
        Validator("postgres_password", must_exist=True),
        Validator("postgres_db", default="ccfatigue"),
        Validator("data_path", default="../Data"),

        # Email settings (new)
        Validator("mail_from", must_exist=True),
        Validator("mail_password", must_exist=True),
        Validator("mail_server", default="smtp.gmail.com"),
        Validator("mail_port", default=587),
        Validator("mail_username", must_exist=True),
        Validator("mail_tls", default=True),
        Validator("mail_ssl", default=False),
        Validator("recipient_email", must_exist=True),
    ],
)

# `envvar_prefix` = export envvars with `export DYNACONF_FOO=bar`.
# `settings_files` = Load these files in the order.
