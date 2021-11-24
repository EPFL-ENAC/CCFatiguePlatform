"""
Dynaconf Settings
"""
from dynaconf import Dynaconf, Validator

settings = Dynaconf(
    envvar_prefix=False,
    settings_files=[
        'ccfatigue/settings.toml',
        'ccfatigue/.secrets.toml',
    ],
    validators=[
        Validator('cors_enabled', default=False),
    ],
)

# `envvar_prefix` = export envvars with `export DYNACONF_FOO=bar`.
# `settings_files` = Load these files in the order.
