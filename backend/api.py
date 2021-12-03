import argparse
import json
import os

from fastapi.openapi.utils import get_openapi

from ccfatigue import __name__, __version__
from ccfatigue.main import app

parser = argparse.ArgumentParser(description=__name__)
parser.add_argument("output", help="OpenAPI output file")
args = parser.parse_args()
output: str = args.output

openapi_schema = get_openapi(title=__name__, version=__version__, routes=app.routes)
with open(output, "w") as file:
    print(f"write to {os.path.abspath(output)}")
    json.dump(openapi_schema, file, indent=2)
