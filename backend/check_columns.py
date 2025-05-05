from sqlalchemy import create_engine, inspect
from ccfatigue.services.database import sync_url

engine = create_engine(sync_url)
inspector = inspect(engine)

columns = inspector.get_columns("experiment")
print("Colonne presenti nella tabella 'experiment':")
for col in columns:
    print(f"- {col['name']}")
