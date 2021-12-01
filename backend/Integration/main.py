from Integration.data_quality import control_quality
from init_db import init, bulkInsertExperiment, bulkInsertTest, bulkInsertTestResult
from read_data import preprocess_data

# Initialize the DB, create class (create table in SQL DB).
# Please replace the name and password corresponding to the DB to connect to.
DB_Name = "postgresql://postgres:197355cC?@localhost:5432/ENAC_Exo"
init(DB_Name)

# Read the data based on a specific folder. returns the extracted data from files
folder = "../../Data"
preprocessed = preprocess_data(folder)

# Process and insert data in the DB
processed = control_quality(preprocessed)
bulkInsertExperiment(processed[0])
bulkInsertTestResult(processed[1])
bulkInsertTest(processed[2])

# Pull & Visualize data separately by launching the visualisation.py
#visualisation.Graphs()
