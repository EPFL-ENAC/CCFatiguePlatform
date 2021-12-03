#from Integration.data_quality import control_quality
import time
#from init_db import init#, bulkInsertExperiment, bulkInsertTest, bulkInsertTestResult
from read_data import preprocess_data
from sqlalchemy import Column, Integer, String, create_engine, ForeignKey
from sqlalchemy import Float, Date, Enum, Boolean
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import scoped_session, sessionmaker, relationship, validates

import base

import model.test
import model.experiment
import model.test_results

from init_db import init

#Base = declarative_base()
DBSession = scoped_session(sessionmaker())
engine = None

# Initialize the DB, create class (create table in SQL DB).
# Please replace the name and password corresponding to the DB to connect to.
DB_Name = "postgresql://postgres:197355cC?@localhost:5432/ENAC_Exo"
start_time = time.time()
init(DB_Name,DBSession)


print("init phase : " + "--- %s seconds ---" % (time.time() - start_time))
start_time = time.time()
# Read the data based on a specific folder. returns the extracted data from files
folder = "../../Data"
preprocessed = preprocess_data(folder)
print("read_data phase : " + "--- %s seconds ---" % (time.time() - start_time))
start_time = time.time()
# Process and insert data in the DB
#processed = control_quality(preprocessed)
print("quality_control phase : " + "--- %s seconds ---" % (time.time() - start_time))
start_time = time.time()
#bulkInsertExperiment(processed[0])
print("Experiment insertion phase : " + "--- %s seconds ---" % (time.time() - start_time))
start_time = time.time()
#bulkInsertTest(processed[1])
print("Test phase : " + "--- %s seconds ---" % (time.time() - start_time))
start_time = time.time()
#bulkInsertTestResult(processed[2])
print("Test_results phase : " + "--- %s seconds ---" % (time.time() - start_time))


# Pull & Visualize data separately by launching the visualisation.py
#visualisation.Graphs()
