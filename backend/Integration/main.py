import logging

import visualisation
from data_quality import verify_test,verify_test_results,verify_experiment
import time
from model.init_db import init, bulkInsertExperiment, bulkInsertTest, bulkInsertTestResult
from read_data import preprocess_data
from sqlalchemy.orm import scoped_session, sessionmaker
import sys
from model import init_db
"""
This script launches the different steps of the homework:
    - DB creation
    - Data extraction
    - Data processing
    - Data insertion in DB
    - Data retrieving from the DB
    - Visualisation of statistics
"""
# Retrieve the DB connection string passed as argument
DB_Name = sys.argv[1]
logging.info(DB_Name)
#DB_Name = "postgresql://postgres:197355cC?@localhost:5432/ENAC_Exo"
start_time = time.time()
# Initialize the DB, create class (create table in SQL DB).
init(DB_Name)
logging.info("init phase : " + "--- %s seconds ---" % (time.time() - start_time))
start_time = time.time()
# Read the data based on a specific folder.
folder = "../../Data"
# Return extracted data
[experiments, tests, test_results_list] = preprocess_data(folder)
logging.info("read_data phase : " + "--- %s seconds ---" % (time.time() - start_time))
start_time = time.time()
# Process data for missing values
experiments = verify_experiment(experiments)
tests = verify_test(tests)
test_results_list = verify_test_results(test_results_list)
logging.info("quality_control phase : " + "--- %s seconds ---" % (time.time() - start_time))
# Insert data in the DB
start_time = time.time()
bulkInsertExperiment(experiments)
logging.info("Experiment insertion phase : " + "--- %s seconds ---" % (time.time() - start_time))
start_time = time.time()
bulkInsertTest(tests)
logging.info("Test phase : " + "--- %s seconds ---" % (time.time() - start_time))
start_time = time.time()
bulkInsertTestResult(test_results_list)
logging.info("Test_results phase : " + "--- %s seconds ---" % (time.time() - start_time))
# Pull data and save the statistics in a file
visualisation.graphs()
