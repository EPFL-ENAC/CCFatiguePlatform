import logging

import numpy
import os

import numpy as np
import pandas

from psycopg2.extensions import register_adapter, AsIs


def addapt_numpy_float64(numpy_float64):
    return AsIs(numpy_float64)


def addapt_numpy_int64(numpy_int64):
    return AsIs(numpy_int64)

from model.init_db import Experiment,Test,Test_results

register_adapter(numpy.float64, addapt_numpy_float64)
register_adapter(numpy.int64, addapt_numpy_int64)

# Take the Excel XLS metadata file, extract the experiment/test data, and put it in the above array
# As input, the id of experiment exp_id
def preprocess_metadata(metadata_file, exp_id, file,testlength):
    """
    From a EXCEL data file, read each sheet with Pandas and populate a list of Test and Experiment object

            Parameters:
                    metadata_file (str): the file name with full path
                    exp_id (int): the id number of the experiment, coming from the folder iteration
                    file (str): the file name
                    testlength (int): the total length of the Test list, used to attributes id

            Returns:
                    tests (list of Test_results): A list of object Test extracted from the file.
                    tests_local (list of Test): A list of object Test for indexing the file regarding the folder's hierarchy.
                    experiment (Experiment): A list of object Experiment extracted from the file.
    """
    ## SHEET 1 ##
    # Read the excel with pandas first sheet and defining data types
    experiment_meta_data = pandas.read_excel(metadata_file, 'Experiment', header=1,
                                             converters={'Date': pandas.to_datetime},
                                             dtype={
                                                 'Laboratory': str,
                                                 'Researcher': str,
                                                 'Experiment Type': str,
                                                 'Fracture': bool,
                                                 'Fracture Mode': str,
                                                 'Initial Crack length': float,
                                                 'Fatigue Test Type': str,
                                                 'Measuring Equipment': str,
                                                 'Reliability Level': str,
                                                 'Control mode': str,
                                                 'Title': str,
                                                 'Author': str,
                                                 'Year': str,
                                                 'DOI': str,
                                                 'Images Repository': str,
                                                 'Fiber Material': str,
                                                 'Fiber Geometry': str,
                                                 'Area Density': float,
                                                 'Resin': str,
                                                 'Hardener': str,
                                                 'Mixing ratio': str,
                                                 'Length': float,
                                                 'Width': float,
                                                 'Thickness': float,
                                                 'Curing Time': float,
                                                 'Curing Temperature': float,
                                                 'Curing Pressure': float,
                                                 'Fiber Content': float,
                                                 'Stacking Sequence': str,
                                                 'Temperature': float,
                                                 'Humidity': float,
                                                 'Subset Size': str,
                                                 'Step Size': str,
                                             })
    # Flag the missing value that appear as NaN
    experiment_meta_data = experiment_meta_data.fillna(np.nan).replace([np.nan], [None])
    # Build experiment table objects
    experiment = Experiment()
    experiment.__name__ = file
    # Fill the table object columns from the Dframes
    experiment.id = exp_id # Come from the function input
    for c in experiment.__table__.c:
        name = c.name
        if not (name == 'id' or name == 'parent_id'):
            setattr(experiment,name,experiment_meta_data.loc[0,name.replace('_',' ')])

    ## SHEET 2 ##
    # Read excel file with Pandas sheet "tests" The types are already OK
    test_meta_data = pandas.read_excel(metadata_file, 'Tests')
    # Transform the text false/true in real boolean
    test_meta_data['Run-out '] = test_meta_data['Run-out '].map(dict(Yes=True, No=False))
    # Management of the missing data
    #test_meta_data['Specimen number'] = test_meta_data['Specimen number'].fillna('')
    test_meta_data = test_meta_data.fillna(np.nan).replace([np.nan], [None])
    tests = []
    # Iterate over the length of test, which correspond to the number of tests in the folder
    tests_local = []
    for i in range(len(test_meta_data)):
        # Build test objects
        test = Test()
        # Increment the test id
        if i == 0 and exp_id == 1:
            test.id = 1 # First test object ever
        else:
            test.id = testlength + len(tests) + 1 # We assume the specimen order match the row order in the file

        test.parent_id = experiment.id # We link to the corresponding parent experiment
        for c in test.__table__.c:
            name = c.name
            if not (name == 'id' or name == 'parent_id' or name == 'Run_out'):
                setattr(test, name, test_meta_data.loc[i, name.replace('_', ' ')])
            if name == 'Run_out':
                setattr(test,'Run_out', test_meta_data.loc[i, 'Run-out '])

        # Add to test list above
        tests.append(test)
        tests_local.append(test)
    return [tests,tests_local,experiment]


# Extract data from CSV data file
def preprocess_file(data_file, file, tests_local):
    """
    From a CSV data file, read with Pandas and populate a list of Test_results object

            Parameters:
                    data_file (str): the file name with full path
                    file (str): the file name
                    tests_local (list of Test): A list of object Test for indexing the file regarding the folder's hierarchy.

            Returns:
                    test_results_list (list of Test_results): A list of object Test_results extracted from the file.
    """
    # Initialisation of a variable
    test_parent = Test_results()
    test_results_list = []
    # Reading the CSV and specifying variables
    test_results_data = pandas.read_csv(data_file,
                                        dtype={
                                            'Machine_N_cycles': str,
                                            'Machine_Load': float,
                                            'Machine_Displacement': float,
                                            'index': str,
                                            'Camera_N_cycles': str,
                                            'exx': float,
                                            'eyy': float,
                                            'exy': float,
                                            'crack_length': float,
                                            'Th_time': str,
                                            'Th_N_cycles': str,
                                            'Th_specimen_max': str,
                                            'Th_specimen_mean': str,
                                            'Th_chamber': str,
                                            'Th_uppergrips': str,
                                            'Th_lowergrips': str,
                                        })
    # Managing missing data that appear as NaN
    test_results_data = test_results_data.fillna(np.nan).replace([np.nan], [None])
    # Getting the test number from the file name
    test_number = int(file.split('_')[-1].split('.')[0])
    # Look for matching folder number given the specimen number
    for test in tests_local:
        if test.Specimen_number == test_number:
            test_parent = test
            break
    # Iterate over the length of the file
    for i in range(len(test_results_data)):
        # Building object
        test_results = Test_results()
        # Fill the table class from the dataframes
        test_results.parent_id = test_parent.id
        for c in test_results.__table__.c:
            name = c.name
            if not (name == 'id' or name == 'parent_id'):
                setattr(test_results, name, test_results_data.loc[0, name])
        # Add to list above
        test_results_list.append(test_results)
    return test_results_list

# Go through folder hierarchy and analyse files
def preprocess_data(folder_name):
    """
    Crawl into the folder hierarchy to extract data

            Parameters:
                    folder_name (str): name of the folder to look into

            Returns:
                    experiments (list of Experiment): list of Experiment objects
                    tests (list of Test): list of Test objects
                    test_results_list (list of Test_results): list of Test_results objects
    """
    # Initialize the experiment counter
    expnumber = int(0)
    experiments = []
    tests = []
    test_results_list = []
    # Iteration through first level folders ( ../Data/.. )
    for root, sub_folders, files in os.walk(folder_name):
        for folder in sub_folders:

            logging.info('Folder :  ' + folder)

            # Getting the folder as experiment name
            experiment_name = folder

            # Iteration through second level folders ( ../Data/experiment/.. )
            for root, sub_folders, files in os.walk(folder_name + '/' + folder):
                files.sort(key=lambda f: os.path.splitext(f), reverse=True)
                for file in files:
                    # Metadata file
                    # Make sure you read the xls file before the results files
                    if not '.json' in file:
                        if '.xls' in file:
                            expnumber = expnumber + 1
                            meta_data_file = folder_name + "/" + folder + "/" + file
                            logging.info('Meta_data_file :  ' + meta_data_file)
                            preprocessed_metadata = preprocess_metadata(meta_data_file, expnumber, file,len(tests))
                            experiments.append(preprocessed_metadata[2])
                            tests.extend(preprocessed_metadata[0])
                        # Then its CSV file
                        else:
                            # File is the test name
                            test_name = file
                            # Result file

                            logging.info('File :  ' + file)
                            result_file = folder_name + "/" + folder + "/" + file
                            # To avoid other files suffix
                            if not file.split('_')[3].split('.')[0] == 'metadata':
                                preprocess_data = preprocess_file(result_file, file,preprocessed_metadata[1])
                                test_results_list.extend(preprocess_data)
                                #break # For debug purpose, to process only 1 file
                            else:
                                logging.info('File skipped')

    return [experiments, tests, test_results_list]

