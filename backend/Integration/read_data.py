import numpy
import os

import numpy as np
import pandas

from psycopg2.extensions import register_adapter, AsIs


def addapt_numpy_float64(numpy_float64):
    return AsIs(numpy_float64)


def addapt_numpy_int64(numpy_int64):
    return AsIs(numpy_int64)


register_adapter(numpy.float64, addapt_numpy_float64)
register_adapter(numpy.int64, addapt_numpy_int64)

from init_db import Experiment, Test, Test_results

# Take the Excel XLS metadata file, extract the experiment/test data, and put it in the above array
# As input, the id of experiment exp_id
def preprocess_metadata(metadata_file, exp_id, file):

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
    for i in range(len(test_meta_data)):
        # Build test objects
        test = Test()
        tests_local = []
        # Increment the test id
        if i == 0 and exp_id == 1:
            test.id = 1 # First test object ever
        else:
            test.id = len(tests) + 1 # We assume the specimen order match the row order in the file

        test.parent_id = experiment.id # We link to the corresponding parent experiment
        for c in test.__table__.c:
            name = c.name
            if not (name == 'id' or name == 'parent_id' or name == 'Run_out'):
                setattr(test, name, test_meta_data.loc[i, name.replace('_', ' ')])
        setattr(test, 'Run_out', test_meta_data.loc[i, 'Run-out'])

        # Add to test list above
        tests.append(test)
        tests_local.append(test)
    return [tests,tests_local,experiment]


# Extract data from CSV data file
def preprocess_file(data_file, file, tests_local):
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
                setattr(test_results, name, test_results_data.loc[0, name.replace('_', ' ')])
        # Add to list above
        test_results_list.append(test_results)
    return [test_results_list]

# Go through folder hierarchy and analyse files
def preprocess_data(folder_name):
    # Initialize the experiment counter
    expnumber = int(0)
    experiments = []
    tests = []
    test_results_list = []
    # Iteration through first level folders ( ../Data/.. )
    for root, sub_folders, files in os.walk(folder_name):
        for folder in sub_folders:

            print('Folder :  ' + folder)

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
                            print('Meta_data_file :  ' + meta_data_file)
                            preprocessed_metadata = preprocess_metadata(meta_data_file, expnumber, file)
                            experiments.append(preprocessed_metadata[2])
                            tests.append(preprocessed_metadata[0])
                        # Then its CSV file
                        else:
                            # File is the test name
                            test_name = file
                            # Result file

                            print('File :  ' + file)
                            result_file = folder_name + "/" + folder + "/" + file
                            # To avoid other files suffix
                            if not file.split('_')[3].split('.')[0] == 'metadata':
                                preprocess_data = preprocess_file(result_file, file)
                                #tests_local_len = len(preprocessed_metadata[1])
                                test_results_list.append([preprocess_data])
                                #break # For debug purpose, to process only 1 file
                            else:
                                print('File skipped')

    return [experiments, tests, test_results_list]

#folder = "../../Data"
#preprocess_data(folder)