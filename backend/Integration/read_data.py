import numpy
import os
import pandas

from psycopg2.extensions import register_adapter, AsIs


def addapt_numpy_float64(numpy_float64):
    return AsIs(numpy_float64)


def addapt_numpy_int64(numpy_int64):
    return AsIs(numpy_int64)


register_adapter(numpy.float64, addapt_numpy_float64)
register_adapter(numpy.int64, addapt_numpy_int64)

from init_db import Experiment, Test, Test_results

# Global variables for data manipulation

experiments = []
tests = []
test_results_list = []
tests_local = []
tests_local_len = int()


# Take the Excel XLS metadata file, extract the experiment/test data, and put it in the above array
# As input, the id of experiment exp_id
def preprocess_metadata(metadata_file, exp_id):
    # Reinitiate the local test list
    tests_local.clear()

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
    # Flag the missing value that appear as NaN on Enum types and Int
    experiment_meta_data['Experiment Type'] = experiment_meta_data['Experiment Type'].fillna('NO_VALUE')
    experiment_meta_data['Fracture Mode'] = experiment_meta_data['Fracture Mode'].fillna('NO_VALUE')
    experiment_meta_data['Fatigue Test Type'] = experiment_meta_data['Fatigue Test Type'].fillna('NO_VALUE')
    experiment_meta_data['Control mode'] = experiment_meta_data['Control mode'].fillna('NO_VALUE')
    experiment_meta_data['Subset Size'] = experiment_meta_data['Subset Size'].fillna(0)
    experiment_meta_data['Step Size'] = experiment_meta_data['Step Size'].fillna(0)
    # Flag remaining missing value with 0
    experiment_meta_data = experiment_meta_data.fillna(0)
    # Build experiment table objects
    experiment = Experiment()
    # Fill the table object columns from the Dframes
    experiment.id = exp_id # Come from the function input
    experiment.Laboratory = experiment_meta_data.iloc[0, 0]
    experiment.Researcher = experiment_meta_data.iloc[0, 1]
    experiment.Date = experiment_meta_data.iloc[0, 2]
    experiment.Experiment_Type = experiment_meta_data.iloc[0, 3]
    experiment.Fracture = experiment_meta_data.iloc[0, 4]
    experiment.Fracture_Mode = experiment_meta_data.iloc[0, 5]
    experiment.Initial_Crack_length = experiment_meta_data.iloc[0, 6]
    experiment.Fatigue_Test_Type = experiment_meta_data.iloc[0, 7]
    experiment.Measuring_Equipment = experiment_meta_data.iloc[0, 8]
    experiment.Reliability_Level = experiment_meta_data.iloc[0, 9]
    experiment.Control_mode = experiment_meta_data.iloc[0, 10]
    experiment.Title = experiment_meta_data.iloc[0, 11]
    experiment.Author = experiment_meta_data.iloc[0, 12]
    experiment.Year = experiment_meta_data.iloc[0, 13]
    experiment.DOI = experiment_meta_data.iloc[0, 14]
    experiment.Images_Repository = experiment_meta_data.iloc[0, 15]
    experiment.Fiber_Material = experiment_meta_data.iloc[0, 16]
    experiment.Fiber_Geometry = experiment_meta_data.iloc[0, 17]
    experiment.Area_Density = experiment_meta_data.iloc[0, 18]
    experiment.Resin = experiment_meta_data.iloc[0, 19]
    experiment.Hardener = experiment_meta_data.iloc[0, 20]
    experiment.Mixing_ratio = experiment_meta_data.iloc[0, 21]
    experiment.Length = experiment_meta_data.iloc[0, 22]
    experiment.Width = experiment_meta_data.iloc[0, 23]
    experiment.Thickness = experiment_meta_data.iloc[0, 24]
    experiment.Curing_Time = experiment_meta_data.iloc[0, 25]
    experiment.Curing_Temperature = experiment_meta_data.iloc[0, 26]
    experiment.Curing_Pressure = experiment_meta_data.iloc[0, 27]
    experiment.Fiber_Content = experiment_meta_data.iloc[0, 28]
    experiment.Stacking_Sequence = experiment_meta_data.iloc[0, 29]
    experiment.Temperature = experiment_meta_data.iloc[0, 30]
    experiment.Humidity = experiment_meta_data.iloc[0, 31]
    experiment.Subset_Size = experiment_meta_data.iloc[0, 32]
    experiment.Step_Size = experiment_meta_data.iloc[0, 33]
    # Add to experiment list
    experiments.append(experiment)

    ## SHEET 2 ##
    # Read excel file with Pandas sheet "tests" The types are already OK
    test_meta_data = pandas.read_excel(metadata_file, 'Tests')
    # Transform the text false/true in real boolean
    test_meta_data['Run-out '] = test_meta_data['Run-out '].map(dict(Yes=True, No=False))
    # Management of the missing data
    test_meta_data['Specimen number'] = test_meta_data['Specimen number'].fillna('')
    # Iterate over the length of test, which correspond to the number of tests
    for i in range(len(test_meta_data)):
        # Build test objects
        test = Test()
        # Increment the test id
        if (i == 0 and exp_id == 1):
            test.id = 1
        else:
            test.id = len(tests) + 1
        test.parent_id = experiment.id
        test.Specimen_number = test_meta_data.iloc[i, 0]
        test.Stress_Ratio = test_meta_data.iloc[i, 1]
        test.Maximum_Stress = test_meta_data.iloc[i, 2]
        test.Loading_rate = test_meta_data.iloc[i, 3]
        test.Run_out = test_meta_data.iloc[i, 4]
        # Add to test list above
        tests.append(test)
        tests_local.append(test)


# Extract data from CSV data file
def preprocess_file(data_file, file):
    # Initialisation of a variable
    test_parent = Test_results()
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
    test_results_data['Machine_N_cycles'] = test_results_data['Machine_N_cycles'].fillna('0')
    test_results_data['index'] = test_results_data['index'].fillna('0')
    test_results_data['Camera_N_cycles'] = test_results_data['Camera_N_cycles'].fillna('0')
    test_results_data['Th_time'] = test_results_data['Th_time'].fillna('0')
    test_results_data['Th_N cycles'] = test_results_data['Th_N_cycles'].fillna('0')
    test_results_data['Th_specimen max'] = test_results_data['Th_specimen_max'].fillna('0')
    test_results_data['Th_specimen mean'] = test_results_data['Th_specimen_mean'].fillna('0')
    test_results_data['Th_chamber'] = test_results_data['Th_chamber'].fillna('0')
    test_results_data['Th_uppergrips'] = test_results_data['Th_uppergrips'].fillna('0')
    test_results_data['Th_lowergrips'] = test_results_data['Th_lowergrips'].fillna('0')
    test_results_data = test_results_data.fillna('0')
    # Getting the test number from the file name
    test_number = int(file.split('_')[-1].split('.')[0])
    # Look for matching test number in test_local given the history of tests
    for test in tests_local:
        if (test.id - tests_local_len) == test_number:
            test_parent = test
    # Iterate over the length of the file
    for i in range(len(test_results_data)):
        # Building object
        test_results = Test_results()
        # Fill the table class from the dataframes
        test_results.parent_id = test_parent.id
        test_results.Machine_N_cycles = test_results_data.iloc[i, 0]
        test_results.Machine_Load = test_results_data.iloc[i, 1]
        test_results.Machine_Displacement = test_results_data.iloc[i, 2]
        test_results.index = test_results_data.iloc[i, 3]
        test_results.Camera_N_cycles = test_results_data.iloc[i, 4]
        test_results.exx = test_results_data.iloc[i, 5]
        test_results.eyy = test_results_data.iloc[i, 6]
        test_results.exy = test_results_data.iloc[i, 7]
        test_results.crack_length = test_results_data.iloc[i, 8]
        test_results.Th_time = test_results_data.iloc[i, 9]
        test_results.Th_N_cycles = test_results_data.iloc[i, 10]
        test_results.Th_specimen_max = test_results_data.iloc[i, 11]
        test_results.Th_specimen_mean = test_results_data.iloc[i, 12]
        test_results.Th_chamber = test_results_data.iloc[i, 13]
        test_results.Th_uppergrips = test_results_data.iloc[i, 14]
        test_results.Th_lowergrips = test_results_data.iloc[i, 15]
        # Add to list above
        test_results_list.append(test_results)


# Go through folder hierarchy and analyse files
def preprocess_data(folder_name):
    # Initialize the experiment counter
    expnumber = int(0)
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
                            preprocess_metadata(meta_data_file, expnumber)
                        # Then its CSV file
                        else:
                            # File is the test name
                            test_name = file
                            # Result file

                            print('File :  ' + file)
                            result_file = folder_name + "/" + folder + "/" + file
                            # To avoid other files suffix
                            if not file.split('_')[3].split('.')[0] == 'metadata':
                                preprocess_file(result_file, file)
                                tests_local_len = len(tests_local)
                                # For debug purpose, to process only 1 file
                            else:
                                print('File skipped')

    return [experiments, tests, test_results_list]
