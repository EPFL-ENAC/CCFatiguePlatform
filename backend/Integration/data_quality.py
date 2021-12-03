
# This pipeline will remove the entries for which the mandatory data is absent
import logging
from math import isnan


def verify_experiment(experiments):
    """
    From ORM objects, check if values that are mandatory are missing, then, remove the corresponding row.

            Parameters:
                    experiments (list of Experiment): A list of Experiment object to check

            Returns:
                    experiments (list of Experiment): A filtered list of Experiment object to check

    """
    # Mark the number of entries that are there before processing
    missing_values_experiments = []
    logging.info("PRE exp length :  " + str(len(experiments)))
    # Iteration for the experiment
    for exp in experiments:
        # Iteration on the column
        for c in exp.__table__.c:
            # Name of the column
            name = c.name
            # Value of the column
            value = getattr(exp, name)
            if not (name == 'id'):
                # Filtering if the value is missing and its a mandatory field
                if (value is None) and c.nullable == False:
                    # Put the value in the missing data file
                    logging.info("Missing value !")
                    missing_values_experiments.append(exp)
                    # If a value is missing, the whole row is removed
                    break
    # Remove the missing data
    experiments = list(set(experiments)-set(missing_values_experiments))
    logging.info("POST missing exp length :  " + str(len(missing_values_experiments)))
    logging.info("POST Good exp remaining :  " + str(len(experiments)))
    return experiments

def verify_test(tests):
    """
    From ORM objects, check if values that are mandatory are missing, then, remove the corresponding row.

            Parameters:
                    tests (list of Test): A list of Test object to check

            Returns:
                    test (list of Test): A filtered list of Test object to check

    """
    # Iteration on the list of tests
    missing_values_tests = []
    logging.info("PRE tests length :  " + str(len(tests)))
    for tst in tests:
        # Iteration on the columns
        for c in tst.__table__.c:
            name = c.name
            value = getattr(tst, name)
            if not (name == 'id' or name == 'parent_id'):
                # Filtering if the value is missing and its a mandatory field
                if (value is None) and c.nullable == False:
                    # Put the value in the missing data file
                    logging.info("Missing value !")
                    missing_values_tests.append(tst)
                    # If a value is missing, the whole row is removed
                    break
    tests = list(set(tests)-set(missing_values_tests))
    logging.info("POST missing test length :  " + str(len(missing_values_tests)))
    logging.info("POST Good test remaining :  " + str(len(tests)))
    return tests

def verify_test_results(test_results_list):
    """
    From ORM objects, check if values that are mandatory are missing, then, remove the corresponding row.

            Parameters:
                    test_results_list (list of Test_results): A list of Test_results object to check

            Returns:
                    test_results_list (list of Test_results): A filtered list of Test_results object to check

    """
    # Iteration on the list of test results
    missing_values_test_results = []
    logging.info("PRE test_results length :  " + str(len(test_results_list)))
    for tstdata in test_results_list:
        # Iteration on the columns
        for c in tstdata.__table__.c:
            name = c.name
            value = getattr(tstdata, name)
            # Iteration on the column
            if not (name == 'id' or name == 'parent_id'):
                # Filtering if the value is missing and its a mandatory field
                if (value is None) and c.nullable == False:
                    # Put the value in the missing data file
                    logging.info("Missing value !")
                    missing_values_test_results.append(tstdata)
                    # If a value is missing, the whole row is removed
                    break
    test_results_list = list(set(test_results_list)-set(missing_values_test_results))
    # Counting of the Good and missing data
    logging.info("POST missing test_result length :  " + str(len(missing_values_test_results)))
    logging.info("POST Good test_result remaining :  " + str(len(test_results_list)))
    return test_results_list

