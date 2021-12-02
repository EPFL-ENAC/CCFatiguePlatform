
# This pipeline will remove the entries for which the mandatory data is absent
from math import isnan


def verify_experiment(experiments):
    # Mark the number of entries that are there before processing
    missing_values_experiments = []
    print("PRE exp length :  " + str(len(experiments)))
    # Iteration for the experiment
    for exp in experiments:
        # Iteration on the column
        for c in exp.__table__.c:
            # Name of the column
            name = c.name
            # Value of the column
            value = getattr(exp, name)
            if not (name == 'id'):
                # Filtering if the value is missing or False IF its a mandatory field
                if (value is None) and c.nullable == False:
                    # Put the value in the missing data file
                    print("Missing value !")
                    missing_values_experiments.append(exp)
                    # If a value is missing, the whole row is removed
                    break
    # Remove the missing data
    experiments = list(set(experiments)-set(missing_values_experiments))
    print("POST Bad exp length :  " + str(len(missing_values_experiments)))
    print("POST Good exp remaining :  " + str(len(experiments)))
    return experiments

def verify_test(tests):
    # Iteration on the list of tests
    missing_values_tests = []
    print("PRE tests length :  " + str(len(tests)))
    for tst in tests:
        # Iteration on the columns
        for c in tst.__table__.c:
            name = c.name
            value = getattr(tst, name)
            if not (name == 'id' or name == 'parent_id'):
                # Filtering if the value is missing or False IF its a mandatory field
                if (value is None) and c.nullable == False:
                    # Put the value in the missing data file
                    print("Missing value !")
                    missing_values_tests.append(tst)
                    # If a value is missing, the whole row is removed
                    break
    tests = list(set(tests)-set(missing_values_tests))
    print("POST Bad test length :  " + str(len(missing_values_tests)))
    print("POST Good test remaining :  " + str(len(tests)))
    return tests

def verify_test_results(test_results_list):
    # Iteration on the list of test results
    missing_values_test_results = []
    print("PRE test_results length :  " + str(len(test_results_list)))
    for tstdata in test_results_list:
        # Iteration on the columns
        for c in tstdata.__table__.c:
            name = c.name
            value = getattr(tstdata, name)
            # Iteration on the column
            if not (name == 'id' or name == 'parent_id'):
                # Filtering if the value is missing or False IF its a mandatory field
                if (value is None) and c.nullable == False:
                    # Put the value in the missing data file
                    print("Missing value !")
                    missing_values_test_results.append(tstdata)
                    # If a value is missing, the whole row is removed
                    break
    test_results_list = list(set(test_results_list)-set(missing_values_test_results))
    # Counting of the Good and missing data
    print("POST Bad test_result length :  " + str(len(missing_values_test_results)))
    print("POST Good test_result remaining :  " + str(len(test_results_list)))
    return test_results_list

