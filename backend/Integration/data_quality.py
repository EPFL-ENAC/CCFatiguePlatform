# Bad data container to be written in a file
bad_experiments = []
bad_tests = []
bad_test_results_list = []

bad_exp_id = []
bad_test_id = []

# This pipeline will remove the entries for which the mandatory data is absent
def control_quality(preprocessing):
    experiments = preprocessing[0]
    # Mark the number of entries that are there before processing
    print("PRE exp length :  " + str(len(experiments)))
    tests = preprocessing[1]
    print("PRE tests length :  " + str(len(tests)))
    test_results_list = preprocessing[2]
    print("PRE test_results length :  " + str(len(test_results_list)))
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
                if ((value == '') or (type(value) == bool and value == False)) and c.nullable == False:
                    # Put the value in the bad data file
                    print("Missing value !")
                    bad_experiments.append(exp)
                    # If a value is missing, the whole row is removed
                    break
    # Remove the bad data
    experiments = list(set(experiments)-set(bad_experiments))

    # Iteration on the list of tests
    for tst in tests:
        # Iteration on the columns
        for c in tst.__table__.c:
            name = c.name
            value = getattr(tst, name)
            if not (name == 'id' or name == 'parent_id'):
                # Filtering if the value is missing or False IF its a mandatory field
                if (value is None) and c.nullable == False:
                    # Put the value in the bad data file
                    print("Missing value !")
                    bad_tests.append(tst)
                    # If a value is missing, the whole row is removed
                    break
    tests = list(set(tests)-set(bad_tests))

    # Iteration on the list of test results
    for tstdata in test_results_list:
        # Iteration on the columns
        for c in tstdata.__table__.c:
            name = c.name
            value = getattr(tstdata, name)
            # Iteration on the column
            if not (name == 'id' or name == 'parent_id'):
                # Filtering if the value is missing or False IF its a mandatory field
                if (value is None) and c.nullable == False:
                    # Put the value in the bad data file
                    print("Missing value !")
                    bad_test_results_list.append(tstdata)
                    # If a value is missing, the whole row is removed
                    break
    test_results_list = list(set(test_results_list)-set(bad_test_results_list))
    # Counting of the Good and missing data
    print("POST Bad test_result length :  " + str(len(bad_test_results_list)))
    print("POST Good test_result remaining :  " + str(len(test_results_list)))
    print("POST Bad test length :  " + str(len(bad_tests)))
    print("POST Good test remaining :  " + str(len(tests)))
    print("POST Bad exp length :  " + str(len(bad_experiments)))
    print("POST Good exp remaining :  " + str(len(experiments)))
    return [experiments, tests, test_results_list]

