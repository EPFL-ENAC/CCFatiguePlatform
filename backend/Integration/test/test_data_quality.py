import unittest
import sys
sys.path.append("..")
from Integration.model.init_db import Test
from Integration.data_quality import verify_test

class TestStringMethods(unittest.TestCase):

    def test_verify_test(self):
        # Generate mock experiments
        tst1 = Test()
        tst2 = Test()
        tst3 = Test()
        tst4 = Test()
        # Filling the good data
        tst1.Stress_Ratio = 1
        tst1.Specimen_number = 0.2
        tst1.Maximum_Stress = 60
        tst1.Loading_rate = 100
        tst1.Run_out = False

        tst2.Stress_Ratio = 1
        tst2.Specimen_number = 0.2
        tst2.Maximum_Stress = None
        tst2.Loading_rate = None
        tst2.Run_out = True

        # Filling bad data

        tst3.Stress_Ratio = None
        tst3.Specimen_number = 0.8
        tst3.Maximum_Stress = 78
        tst3.Loading_rate = 10
        tst3.Run_out = True

        tst4.Stress_Ratio = 2
        tst4.Specimen_number = 10
        tst4.Maximum_Stress = 60
        tst4.Loading_rate = 1000
        tst4.Run_out = False

        # call the function and verify
        self.assertEqual(2,len(verify_test(verify_test([tst1,tst2,tst3,tst4]))))

if __name__ == '__main__':
    unittest.main()