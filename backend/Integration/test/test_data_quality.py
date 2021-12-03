import unittest
from Integration.model.init_db import Experiment

class TestStringMethods(unittest.TestCase):

    def test_verify_experiment(self):
        # Generate mock experiments
        exp1 = Experiment()
        exp1.Experiment_Type = "invalid type"
        control_quality([exp1])

        self.assertEqual('foo'.upper(), 'FOO')

    def test_verifytest(self):
        self.assertTrue('FOO'.isupper())
        self.assertFalse('Foo'.isupper())

    def test_verifyresult(self):
        s = 'hello world'
        self.assertEqual(s.split(), ['hello', 'world'])
        # check that s.split fails when the separator is not a string
        with self.assertRaises(TypeError):
            s.split(2)

if __name__ == '__main__':
    unittest.main()