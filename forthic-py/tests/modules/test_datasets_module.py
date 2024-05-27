import os
import json
import unittest

from forthic.v3.interpreter import Interpreter
from forthic.v3.modules.datasets_module import DatasetsModule


def get_data_dir():
    return f"{os.getcwd()}/tests/tests_py/v3/modules/datasets_data"


def get_dataset_file(dataset_name):
    return f"{get_data_dir()}/datasets/{dataset_name}.dataset"


def load_dataset(dataset_name):
    with open(get_dataset_file(dataset_name)) as f:
        result = json.loads(f.read())
    return result


def clear_dataset(dataset_name):
    dataset_file = get_dataset_file(dataset_name)
    if os.path.isfile(dataset_file):
        os.remove(dataset_file)


def get_interp():
    result = Interpreter()
    result.register_module(DatasetsModule)
    result.run(f"['datasets'] USE-MODULES '{get_data_dir()}' datasets.CWD!")
    return result


class TestDatasetsModule(unittest.TestCase):
    def setUp(self):
        clear_dataset("greek")
        self.interp = get_interp()

    def test_DATASET_bang(self):
        # Test: Store data
        dataset = {"alpha": [1, 2, 3], "beta": [4, 5, 6]}
        self.interp.stack_push(dataset)
        self.interp.run("'greek' datasets.DATASET!")

        loaded_data = load_dataset("greek")
        self.assertDictEqual(dataset, loaded_data)

        # Test: Add data to existing dataset
        dataset = {"gamma": [7, 8, 9]}
        self.interp.stack_push(dataset)
        self.interp.run("'greek' datasets.DATASET!")
        loaded_data = load_dataset("greek")
        modified_dataset = {"alpha": [1, 2, 3], "beta": [4, 5, 6], "gamma": [7, 8, 9]}
        self.assertDictEqual(modified_dataset, loaded_data)

        # Test: Ovewrite existing dataset
        dataset = {"delta": [10, 11, 12]}
        self.interp.stack_push(dataset)
        self.interp.run("'greek' datasets.!OVERWRITE datasets.DATASET!")
        loaded_data = load_dataset("greek")
        new_dataset = {"delta": [10, 11, 12]}
        self.assertDictEqual(new_dataset, loaded_data)

    def test_DATASET(self):
        # Store dataset
        dataset = {"alpha": [1, 2, 3], "beta": [4, 5, 6]}
        self.interp.stack_push(dataset)
        self.interp.run("'greek' datasets.DATASET!")

        # Get dataset
        self.interp.run("'greek' datasets.DATASET")
        self.assertDictEqual(dataset, self.interp.stack[0])

    def test_RECORDS(self):
        # Store dataset
        dataset = {"alpha": [1, 2, 3], "beta": [4, 5, 6]}
        self.interp.stack_push(dataset)
        self.interp.run("'greek' datasets.DATASET!")

        # Get records
        self.interp.run("'greek' ['beta' 'alpha'] datasets.RECORDS")
        self.assertEqual([[4, 5, 6], [1, 2, 3]], self.interp.stack[-1])

        # Get records with NULLs for missing keys
        self.interp.run("'greek' ['beta' 'MISSING' 'alpha'] datasets.RECORDS")
        self.assertEqual([[4, 5, 6], None, [1, 2, 3]], self.interp.stack[-1])

        # Get records dropping NULLs for missing keys
        self.interp.run(
            "'greek' ['beta' 'MISSING' 'alpha'] datasets.!DROP-NULLS datasets.RECORDS"
        )
        self.assertEqual([[4, 5, 6], [1, 2, 3]], self.interp.stack[-1])


if __name__ == "__main__":
    unittest.main()
