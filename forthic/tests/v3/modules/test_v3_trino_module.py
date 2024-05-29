import unittest
from forthic.v3.interpreter import Interpreter
from forthic.v3.modules.trino_module import TrinoModule
from tests.tests_py.v3.modules.trino_context import TrinoTestContext


def get_interp():
    interp = Interpreter()
    interp.register_module(TrinoModule)

    # Set up Trino test context
    interp.run("['trino'] USE-MODULES")
    interp.stack_push(TrinoTestContext())
    interp.run("trino.PUSH-CONTEXT!")
    return interp


class TestTrinoModule(unittest.TestCase):
    def setUp(self):
        self.interp = get_interp()

    def test_QUERY(self):
        self.interp.run("""
            'select * from metric_ds.dim_country_tests' trino.QUERY
        """)
        data = self.interp.stack[0]
        self.assertEqual('alpha', data['test_key']['0'])
        self.assertEqual('Tanzania', data['country']['0'])


if __name__ == '__main__':
    unittest.main()