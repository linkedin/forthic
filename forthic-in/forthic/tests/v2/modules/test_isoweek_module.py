import unittest
from forthic.v2.interpreter import Interpreter
from forthic.v2.modules.isoweek_module import ISOWeekModule
import datetime


def get_interp():
    result = Interpreter()
    result.register_module(ISOWeekModule)
    result.run('["isoweek"] USE-MODULES')
    return result


class TestISOWeekModule(unittest.TestCase):
    def setUp(self):
        self.interp = get_interp()

    def test_DATE_to_WEEK_NUM(self):
        self.interp.run("""
        2022-08-09 isoweek.WEEK-NUM
        """)
        self.assertEqual(32, self.interp.stack[0])

    def test_QUARTER_START(self):
        self.interp.run("""
        2022-08-09 isoweek.QUARTER-START
        2022-07-04 isoweek.QUARTER-START
        """)
        self.assertEqual(datetime.date(2022, 7, 4), self.interp.stack[0])
        self.assertEqual(datetime.date(2022, 7, 4), self.interp.stack[1])

    def test_QUARTER_END(self):
        self.interp.run("""
        2022-08-09 isoweek.QUARTER-END
        2022-10-02 isoweek.QUARTER-END
        """)
        self.assertEqual(datetime.date(2022, 10, 2), self.interp.stack[0])
        self.assertEqual(datetime.date(2022, 10, 2), self.interp.stack[1])

    def test_QUARTER_slash_YEAR(self):
        self.interp.run("""
        # Computes fiscal quarter for a company with a FY offset by 2 quarters
        2022-08-09 2 isoweek.QUARTER/YEAR
        2022-06-19 2 isoweek.QUARTER/YEAR
        """)
        self.assertEqual([1, 2023], self.interp.stack[0])
        self.assertEqual([4, 2022], self.interp.stack[1])

if __name__ == '__main__':
    unittest.main()
