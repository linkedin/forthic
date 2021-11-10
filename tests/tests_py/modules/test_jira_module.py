import unittest
import datetime
from forthic.interpreter import Interpreter
from forthic.modules.jira_module import JiraModule
from tests.tests_py.modules.jira_context import JiraTestContext


def get_interp():
    interp = Interpreter()
    interp.register_module(JiraModule)

    # Set up Jira staging context
    interp.run("['jira'] USE-MODULES")
    interp.stack_push(JiraTestContext())
    interp.run("jira.PUSH-CONTEXT!")
    return interp


class TestJiraModule(unittest.TestCase):
    def setUp(self):
        self.interp = get_interp()

    def test_HOST(self):
        self.interp.run("jira.HOST")
        self.assertEqual(self.interp.stack[0], "http://testcontext")

    def test_SEARCH(self):
        self.interp.run("""
            : JQL   ["assignee=testuser and resolution is null"] CONCAT;
            : FIELDS   ['Summary' 'Assignee'];
            JQL FIELDS jira.SEARCH
        """)
        issues = self.interp.stack[0]
        self.assertEqual(2, len(issues))
        self.assertEqual("SAMPLE-1234", issues[0]['key'])
        self.assertEqual("testuser", issues[0]['Assignee'])
        self.assertEqual("SAMPLE-1235", issues[1]['key'])

    def test_DEFAULT_SEARCH(self):
        self.interp.run("""
            : JQL   ["assignee=testuser and resolution is null"] CONCAT;
            : FIELDS   ['Summary' 'Assignee'];
            JQL FIELDS jira.DEFAULT-SEARCH
        """)
        issues = self.interp.stack[0]
        self.assertEqual(2, len(issues))
        self.assertEqual("SAMPLE-1234", issues[0]['key'])
        self.assertEqual("testuser", issues[0]['Assignee']['key'])

    def test_CREATE(self):
        self.interp.run("""
            [
               ["Project" "SAMPLE"]
               ["Summary" "A sample ticket"]
               ["Reporter" "testuser"]
               ["Issue Type"  "Task"]
            ] REC jira.CREATE
        """)
        self.assertEqual("SAMPLE-12345", self.interp.stack[0])

    def test_UPDATE(self):
        self.interp.run("""
            "SAMPLE-1234" [["Assignee" "testuser2"]] REC jira.UPDATE
        """)

    def test_ADD_WATCHER(self):
        self.interp.run("""
            "SAMPLE-1234" "manager1" jira.ADD-WATCHER
        """)

    def test_LINK_ISSUES(self):
        self.interp.run("""
            "SAMPLE-101" "SAMPLE-202" jira.DEPENDENCY jira.LINK-ISSUES
            "SAMPLE-101" "SAMPLE-202" jira.ACTION-ITEM jira.LINK-ISSUES
            "SAMPLE-101" "SAMPLE-202" jira.CLONERS jira.LINK-ISSUES
            "SAMPLE-101" "SAMPLE-202" jira.DUPLICATE jira.LINK-ISSUES
            "SAMPLE-101" "SAMPLE-202" jira.ISSUE-SPLIT jira.LINK-ISSUES
            "SAMPLE-101" "SAMPLE-202" jira.RELATED jira.LINK-ISSUES
            "SAMPLE-101" "SAMPLE-202" jira.REQUIRE jira.LINK-ISSUES
        """)

    def test_VOTES(self):
        self.interp.run("""
            "SAMPLE-101" jira.VOTES
        """)
        self.assertEqual(['user1', 'user2'], self.interp.stack[0])

    def test_CHANGELOG(self):
        self.interp.run("""
            "SAMPLE-101" ["Risk_Factor"] jira.CHANGELOG
        """)
        changes = self.interp.stack[0]
        self.assertEqual(3, len(changes))

        self.assertEqual('', changes[0]['from'])
        self.assertEqual('Blue', changes[0]['to'])

        self.assertEqual('Blue', changes[1]['from'])
        self.assertEqual('Green', changes[1]['to'])

        self.assertEqual('Green', changes[2]['from'])
        self.assertEqual('Yellow', changes[2]['to'])

    def test_FIELD_AS_OF(self):
        self.interp.run("""
            ["changes"] VARIABLES
            "SAMPLE-101" ["Risk_Factor"] jira.CHANGELOG changes !
             2020-07-25 changes @ "Risk_Factor" jira.FIELD-AS-OF
             2020-10-01 changes @ "Risk_Factor" jira.FIELD-AS-OF
        """)
        self.assertEqual("Green", self.interp.stack[0])
        self.assertEqual("Yellow", self.interp.stack[1])

    def test_FIELD_CHANGE_AS_OF(self):
        self.interp.run("""
            ["changes"] VARIABLES
            "SAMPLE-101" ["Risk_Factor"] jira.CHANGELOG changes !
             2020-07-25 changes @ "Risk_Factor" jira.FIELD-CHANGE-AS-OF 'date' REC@ DATE>STR
             2020-10-01 changes @ "Risk_Factor" jira.FIELD-CHANGE-AS-OF 'date' REC@ DATE>STR
        """)
        self.assertEqual("2020-07-25", self.interp.stack[0])
        self.assertEqual("2020-08-15", self.interp.stack[1])

    def test_TIME_IN_STATE(self):
        field = "status"
        resolution = "Fixed"

        # NOTE: The following data would come from something like `'PROJ-1234' ['status'] jira.CHANGELOG`
        changes = [
            {"date": datetime.datetime(2021, 7, 21, 1, 14, 57), "field": "status", "from": "", "to": "Open"},
            {"date": datetime.datetime(2021, 8, 23, 2, 56, 7), "field": "status", "from": "Open", "to": "Scoping", "from_": "1", "to_": "10128"},
            {"date": datetime.datetime(2021, 9, 27, 19, 53, 39), "field": "status", "from": "Scoping", "to": "In Development", "from_": "10128", "to_": "10194"},
            {"date": datetime.datetime(2021, 11, 4, 8, 36, 5), "field": "status", "from": "In Development", "to": "Closed", "from_": "10194", "to_": "6"}
        ]

        # Make the call
        self.interp.stack_push(resolution)
        self.interp.stack_push(changes)
        self.interp.stack_push(field)
        self.interp.run("jira.TIME-IN-STATE")

        # Check the results
        result = self.interp.stack_pop()
        self.assertAlmostEqual(793, int(result['Open']))
        self.assertAlmostEqual(856, int(result['Scoping']))
        self.assertAlmostEqual(900, int(result['In Development']))
        self.assertAlmostEqual(0, int(result['Closed']))

    def test_FIELD_TAG(self):
        self.interp.run("""
            ["ticket"] VARIABLES
            [
                ["Description" "This is a sample description [objective: To make things awesome]"]
            ] REC ticket !

            ticket @ "Description" "objective" jira.FIELD-TAG
        """)
        self.assertEqual("To make things awesome", self.interp.stack[0])

    def test_REMOVE_FIELD_TAGS(self):
        self.interp.run("""
            "This is a sample description. [objective: To make things awesome] alpha [tag2: Something else] beta" jira.REMOVE-FIELD-TAGS
        """)
        self.assertEqual("This is a sample description.  alpha  beta", self.interp.stack[0])

    def test_l_FIELD_TAG_bang(self):
        self.interp.run("""
            ["ticket"] VARIABLES
            [
                ["Description" "This is a sample description."]
            ] REC ticket !

            ticket @ "Description" "risk" "There isn't any risk!" jira.<FIELD-TAG!
        """)
        ticket = self.interp.stack[0]
        self.assertEqual("This is a sample description.\n\n[risk: There isn't any risk!]", ticket["Description"])


if __name__ == '__main__':
    unittest.main()