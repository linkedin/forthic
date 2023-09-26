import unittest
from forthic.v3.interpreter import Interpreter
from forthic.v3.modules.intake_module import IntakeModule
from forthic.v3.modules.cache_module import CacheModule
from forthic.v3.modules.gsheet_module import GsheetModule

TICKET_INFO = {'formConfig': {'tab': 'Simple', 'Project': 'CF', 'Issue Type': 'Task', 'Labels': ['forthic-intake'], 'field_records': [{'Field ID': 'step1', 'Jira Field': '', 'Field Label': '', 'Field Description': '', 'Is Required?': '', 'Field Type': 'Markdown', 'Field Content': '# Forthic Intake Form', 'Max Input Length': ''}, {'Field ID': 'summary', 'Jira Field': 'Summary', 'Field Label': 'Brief Description', 'Field Description': 'Just type a **one line** statement here', 'Is Required?': 'Yes', 'Field Type': 'TextInput', 'Field Content': '', 'Max Input Length': ''}, {'Field ID': 'description', 'Jira Field': 'Description', 'Field Label': 'Long Description', 'Field Description': 'This is a longer description\n- Something to consider\n- Another thing as well', 'Is Required?': 'Yes', 'Field Type': 'Textarea', 'Field Content': '', 'Max Input Length': ''}, {'Field ID': 'images', 'Jira Field': 'Attachment', 'Field Label': 'Images', 'Field Description': 'Please submit some attachments', 'Is Required?': 'No', 'Field Type': 'Attachment', 'Field Content': '', 'Max Input Length': ''}, {'Field ID': 'priority', 'Jira Field': 'Description', 'Field Label': 'Priority', 'Field Description': '', 'Is Required?': 'No', 'Field Type': 'Dropdown', 'Field Content': 'Blocker\nCritical\nMajor\nMinor\nTrivial', 'Max Input Length': ''}, {'Field ID': 'team', 'Jira Field': 'Description', 'Field Label': 'Team', 'Field Description': 'Please enter a team (if you want)', 'Is Required?': 'Yes', 'Field Type': 'Dropdown', 'Field Content': 'LMS\nLSS\nProject Tools\nCore Eng', 'Max Input Length': ''}], 'dup_fields': {}, 'steps': [{'id': 'single_step', 'fields': ['step1', 'summary', 'description', 'images', 'priority', 'team']}]}, 'fieldsById': {'step1': {'Field ID': 'step1', 'Jira Field': '', 'Field Label': '', 'Field Description': '', 'Is Required?': '', 'Field Type': 'Markdown', 'Field Content': '# Forthic Intake Form', 'Max Input Length': ''}, 'summary': {'Field ID': 'summary', 'Jira Field': 'Summary', 'Field Label': 'Brief Description', 'Field Description': 'Just type a **one line** statement here', 'Is Required?': 'Yes', 'Field Type': 'TextInput', 'Field Content': '', 'Max Input Length': ''}, 'description': {'Field ID': 'description', 'Jira Field': 'Description', 'Field Label': 'Long Description', 'Field Description': 'This is a longer description\n- Something to consider\n- Another thing as well', 'Is Required?': 'Yes', 'Field Type': 'Textarea', 'Field Content': '', 'Max Input Length': ''}, 'images': {'Field ID': 'images', 'Jira Field': 'Attachment', 'Field Label': 'Images', 'Field Description': 'Please submit some attachments', 'Is Required?': 'No', 'Field Type': 'Attachment', 'Field Content': '', 'Max Input Length': ''}, 'priority': {'Field ID': 'priority', 'Jira Field': 'Description', 'Field Label': 'Priority', 'Field Description': '', 'Is Required?': 'No', 'Field Type': 'Dropdown', 'Field Content': 'Blocker\nCritical\nMajor\nMinor\nTrivial', 'Max Input Length': ''}, 'team': {'Field ID': 'team', 'Jira Field': 'Description', 'Field Label': 'Team', 'Field Description': 'Please enter a team (if you want)', 'Is Required?': 'Yes', 'Field Type': 'Dropdown', 'Field Content': 'LMS\nLSS\nProject Tools\nCore Eng', 'Max Input Length': ''}}, 'valuesById': {'summary': 'Test', 'description': 'A long description', 'priority': 'Critical', 'team': 'Project Tools'}}

def get_interp():
    result = Interpreter()
    result.register_module(IntakeModule)
    result.register_module(CacheModule)
    result.register_module(GsheetModule)
    result.run('["intake" "cache" "gsheet"] USE-MODULES')
    return result


class TestIntakeModule(unittest.TestCase):
    def setUp(self):
        self.interp = get_interp()

    def test_AGGREGATE_JIRA_FIELDS(self):
        # `TICKET_INFO` represents a form submission where the `team`, `priority`, and `description` all map
        # to the Description field
        self.interp.stack_push(TICKET_INFO)
        self.interp.stack_push("Description")
        self.interp.run("intake.AGGREGATE-JIRA-FIELDS")
        result = self.interp.stack_pop()
        self.assertEqual(set(["description", "priority", "team"]), set(result.keys()))


if __name__ == '__main__':
    unittest.main()
