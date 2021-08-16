from forthic.interpreter import Interpreter
import forthic.modules.jira_module as jira_module

from .utils import creds


def get_interp():
    interp = Interpreter()
    interp.register_module(jira_module.JiraModule)

    class MyContext(jira_module.JiraContext):
        def __init__(self):
            self.app_creds = creds.get_creds('JIRA_STG')

        def get_host(self):
            return self.app_creds['host']

        def get_username(self):
            return self.app_creds['username']

        def get_password(self):
            return self.app_creds['password']

    # Set up Jira staging context
    interp.run("['jira'] USE-MODULES")
    interp.stack_push(MyContext())
    interp.run('jira.PUSH-CONTEXT!')
    return interp


def main():
    interp = get_interp()

    # NOTE: Update this to specify a user to be used throughout this example
    user = '<JIRA-USERNAME>'

    interp.run(f"['user'] VARIABLES  '{user}' user !")

    interp.run(
        '''
    : JQL   ["assignee=" user @ " and resolution is null"] CONCAT;
    : FIELDS   ['Summary' 'Assignee'];
    JQL FIELDS jira.SEARCH .s

    # jira.HOST
    # JQL FIELDS jira.DEFAULT-SEARCH
    # JQL FIELDS jira.RENDERED-SEARCH .s
    # [
    #    ["Project" "A-JIRA-PROJECT"]
    #    ["Summary" "A sample ticket"]
    #    ["Reporter" user @]
    #    ["Issue Type"  "Task"]
    # ] REC jira.CREATE .s

    ["changes"] VARIABLES
    # "A-JIRA-PROJECT-1234" ["Assignee"] jira.CHANGELOG changes !
    # 2020-07-25 changes @ "Assignee" jira.FIELD-AS-OF
    # 2020-10-01 changes @ "Assignee" jira.FIELD-AS-OF .s

    ["ticket"] VARIABLES
    #"key = A-JIRA-PROJECT-1234" ["Summary" "Description"] jira.SEARCH  0 NTH  ticket !
    #ticket @ "Description" "objective" jira.FIELD-TAG
    #ticket @ "Description" REC@ jira.REMOVE-FIELD-TAGS
    #ticket @ "Description" "risk" "There isn't any risk!" jira.<FIELD-TAG! .s
    #"A-JIRA-PROJECT-1234" [["Risk_Factor" "Yellow"]] REC jira.UPDATE .s
    # "A-JIRA-PROJECT-1234" "a_username" jira.ADD-WATCHER .s
    # "A-JIRA-PROJECT-1234" "A-JIRA-PROJECT-4567" jira.DEPENDENCY jira.LINK-ISSUES .s
    # "key = 'A-JIRA-PROJECT-1234'" ['Summary']
    # "A-JIRA-PROJECT-1234" jira.PARENT-KEY
    # "A-JIRA-PROJECT-4567" jira.PARENT-KEY
    # "A-JIRA-PROJECT-1234" jira.VOTES
    '''
    )


if __name__ == '__main__':
    main()
