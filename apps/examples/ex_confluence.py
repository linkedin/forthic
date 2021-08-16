from forthic.interpreter import Interpreter
import forthic.modules.confluence_module as confluence_module

from .utils import creds


def get_interp():
    interp = Interpreter()
    interp.register_module(confluence_module.ConfluenceModule)

    class MyContext(confluence_module.ConfluenceContext):
        def __init__(self):
            self.app_creds = creds.get_creds('CONFLUENCE')

        def get_host(self):
            return self.app_creds['host']

        def get_username(self):
            return self.app_creds['username']

        def get_password(self):
            return self.app_creds['password']

    # Set up confluence context
    interp.run("['confluence'] USE-MODULES")
    interp.stack_push(MyContext())
    interp.run('confluence.PUSH-CONTEXT!')
    return interp


def main():
    interp = get_interp()
    interp.run(
        '''
    'SPACE' 'A page title' confluence.PAGE-INFO
    #'SPACE' 'A parent page title' 'Page title' 'h2. This is a test' confluence.UPSERT-PAGE
    '''
    )


if __name__ == '__main__':
    main()
