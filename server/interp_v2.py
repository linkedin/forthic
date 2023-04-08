from forthic.v2.interpreter import Interpreter
import forthic.v2.modules.jira_module as jira_module
import forthic.v2.modules.gsheet_module as gsheet_module
import forthic.v2.modules.excel_module as excel_module
from forthic.v2.modules.cache_module import CacheModule
from forthic.v2.modules.jinja_module import JinjaModule
from forthic.v2.modules.html_module import HtmlModule
from forthic.v2.modules.org_module import OrgModule
from forthic.v2.modules.confluence_module import ConfluenceModule
from contexts_module_v2 import ContextsModule


def get_interp(app_dir):
    def configure_cache_module(interp):
        interp.register_module(CacheModule)
        interp.run(f"['cache'] USE-MODULES '{app_dir}' cache.CWD!")

    def configure_html_module(interp):
        interp.register_module(HtmlModule)
        js_path = '/static/forthic/forthic-js'
        interp.run(f"['html'] USE-MODULES '{js_path}' html.JS-PATH!")

    interp = Interpreter()
    interp.dev_mode = True

    configure_html_module(interp)
    configure_cache_module(interp)

    interp.register_module(gsheet_module.GsheetModule)
    interp.register_module(excel_module.ExcelModule)
    interp.register_module(jira_module.JiraModule)
    interp.register_module(JinjaModule)
    interp.register_module(ConfluenceModule)
    interp.register_module(ContextsModule)
    interp.register_module(OrgModule)
    return interp
