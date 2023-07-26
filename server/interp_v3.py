from forthic.v3.interpreter import Interpreter
from forthic.v3.modules.jira_module import JiraModule
from forthic.v3.modules.cache_module import CacheModule
from forthic.v3.modules.stats_module import StatsModule
from forthic.v3.modules.org_module import OrgModule
from forthic.v3.modules.ui_module import UIModule
from forthic.v3.modules.gsheet_module import GsheetModule
from forthic.v3.modules.jinja_module import JinjaModule
from contexts_module_v3 import ContextsModule


def get_interp(app_dir):
    interp = Interpreter()
    interp.dev_mode = True

    def configure_cache_module(interp):
        interp.register_module(CacheModule)
        interp.run(f"['cache'] USE-MODULES '{app_dir}' cache.CWD!")

    configure_cache_module(interp)
    interp.register_module(GsheetModule)
    # interp.register_module(excel_module.ExcelModule)
    interp.register_module(JiraModule)
    interp.register_module(StatsModule)
    interp.register_module(JinjaModule)
    # interp.register_module(ConfluenceModule)
    interp.register_module(ContextsModule)
    interp.register_module(OrgModule)
    interp.register_module(UIModule)
    return interp
