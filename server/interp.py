from forthic.interpreter import Interpreter
from forthic.modules.jira_module import JiraModule
from forthic.modules.cache_module import CacheModule
from forthic.modules.stats_module import StatsModule
from forthic.modules.org_module import OrgModule
from forthic.modules.ui_module import UIModule
from forthic.modules.gsheet_module import GsheetModule
from forthic.modules.jinja_module import JinjaModule
from forthic.modules.intake_module import IntakeModule
from server.contexts_module import ContextsModule
from server.simple_module import SimpleModule


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
    interp.register_module(IntakeModule)
    interp.register_module(SimpleModule)
    return interp
