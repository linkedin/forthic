import jinja2
from ..module import Module
from ..interfaces import IInterpreter


class JinjaModule(Module):
    """This provides access to Jinja via a thin wrapper

    See `docs/modules/jinja_module.md` for detailed descriptions of each word.
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('jinja', interp, JINJA_FORTHIC)
        self.add_module_word('RENDER', self.word_RENDER)

    # ( template_contents kw_args -- string )
    def word_RENDER(self, interp: IInterpreter):
        kw_args = interp.stack_pop()
        template_contents = interp.stack_pop()

        template = jinja2.Template(template_contents)
        result = template.render(kw_args)
        interp.stack_push(result)


JINJA_FORTHIC = ''
