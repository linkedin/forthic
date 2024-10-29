from forthic.module import Module
from forthic.interfaces import IInterpreter


class SimpleModule(Module):
    """This implements a simple module"""

    def __init__(self, interp: IInterpreter):
        super().__init__("simple", interp)
        self.add_module_word("MESSAGE", self.word_MESSAGE)

    # ( -- message )
    def word_MESSAGE(self, interp: IInterpreter):
        interp.stack_push("Hello, world!")
