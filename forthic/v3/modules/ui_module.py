"""Implements module specifying UI frameworks
"""
from ..module import Module
from ..interfaces import IInterpreter


class UIModule(Module):
    def __init__(self, interp: IInterpreter):
        super().__init__("ui", interp, FORTHIC)
        self.add_module_word("FORTHIC-REACT/v1", self.word_FORTHIC_REACT_slash_v1)
        self.add_module_word("<CSS", self.word_l_CSS)
        self.add_module_word("<JSX", self.word_l_JSX)
        self.add_module_word("<FORTHIC", self.word_l_FORTHIC)
        return

    # ( -- ForthicUI )
    def word_FORTHIC_REACT_slash_v1(self, interp: IInterpreter):
        result = ForthicReactUI_v1()
        interp.stack_push(result)

    # ( ForthicUI css -- ForthicUI )
    def word_l_CSS(self, interp: IInterpreter):
        css = interp.stack_pop()
        forthic_ui = interp.stack_pop()

        forthic_ui.css = css
        interp.stack_push(forthic_ui)

    # ( ForthicUI jsx -- ForthicUI )
    def word_l_JSX(self, interp: IInterpreter):
        jsx = interp.stack_pop()
        forthic_ui = interp.stack_pop()

        forthic_ui.jsx = jsx
        interp.stack_push(forthic_ui)

    # ( ForthicUI forthic -- ForthicUI )
    def word_l_FORTHIC(self, interp: IInterpreter):
        forthic = interp.stack_pop()
        forthic_ui = interp.stack_pop()

        forthic_ui.forthic = forthic
        interp.stack_push(forthic_ui)


FORTHIC = ""


class ForthicReactUI:
    def __init__(self):
        self.css = ""
        self.jsx = ""
        self.forthic = ""


class ForthicReactUI_v1(ForthicReactUI):
    pass
