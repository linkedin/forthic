"""Implements module specifying UI frameworks
"""
from ..module import Module
from ..interfaces import IInterpreter


class UIModule(Module):
    def __init__(self, interp: IInterpreter):
        super().__init__("ui", interp, FORTHIC)
        self.add_module_word("FORTHIC-REACT-v1", self.word_FORTHIC_REACT_v1)
        self.add_module_word("<CSS", self.word_l_CSS)
        self.add_module_word("<JSX", self.word_l_JSX)
        self.add_module_word("<FORTHIC", self.word_l_FORTHIC)
        return

    # ( -- ForthicReact )
    def word_FORTHIC_REACT_v1(self, interp: IInterpreter):
        result = ForthicReact("v1")
        interp.stack_push(result)

    # ( ForthicReact css -- ForthicReact )
    def word_l_CSS(self, interp: IInterpreter):
        css = interp.stack_pop()
        forthic_react = interp.stack_pop()

        forthic_react.css = css
        interp.stack_push(forthic_react)

    # ( ForthicReact jsx -- ForthicReact )
    def word_l_JSX(self, interp: IInterpreter):
        jsx = interp.stack_pop()
        forthic_react = interp.stack_pop()

        forthic_react.jsx = jsx
        interp.stack_push(forthic_react)

    # ( ForthicReact forthic -- ForthicReact )
    def word_l_FORTHIC(self, interp: IInterpreter):
        forthic = interp.stack_pop()
        forthic_react = interp.stack_pop()

        forthic_react.forthic = forthic
        interp.stack_push(forthic_react)


FORTHIC = ""


class ForthicReact:
    def __init__(self, version):
        self.version = version
        self.css = ""
        self.jsx = ""
        self.forthic = ""
