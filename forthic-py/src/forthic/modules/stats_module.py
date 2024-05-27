"""Implements module to compute statistics
"""
import statistics
from ..module import Module
from ..interfaces import IInterpreter


class StatsModule(Module):
    def __init__(self, interp: IInterpreter):
        super().__init__("stats", interp, FORTHIC)
        self.add_module_word("MEAN", self.word_MEAN)
        self.add_module_word("MEDIAN", self.word_MEDIAN)
        return

    # ( numbers -- mean )
    def word_MEAN(self, interp: IInterpreter):
        numbers = interp.stack_pop()
        result = statistics.mean(numbers)
        interp.stack_push(result)

    # ( numbers -- median )
    def word_MEDIAN(self, interp: IInterpreter):
        numbers = interp.stack_pop()
        result = statistics.median(numbers)
        interp.stack_push(result)


FORTHIC = ""
