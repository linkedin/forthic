from forthic.module import Module
import datetime


class SampleDateModule(Module):
    def __init__(self, interp):
        super().__init__("date", interp)
        self.add_module_word("TODAY", self.word_TODAY)

    # ( -- today )
    def word_TODAY(self, interp):
        """Pushes today's date"""
        result = datetime.date.today()
        interp.stack_push(result)
