from typing import Any, Optional


class IWord:
    """Forthic words can be executed by the interpreter or compiled into Forthic definitions"""
    def __init__(self, name: str):
        self.name = name

    def execute(self, _interp: 'IInterpreter') -> None:
        """Called when a Forthic word is executed by the interpreter

        Words take parameters from the interpreter `stack` and return values to it.
        """
        pass


class IModule:
    """Modules store Forthic words and variables"""
    def __init__(self):
        self.name = None

    def find_word(self, name: str) -> Optional[IWord]:
        """Searches module for a word with the specified `name`"""
        pass

    def add_word(self, word: IWord) -> None:
        """Adds a `word` to a module"""
        pass

    def add_memo_words(self, word: IWord) -> None:
        """Adds memo words based on `word` to a module"""
        pass


class IInterpreter:
    """A Forthic interpreter runs Forthic strings

    The interpreter maintains the following:

    * A `stack` for passing parameters between words
    * An `app_module` in which the current Forthic application runs
    * A `global_module` containing words common to all Forthic applications

    The interpreter has a `dev_mode` property which can change the behavior of certain words
    (e.g., `.s` will drop into the debugger if in dev mode).

    The interpreter also maintains some data structures for profiling Forthic code.
    """
    def __init__(self):
        self.app_module = None
        self.cur_module = None
        self.stack = None

        # Profiling support
        self.cur_word_profile = None
        self.profile_timestamps = None
        self.word_histogram = None
        self.dev_mode = None

    def run(self, string: str):
        """Runs a Forthic string in the context of the current module"""
        pass

    def run_in_module(self, module: IModule, string: str):
        """Runs a Forthic string in the context of the specified `module`"""
        pass

    def stack_push(self, value: Any):
        """Pushes a value onto the parameter `stack`"""
        pass

    def stack_pop(self) -> Any:
        """Pops a variable from the parameter `stack`"""
        pass

    def module_stack_push(self, module: IModule):
        """Pushes a module onto the module stack, making it the current module"""
        pass

    def module_stack_pop(self):
        """Popping a module from the module stack"""
        pass

    def find_module(self, name: str) -> IModule:
        """Searches interpreter for a module registered under `name`"""
        return IModule()

    def start_profiling(self) -> None:
        """Initializes interpreter profiling data to start a profiling run"""
        pass

    def add_timestamp(self, label: str) -> None:
        """Adds a labeled timestamp during a profiling run"""
        pass

    def stop_profiling(self) -> None:
        """Stops a profiling run and returns interpreter to normal mode"""
        pass

    def count_word(self, w: IWord) -> None:
        """Increments count of a word's execution during a profiling run"""
        pass

    def start_profile_word(self, word: IWord):
        """Notes the start of a word execution during a profiling run"""
        pass

    def end_profile_word(self):
        """Notes the end of a word execution during a profiling run"""
        pass
