from .interfaces import IInterpreter, IModule, IWord
from typing import Any, Callable, List, Dict, Optional


class Variable:
    """Represents a Forthic variable"""
    def __init__(self, value: Any = None):
        self.value = value
        self.has_value = False

    def set_value(self, val):
        self.value = val
        self.has_value = True

    def get_value(self):
        return self.value


class Word(IWord):
    """Base class for all Forthic words"""
    def __init__(self, name: str):
        self.name: str = name

    def execute(self, _interp: IInterpreter) -> None:
        raise RuntimeError('Must override Word.execute')


class PushValueWord(Word):
    """This word knows how to push a value onto the stack

    One use is to implement literal words
    """
    def __init__(self, name: str, value: Any):
        super().__init__(name)
        self.value = value

    def execute(self, interp: IInterpreter) -> None:
        interp.stack_push(self.value)


class DefinitionWord(Word):
    """This represents a word that is defined from other words

    A definition looks like this:
    ```
    : WORD-NAME   WORD1 WORD2 WORD3;
    ```
    The name of the defined word is `WORD-NAME`. When it is executed, `WORD1`, `WORD2`, and `WORD3` are
    executed in that order.
    """
    def __init__(self, name: str):
        super().__init__(name)
        self.words: List[IWord] = []

    def add_word(self, word: IWord):
        """Adds a new word to the definition"""
        self.words.append(word)

    def execute(self, interp: IInterpreter) -> None:
        for w in self.words:
            interp.start_profile_word(w)
            w.execute(interp)
            interp.end_profile_word()


class ModuleWord(Word):
    """This is used when defining Forthic words in Python

    The `name` is the word name.
    The `handler` is a Python function that's called when the word is executed. All handlers take an interpreter
    as their only argument and return nothing. All argument passing and results are handled via the interpreter
    stack.
    """
    def __init__(self, name: str, handler: Callable[[IInterpreter], None]):
        super().__init__(name)
        self.handler = handler

    def execute(self, interp: IInterpreter) -> None:
        self.handler(interp)


class ImportedWord(Word):
    """This represents words imported from other modules

    Words imported from other modules usually have their module name as a prefix (e.g., jira.SEARCH), but
    it's also possible to use a different prefix, or none at all.
    """
    def __init__(self, module_word: IWord, prefix: str, module: 'Module'):
        if prefix != '':
            prefix = prefix + '.'

        super().__init__(f'{prefix}{module_word.name}')
        self.module_word = module_word
        self.imported_module = module

    def execute(self, interp: IInterpreter) -> None:
        interp.module_stack_push(self.imported_module)
        self.module_word.execute(interp)
        interp.module_stack_pop()


class ModuleMemoWord(Word):
    """This memoizes the execution of an expensive operation that returns a value
    """
    def __init__(self, word: IWord):
        super().__init__(word.name)
        self.word = word
        self.has_value = False
        self.value = None

    def refresh(self, interp):
        self.word.execute(interp)
        self.value = interp.stack_pop()
        self.has_value = True

    def execute(self, interp: IInterpreter) -> None:
        if not self.has_value:
            self.refresh(interp)
        interp.stack_push(self.value)


class ModuleMemoBangWord(Word):
    """This forces the update of a ModuleMemoWord
    """
    def __init__(self, word: ModuleMemoWord):
        super().__init__(f"{word.name}!")
        self.memo_word = word

    def execute(self, interp: IInterpreter) -> None:
        self.memo_word.refresh(interp)


class ModuleMemoBangAtWord(Word):
    """This forces the update of a ModuleMemoWord
    """
    def __init__(self, word: ModuleMemoWord):
        super().__init__(f"{word.name}!@")
        self.memo_word = word

    def execute(self, interp: IInterpreter) -> None:
        self.memo_word.refresh(interp)
        interp.stack_push(self.memo_word.value)


class Module(IModule):
    """A Module is a collection of variables and words

    Modules may also create other modules.
    """
    def __init__(self, name: str, interp: IInterpreter, forthic_code: str = ''):
        self.interp: IInterpreter = interp
        self.words: List[IWord] = []
        self.exportable: List[str] = []   # Word names
        self.variables: Dict[str, Variable] = {}
        self.modules: Dict[str, Module] = {}
        self.name: str = name
        self.forthic_code: str = forthic_code

    def find_module(self, name: str) -> Optional['Module']:
        result = self.modules.get(name)
        return result

    def add_word(self, word: IWord) -> None:
        """Adds a word to the module"""
        self.words.append(word)

    def add_memo_words(self, word: IWord) -> None:
        """Adds memo words to a module based on a core definition word

        For a word named "MY-MEMO", this adds the following words:
            * MY-MEMO  (memoizes the execution of the provided definition word)
            * MY-MEMO!  (re-runs MY-MEMO to update its memoized value)
            * MY-MEMO!@  (runs MY-MEMO! and returns then returns the new memo value)
        """
        memo_word = ModuleMemoWord(word)
        self.words.append(memo_word)
        self.words.append(ModuleMemoBangWord(memo_word))
        self.words.append(ModuleMemoBangAtWord(memo_word))

    def add_module_word(self, word_name: str, word_func: Callable[[IInterpreter], None]) -> None:
        """Convenience function for adding exportable module words"""
        self.add_exportable_word(ModuleWord(word_name, word_func))

    def add_exportable_word(self, word: ModuleWord) -> None:
        """Marks a word as exportable by the module

        Only exportable words can be used by other modules
        """
        self.words.append(word)
        self.exportable.append(word.name)

    def add_exportable(self, names: List[str]) -> None:
        """Convenience to add a set of exportable words

        This is used when marking words as exportable from Forthic
        """
        self.exportable += names

    def exportable_words(self) -> List[IWord]:
        result = [w for w in self.words if w.name in self.exportable]
        return result

    def add_variable(self, name: str, value: Any = None) -> None:
        """Adds variable to module, noop if variable exists"""
        if name not in self.variables:
            self.variables[name] = Variable(value)

    def initialize(self, interp: IInterpreter) -> None:
        """When a module is imported, its `forthic_code` must be executed in order to fully define its words"""
        interp.run_in_module(self, self.forthic_code)

    def register_module(self, module_name: str, module: 'Module') -> None:
        """Registers a module by name"""
        self.modules[module_name] = module

    def import_module(self, module_name: str, module: 'Module', interp: IInterpreter) -> None:
        """This is used to import a module for use by another module via Python

        Typically, modules are independent. But in some cases, a module may depend on other modules. When this
        is the case, `import_module` is used to import the modules at code time.
        """
        # If module has already been registered, use it
        if module_name in self.modules:
            new_module = self.modules[module_name]
        else:
            new_module = module
            new_module.initialize(interp)

        words = new_module.exportable_words()
        for word in words:
            self.add_word(ImportedWord(word, module_name, new_module))

        self.register_module(module_name, new_module)

    def find_word(self, name: str) -> Optional[IWord]:
        """Searches module for a word"""
        result = self.find_dictionary_word(name)
        if result is None:
            result = self.find_variable(name)
        return result

    def find_dictionary_word(self, word_name: str) -> Optional[IWord]:
        """Looks up word in module, returning None if not found"""
        indexes = list(reversed(range(len(self.words))))
        for i in indexes:
            w = self.words[i]
            if w.name == word_name:
                return w
        return None

    def find_variable(self, varname: str) -> Optional[PushValueWord]:
        """Returns variable"""
        variable = self.variables.get(varname)
        result = None
        if variable:
            result = PushValueWord(varname, variable)
        return result
