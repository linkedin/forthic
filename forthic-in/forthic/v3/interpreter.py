import time
import operator
import pytz
import collections
from .tokens import (
    StringToken,
    CommentToken,
    StartArrayToken,
    EndArrayToken,
    StartModuleToken,
    EndModuleToken,
    StartDefinitionToken,
    EndDefinitionToken,
    StartMemoToken,
    WordToken,
    EOSToken,
    Token,
)
from .tokenizer import Tokenizer

from .module import Module, Word, PushValueWord, DefinitionWord
from .global_module import GlobalModule
from .profile import WordProfile
from .interfaces import IInterpreter, IModule, IWord
from typing import List, Any, Dict, Optional


# ----- Errors -----------------------------------------------------------------------------------------------
class InterpreterError(RuntimeError):
    pass


class UnknownModuleError(InterpreterError):
    def __init__(self, name: str):
        super().__init__(f"Can't find module: '{name}'")


class UnknownTokenError(InterpreterError):
    def __init__(self, token: Token):
        super().__init__(f'Unknown token: {token}')


class NestedDefinitionError(InterpreterError):
    def __init__(self):
        super().__init__("Can't have nested definitions")


class UnmatchedEndDefinitionError(InterpreterError):
    def __init__(self):
        super().__init__('Unmatched end definition')


class UnknownWordError(InterpreterError):
    def __init__(self, word_name: str):
        super().__init__(f"Unknown word: '{word_name}'")


# ----- Word Types -------------------------------------------------------------------------------------------
class EndArrayWord(Word):
    """This represents the end of an array"""
    def __init__(self):
        super().__init__(']')

    def execute(self, interp: IInterpreter) -> None:
        items: List[Any] = []
        item = interp.stack_pop()
        while not isinstance(item, StartArrayToken):
            items.append(item)
            item = interp.stack_pop()
        items.reverse()
        interp.stack_push(items)


class StartModuleWord(Word):
    """This indicates the start of a module

    See `docs/ARCHITECTURE.md` for more details on modules.
    """
    def __init__(self, name: str):
        super().__init__(name)

    def execute(self, interp: IInterpreter) -> None:
        # The app module is the only module with a blank name
        if self.name == '':
            interp.module_stack_push(interp.app_module)
            return

        # If the module is used by the current module, push it onto the module stack;
        # otherwise, create a new module and push that onto the module stack.
        module = interp.cur_module().find_module(self.name)

        # Check app module
        if not module:
            module = interp.app_module.find_module(self.name)

        if not module:
            module = Module(self.name, interp)
            interp.cur_module().register_module(module.name, module)

        interp.module_stack_push(module)


class EndModuleWord(Word):
    def __init__(self):
        super().__init__('}')

    def execute(self, interp: IInterpreter) -> None:
        interp.module_stack_pop()


class AppModule(Module):
    """The AppModule contains the words and variables of a Forthic application

    The app module is a speical module. This is the first module on the module stack. All applications start
    here. It is the only module where `USE-MODULE` can be called. It is the only nameless module.
    """
    def __init__(self, interp: 'Interpreter'):
        super().__init__('', interp)
        # Screens map names to chunks of Forthic code
        self.screens: Dict[str, str] = collections.defaultdict(str)

    def set_screen(self, name: str, content: str):
        self.screens[name] = content

    def get_screen(self, name: str) -> str:
        return self.screens[name]


class Interpreter(IInterpreter):
    """Interprets Forthic strings

    Modules may be registered with an Interpreter to provide more functionality.
    """
    def __init__(self, timezone=None):
        if not timezone:
            timezone = pytz.timezone('US/Pacific')
        self.timezone = timezone
        self.stack: List[Any] = []
        self.global_module = GlobalModule(self, self.timezone)
        self.app_module = AppModule(self)
        self.module_stack: List[IModule] = [self.app_module]
        self.registered_modules: Dict[str, Module] = {}
        self.is_compiling: bool = False
        self.is_memo_definition: bool = False
        self.cur_definition: Optional[DefinitionWord] = None
        self._dev_mode: bool = False

        # Profiling support
        self.word_counts: Dict[IWord, int] = collections.defaultdict(int)
        self.is_profiling: bool = False
        self.start_profile_time: Optional[float] = None
        self.timestamps: List[Any] = []
        self.cur_word_profile: WordProfile = None

    @property
    def dev_mode(self) -> bool:
        """This is used to indicate that things like debugging are ok"""
        return self._dev_mode

    @dev_mode.setter
    def dev_mode(self, dev_mode: bool):
        self._dev_mode = dev_mode

    def run(self, string: str) -> None:
        """Interprets a Forthic string, executing words one-at-a-time until the end of the string"""
        tokenizer = Tokenizer(string)
        token = tokenizer.next_token()
        while not isinstance(token, EOSToken):
            self.handle_token(token)
            token = tokenizer.next_token()

    def run_in_module(self, module: IModule, string: str) -> None:
        """Runs a Forthic string in the context of a given module"""
        self.module_stack_push(module)
        self.run(string)
        self.module_stack.pop()

    def cur_module(self) -> IModule:
        """The top of the module stack is the currently active module"""
        result = self.module_stack[-1]
        return result

    def find_module(self, name: str) -> Module:
        """Returns the module registered under the specified `name`"""
        if name not in self.registered_modules:
            raise UnknownModuleError(name)
        result = self.registered_modules[name]
        return result

    def stack_push(self, val: Any) -> None:
        """Pushes a value onto the Forth stack"""
        self.stack.append(val)

    def stack_pop(self) -> Any:
        """Pops a value from the Forth stack and returns it"""
        result = self.stack.pop()
        return result

    def module_stack_push(self, module: IModule) -> None:
        """Makes the specified `module` the active module"""
        self.module_stack.append(module)

    def module_stack_pop(self) -> IModule:
        """Removes the current module from the stack and makes the next module the current module"""
        return self.module_stack.pop()

    def register_module(self, module_class):
        """Registers an instance of Module with the interpreter

        Modules are typically registered at code time. This is where new capabilities can be made available
        to Forthic programs.
        """
        module = module_class(self)
        self.registered_modules[module.name] = module

    def find_word(self, name: str) -> Optional[IWord]:
        """Searches the interpreter for a word

        The module stack is searched top down. If the words cannot be found, the global module is searched.
        Note that the bottom of the module stack is always the application module.
        """
        modules = reversed(self.module_stack)
        result = None
        for m in modules:
            result = m.find_word(name)
            if result:
                break

        if not result:
            result = self.global_module.find_word(name)
        return result

    # --------------------------------------------------------------------------
    # Profiling support

    def start_profiling(self) -> None:
        """Clears word counts and starts profiling word executions"""
        self.is_profiling = True
        self.timestamps = []
        self.start_profile_time = time.perf_counter()
        self.add_timestamp('START')
        self.word_counts = collections.defaultdict(int)

    def add_timestamp(self, label: str) -> None:
        """Adds a timestamped label to a profiling run"""
        if not self.is_profiling or not self.start_profile_time:
            return
        self.timestamps.append(
            {
                'label': label,
                'time': time.perf_counter() - self.start_profile_time,
            }
        )

    def count_word(self, w: IWord) -> None:
        """If profiling, count word"""
        if self.is_profiling:
            self.word_counts[w] += 1

    def start_profile_word(self, word: IWord) -> None:
        """Used to mark the start of a word execution during a profiling run"""
        if not self.is_profiling:
            return

        word_profile = WordProfile(
            self.cur_word_profile, self.cur_module(), word
        )
        self.cur_word_profile = word_profile

    def end_profile_word(self) -> None:
        """Used to mark the end of a word execution during a profiling run"""
        if not self.cur_word_profile:
            return

        self.cur_word_profile.end_profile()
        parent = self.cur_word_profile.get_parent()
        if parent:
            self.cur_word_profile = parent

    def stop_profiling(self) -> None:
        """Stops profiling"""
        self.add_timestamp('END')
        self.is_profiling = False

    def word_histogram(self) -> List[Any]:
        """Returns a list of counts in descending order"""
        items = [
            {'word': w.name, 'count': c} for w, c in self.word_counts.items()
        ]
        result = sorted(items, key=operator.itemgetter('count'), reverse=True)
        return result

    def profile_timestamps(self) -> List[Any]:
        return self.timestamps

    # --------------------------------------------------------------------------
    # Handle tokens

    def handle_token(self, token: Token) -> None:
        """Called to handle each token from the Tokenizer"""
        if isinstance(token, StringToken):
            self.handle_string_token(token)
        elif isinstance(token, CommentToken):
            self.handle_comment_token(token)
        elif isinstance(token, StartArrayToken):
            self.handle_start_array_token(token)
        elif isinstance(token, EndArrayToken):
            self.handle_end_array_token(token)
        elif isinstance(token, StartModuleToken):
            self.handle_start_module_token(token)
        elif isinstance(token, EndModuleToken):
            self.handle_end_module_token(token)
        elif isinstance(token, StartDefinitionToken):
            self.handle_start_definition_token(token)
        elif isinstance(token, StartMemoToken):
            self.handle_start_memo_token(token)
        elif isinstance(token, EndDefinitionToken):
            self.handle_end_definition_token(token)
        elif isinstance(token, WordToken):
            self.handle_word_token(token)
        else:
            raise UnknownTokenError(token)

    def handle_string_token(self, token: StringToken) -> None:
        self.handle_word(PushValueWord('<string>', token.string))

    def handle_start_module_token(self, token: StartModuleToken) -> None:
        """Start/end module tokens are treated as IMMEDIATE words *and* are compiled"""
        word = StartModuleWord(token.name)
        if self.is_compiling:
            if not self.cur_definition:
                raise InterpreterError("Interpreter is compiling, but there is no current definition")
            self.cur_definition.add_word(word)

        # NOTE: We execute the word within a definition so we can do lookups during compile
        self.count_word(word)
        word.execute(self)

    def handle_end_module_token(self, token: EndModuleToken) -> None:
        word = EndModuleWord()
        if self.is_compiling:
            if not self.cur_definition:
                raise InterpreterError("Interpreter is compiling, but there is no current definition")
            self.cur_definition.add_word(word)

        # NOTE: We execute the word within a definition so we can do lookups during compile
        self.count_word(word)
        word.execute(self)

    def handle_start_array_token(self, token: StartArrayToken) -> None:
        self.handle_word(PushValueWord('<start_array_token>', token))

    def handle_end_array_token(self, token: EndArrayToken) -> None:
        self.handle_word(EndArrayWord())

    def handle_comment_token(self, token: CommentToken) -> None:
        pass

    def handle_start_definition_token(self, token: StartDefinitionToken) -> None:
        if self.is_compiling:
            raise NestedDefinitionError()
        self.cur_definition = DefinitionWord(token.name)
        self.is_compiling = True
        self.is_memo_definition = False

    def handle_start_memo_token(self, token: StartMemoToken) -> None:
        if self.is_compiling:
            raise NestedDefinitionError()
        self.cur_definition = DefinitionWord(token.name)
        self.is_compiling = True
        self.is_memo_definition = True

    def handle_end_definition_token(self, token: EndDefinitionToken) -> None:
        if not self.is_compiling:
            raise UnmatchedEndDefinitionError()
        if not self.cur_definition:
            raise InterpreterError("Cannot finish definition because no 'cur_definition'")

        if self.is_memo_definition:
            self.cur_module().add_memo_words(self.cur_definition)
        else:
            self.cur_module().add_word(self.cur_definition)
        self.is_compiling = False

    def handle_word_token(self, token: WordToken) -> None:
        word = self.find_word(token.name)
        if word is None:
            raise UnknownWordError(token.name)

        self.handle_word(word)

    def handle_word(self, word: IWord) -> None:
        if self.is_compiling:
            if not self.cur_definition:
                raise InterpreterError("Interpreter is compiling, but there is no current definition")
            self.cur_definition.add_word(word)
        else:
            self.count_word(word)
            word.execute(self)
