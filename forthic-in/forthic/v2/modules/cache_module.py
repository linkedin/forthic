import os
import json
from ..module import Module
from ..interfaces import IInterpreter


class CacheModule(Module):
    """This implements a simple file-based cache for Forthic data

    `CACHE!` stores data in JSON format
    `CACHE@` loads data from cache as a Python dict

    See `docs/modules/cache_module.md` for detailed descriptions of each word.
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('cache', interp, CACHE_FORTHIC)
        self.add_module_word('CWD!', self.word_CWD_bang)
        self.add_module_word('CACHE!', self.word_CACHE_bang)
        self.add_module_word('CACHE@', self.word_CACHE_at)

        self.working_directory = '.'
        self.cache_file = '.cache'

    # ( path -- )
    def word_CWD_bang(self, interp: IInterpreter):
        path = interp.stack_pop()
        self.working_directory = path

    # ( value key -- )
    def word_CACHE_bang(self, interp: IInterpreter):
        key = interp.stack_pop()
        value = interp.stack_pop()
        cache = self.load_cache()

        cache[key] = value

        self.store_cache(cache)

    # ( key -- value )
    def word_CACHE_at(self, interp: IInterpreter):
        key = interp.stack_pop()
        cache = self.load_cache()
        result = cache.get(key)
        interp.stack_push(result)

    # ----------------------------------------
    # Helpers
    def get_cache_filename(self):
        result = f'{self.working_directory}/{self.cache_file}'
        return result

    def ensure_cache_file(self):
        filename = self.get_cache_filename()
        if not os.path.isfile(filename):
            with open(filename, 'w') as f:
                f.write(json.dumps({}))

    def load_cache(self):
        self.ensure_cache_file()
        filename = self.get_cache_filename()
        with open(filename, 'r') as f:
            content = f.read().strip()
            if content:
                result = json.loads(content)
            else:
                result = {}
        return result

    def store_cache(self, cache):
        filename = self.get_cache_filename()
        with open(filename, 'w') as f:
            f.write(json.dumps(cache, indent=4, separators=(',', ': ')))


CACHE_FORTHIC = ''
