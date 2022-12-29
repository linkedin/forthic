import time
from typing import List, Optional
from .interfaces import IModule, IWord


class WordProfile:
    """Stores information about a word's execution time

    This also stores a list of WordProfiles for words that are called by the word in question
    """

    def __init__(self, parent: 'WordProfile', module: IModule, word: IWord):
        self.parent = parent
        self.module = module
        self.word = word
        self.start_time = time.perf_counter()
        self.end_time: Optional[float] = None
        self.word_profiles: List['WordProfile'] = []
        self.index: int = -1

        if self.parent:
            self.parent.add_word_profile(self)

    def add_word_profile(self, word_profile: 'WordProfile'):
        self.word_profiles.append(word_profile)

    def get_key(self) -> str:
        result = f'{self.module.name}:{self.word.name}'
        return result

    def get_parent(self) -> 'WordProfile':
        return self.parent

    def end_profile(self) -> None:
        self.end_time = time.perf_counter()

    def get_duration_s(self) -> Optional[float]:
        if self.end_time is None:
            return None

        result = self.end_time - self.start_time
        return result


class ProfileAnalyzer:
    """Prints a report for a WordProfile and allows navigation through the call tree

    This is meant to be used at the commandline when the interpreter is in debug mode:

        * print()       This prints the execution time of the current word as well as the words
                        called by it sorted by execution time, descending. The words are prefixed
                        by an index which represent the order in which the words were called.
        * up()          This drills up to the current word's parent and calls print()
        * down(index)   This drills down to a word at the specified index (see print()) and calls print()
    """

    def __init__(self, word_profile: WordProfile):
        self.word_profile: WordProfile = word_profile
        self.cur_profile: WordProfile = word_profile
        self.num_called: int = 10   # Limits number of called words to display

    def down(self, index: int) -> None:
        self.cur_profile = self.cur_profile.word_profiles[index]
        self.print()

    def up(self) -> None:
        self.cur_profile = self.cur_profile.get_parent()
        self.print()

    def print(self) -> None:
        duration = self.cur_profile.get_duration_s()
        if not duration:
            print("Nothing to report")
            return

        print(
            '%s: %.3f s'
            % (self.cur_profile.get_key(), duration)
        )
        for i, p in enumerate(self.cur_profile.word_profiles):
            p.index = i

        def get_duration(profile):
            res = profile.get_duration_s()
            if not res:
                res = 0
            return res

        def get_max_key_len(profiles):
            res = 0
            for p in profiles:
                key = p.get_key()
                if len(key) > res:
                    res = len(key)
            return res

        sorted_profiles = sorted(
            self.cur_profile.word_profiles, key=get_duration
        )
        sorted_profiles.reverse()
        format_string = (
            f'    [%d] %{get_max_key_len(sorted_profiles) + 1}s: %.3f s'
        )
        for p in sorted_profiles[: self.num_called]:
            print(format_string % (p.index, p.get_key(), get_duration(p)))
