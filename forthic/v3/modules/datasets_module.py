import os
import json
import threading
from ..module import Module
from ..interfaces import IInterpreter
from typing import Any


# From: https://www.oreilly.com/library/view/python-cookbook/0596001673/ch06s04.html
class ReadWriteLock:
    """A lock object that allows many simultaneous "read locks", but
    only one "write lock." """

    def __init__(self):
        self._read_ready = threading.Condition(threading.Lock())
        self._readers = 0

    def acquire_read(self):
        """Acquire a read lock. Blocks only if a thread has
        acquired the write lock."""
        self._read_ready.acquire()
        try:
            self._readers += 1
        finally:
            self._read_ready.release()

    def release_read(self):
        """ Release a read lock. """
        self._read_ready.acquire()
        try:
            self._readers -= 1
            if not self._readers:
                self._read_ready.notifyAll()
        finally:
            self._read_ready.release()

    def acquire_write(self):
        """Acquire a write lock. Blocks until there are no
        acquired read or write locks."""
        self._read_ready.acquire()
        while self._readers > 0:
            self._read_ready.wait()

    def release_write(self):
        """ Release a write lock. """
        self._read_ready.release()


DATASETS_LOCK = ReadWriteLock()


class DatasetsModule(Module):
    """This implements a simple file-based storage of datasets

    This reads/writes/upserts arrays of records as coherent datasets.

    See `docs/modules/datasets_module.md` for detailed descriptions of each word.
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('datasets', interp, DATASETS_FORTHIC)
        self.working_directory = None
        self.flags = {
            "overwrite": None,
            "drop_nulls": None,
        }

        self.add_module_word('CWD!', self.word_CWD_bang)

        self.add_module_word('DATASET!', self.word_DATASET_bang)
        self.add_module_word('DATASET', self.word_DATASET)
        self.add_module_word('RECORDS', self.word_RECORDS)

        # Flag words
        self.add_module_word('!OVERWRITE', self.word_bang_OVERWRITE)
        self.add_module_word('!DROP-NULLS', self.word_bang_DROP_NULLS)

    # ( path -- )
    def word_CWD_bang(self, interp: IInterpreter):
        path = interp.stack_pop()
        self.working_directory = path

    # ( record dataset_label -- )
    def word_DATASET_bang(self, interp: IInterpreter):
        """Updates a dataset

        If !OVERWRITE is set, then this overwrites the dataset. Otherwise, the data is merged.
        """
        dataset_label = interp.stack_pop()
        record = interp.stack_pop()
        flags = self.get_flags()

        filepath = self.dataset_filepath(dataset_label)

        if flags.get("overwrite"):
            self.write_dataset(filepath, record)
        else:
            dataset = self.load_dataset(filepath)
            for k, v in record.items():
                dataset[k] = v
            self.write_dataset(filepath, dataset)

    # ( dataset_label -- dataset )
    def word_DATASET(self, interp: IInterpreter):
        """Loads a dataset
        """
        dataset_label = interp.stack_pop()

        filepath = self.dataset_filepath(dataset_label)
        result = self.load_dataset(filepath)
        interp.stack_push(result)

    # ( dataset_label keys -- records )
    def word_RECORDS(self, interp: IInterpreter):
        """Loads records from a dataset
        """
        keys = interp.stack_pop()
        dataset_label = interp.stack_pop()
        flags = self.get_flags()

        filepath = self.dataset_filepath(dataset_label)
        dataset = self.load_dataset(filepath)
        result = []
        for k in keys:
            value = dataset.get(k)
            if flags.get('drop_nulls') and value is None:
                pass
            else:
                result.append(value)
        interp.stack_push(result)

    # ( -- )
    def word_bang_OVERWRITE(self, interp: IInterpreter):
        self.flags["overwrite"] = True

    # ( -- )
    def word_bang_DROP_NULLS(self, interp: IInterpreter):
        self.flags["drop_nulls"] = True

    # ----------------------------------------
    # Helpers

    def get_flags(self):
        flags = self.flags.copy()
        self.flags = {}
        return flags

    def dataset_filepath(self, dataset_label: str) -> str:
        result = f'{self.working_directory}/datasets/{dataset_label}.dataset'
        return result

    def load_dataset(self, filepath: str) -> Any:
        result = {}
        DATASETS_LOCK.acquire_read()
        try:
            self.ensure_dirpath(filepath)
            if not os.path.isfile(filepath):
                return {}

            with open(filepath, 'r') as f:
                content = f.read().strip()
                if content:
                    result = json.loads(content)
                else:
                    result = {}
        finally:
            DATASETS_LOCK.release_read()
        return result

    def write_dataset(self, filepath: str, dataset: Any) -> None:
        DATASETS_LOCK.acquire_write()
        try:
            self.ensure_dirpath(filepath)
            with open(filepath, 'w') as f:
                f.write(json.dumps(dataset, indent=4, separators=(',', ': ')))
        finally:
            DATASETS_LOCK.release_write()

    def ensure_dirpath(self, filepath: str) -> None:
        if not os.path.exists(os.path.dirname(filepath)):
            os.makedirs(os.path.dirname(filepath))


DATASETS_FORTHIC = '''
'''
