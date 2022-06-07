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
        self.add_module_word('CWD!', self.word_CWD_bang)

        self.add_module_word('DATASET', self.word_DATASET)
        self.add_module_word('KEYS>DATA', self.word_KEYS_to_DATA)
        self.add_module_word('RECORDS', self.word_RECORDS)
        self.add_module_word('DATASET!', self.word_DATASET_bang)
        self.add_module_word('RECORDS!', self.word_RECORDS_bang)

        self.working_directory = None

    # ( path -- )
    def word_CWD_bang(self, interp: IInterpreter):
        path = interp.stack_pop()
        self.working_directory = path

    # ( dataset_label -- records )
    def word_DATASET(self, interp: IInterpreter):
        """Returns the records in a datset"""
        dataset_label = interp.stack_pop()

        filepath = self.dataset_filepath(dataset_label)

        dataset = self.load_dataset(filepath)
        result = list(dataset.values())
        interp.stack_push(result)

    # ( dataset_label data_keys -- records )
    # NOTE: NULL results are filtered from the returned records
    def word_KEYS_to_DATA(self, interp: IInterpreter):
        """Returns specific records from a dataset"""
        data_keys = interp.stack_pop()
        dataset_label = interp.stack_pop()

        filepath = self.dataset_filepath(dataset_label)

        dataset = self.load_dataset(filepath)
        result = []
        for key in data_keys:
            value = dataset.get(key)
            if value is not None:
                result.append(dataset.get(key))
        interp.stack_push(result)

    # ( dataset_label data_keys -- records )
    # NOTE: NULL results are *not* filtered from the returned records
    def word_RECORDS(self, interp: IInterpreter):
        """Returns specific records from a dataset"""
        data_keys = interp.stack_pop()
        dataset_label = interp.stack_pop()

        filepath = self.dataset_filepath(dataset_label)

        dataset = self.load_dataset(filepath)
        result = []
        for key in data_keys:
            result.append(dataset.get(key))
        interp.stack_push(result)

    # ( records fdata_key dataset_label -- )
    def word_DATASET_bang(self, interp: IInterpreter):
        """Overwrites a dataset with an array of records

        |fdata_key| is Forthic that returns a data key for a record
        |dataset_label| specifies the dataset to create/overwrite
        """
        dataset_label = interp.stack_pop()
        fdata_key = interp.stack_pop()
        records = interp.stack_pop()

        filepath = self.dataset_filepath(dataset_label)

        dataset = {}
        for r in records:
            data_key = self.rec_to_key(interp, r, fdata_key)
            dataset[data_key] = r

        self.write_dataset(filepath, dataset)

    # ( records fdata_key dataset_label -- )
    def word_RECORDS_bang(self, interp: IInterpreter):
        """Upserts records into a dataset"""
        dataset_label = interp.stack_pop()
        fdata_key = interp.stack_pop()
        records = interp.stack_pop()

        filepath = self.dataset_filepath(dataset_label)
        dataset = self.load_dataset(filepath)

        for r in records:
            data_key = self.rec_to_key(interp, r, fdata_key)
            dataset[data_key] = r

        self.write_dataset(filepath, dataset)

    # ----------------------------------------
    # Helpers
    def dataset_filepath(self, dataset_label: str) -> str:
        result = f'{self.working_directory}/datasets/{dataset_label}.dataset'
        return result

    def rec_to_key(self, interp: IInterpreter, rec: Any, fdata_key: str) -> str:
        interp.stack_push(rec)
        interp.run(fdata_key)
        res = interp.stack_pop()
        return res

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
["key" "dataset_label"] VARIABLES
: RECORD    (key ! dataset_label !) dataset_label @  [key @] RECORDS 0 NTH;

["RECORD"] EXPORT
'''
