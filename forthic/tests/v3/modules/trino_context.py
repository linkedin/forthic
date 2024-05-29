import json
from forthic.v3.modules.trino_module import TrinoContext


class TestDataFrame:
    def __init__(self, json_data):
        self.json_data = json_data

    def to_json(self):
        return self.json_data


class TrinoTestContext(TrinoContext):
    def connect(self):
        pass

    def close(self):
        pass

    def query(self, _):
        result = TestDataFrame(QUERY_RESPONSE)
        return result


QUERY_RESPONSE = '''
{
    "test_key": {
        "0": "alpha",
        "1": "beta",
        "2": "gamma"
    },
    "priority": {
        "0": 4,
        "1": 3,
        "2": 4
    },
    "name": {
        "0": "Test1",
        "1": "Test2",
        "2": "Test3"
    },
    "country": {
        "0": "Tanzania",
        "1": "Nicaragua",
        "2": "Costa Rica"
    },
    "metric": {
        "0": "runners",
        "1": "hikers",
        "2": "runners"
    },
    "sample": {
        "0": 12345,
        "1": 23456,
        "2": 34567
    },
    "confidence": {
        "0": 0.91,
        "1": 0.92,
        "2": 0.93
    },
    "owners": {
        "0": "['user1', 'user2']",
        "1": "['user2', 'user3']",
        "2": "['user3', 'user4']"
    }
}
'''
