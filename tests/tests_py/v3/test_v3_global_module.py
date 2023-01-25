import unittest
import datetime
import pytz
from forthic.v3.interpreter import Interpreter, UnknownWordError
from forthic.v3.tokenizer import DLE
from forthic.v3.global_module import GlobalModuleError



class TestGlobalModule(unittest.TestCase):
    def test_literal(self):
        interp = Interpreter()
        interp.run("TRUE FALSE 2  3.14 2020-06-05 9:00 11:30 PM 22:15 AM")
        self.assertEqual(interp.stack[0], True)
        self.assertEqual(interp.stack[1], False)
        self.assertEqual(interp.stack[2], 2)
        self.assertEqual(interp.stack[3], 3.14)
        self.assertEqual(interp.stack[4], datetime.date(2020, 6, 5))
        self.assertEqual(interp.stack[5], datetime.time(9, 0))
        self.assertEqual(interp.stack[6], datetime.time(23, 30))
        self.assertEqual(interp.stack[7], datetime.time(10, 15))

    def test_variables(self):
        interp = Interpreter()
        interp.run("['x' 'y']  VARIABLES")
        variables = interp.app_module.variables
        self.assertIsNotNone(variables.get('x'))
        self.assertIsNotNone(variables.get('y'))

    def test_set_get_variables(self):
        interp = Interpreter()
        interp.run("['x']  VARIABLES")
        interp.run("24 x !")
        x_var = interp.app_module.variables['x']

        self.assertEqual(x_var.get_value(), 24)

        interp.run("x @")
        self.assertEqual(interp.stack[-1], 24)

    def test_bang_at(self):
        interp = Interpreter()
        interp.run("['x']  VARIABLES")
        interp.run("24 x !@")
        x_var = interp.app_module.variables['x']

        self.assertEqual(x_var.get_value(), 24)
        self.assertEqual(interp.stack[-1], 24)

    def test_interpret(self):
        interp = Interpreter()
        interp.run("'24' INTERPRET")

        self.assertEqual(interp.stack[-1], 24)

        interp.run("""'{module-A  : MESSAGE   "Hi" ;}' INTERPRET""")
        interp.run("{module-A MESSAGE}")
        self.assertEqual(interp.stack[-1], 'Hi')

    def test_memo(self):
        interp = Interpreter()
        interp.run("""
        ['count'] VARIABLES
        0 count !
        @: COUNT   count @ 1 +  count !  count @;
        """)

        interp.run("COUNT")
        self.assertEqual(interp.stack[-1], 1)

        interp.run("COUNT")
        self.assertEqual(interp.stack[-1], 1)

        interp.run("COUNT! COUNT")
        self.assertEqual(interp.stack[-1], 2)
        self.assertEqual(len(interp.stack), 3)

        interp.run("COUNT!@")
        self.assertEqual(interp.stack[-1], 3)

    def test_rec(self):
        interp = Interpreter()
        interp.run("""
        [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
        """)

        self.assertEqual(len(interp.stack), 1)

        rec = interp.stack[-1]
        self.assertEqual(rec["alpha"], 2)
        self.assertEqual(rec["gamma"], 4)

    def test_rec_at(self):
        interp = Interpreter()
        interp.run("""
        [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
        'beta' REC@
        """)
        self.assertEqual(len(interp.stack), 1)
        self.assertEqual(interp.stack[0], 3)

        interp.run("""
        [10 20 30 40 50] 3 REC@
        """)
        self.assertEqual(interp.stack[-1], 40)

    def test_nested_rec_at(self):
        interp = Interpreter()
        interp.run("""
        [ ["alpha" [["alpha1" 20]] REC]
          ["beta" [["beta1"  30]] REC]
        ] REC
        ["beta" "beta1"] REC@
        """)
        self.assertEqual(interp.stack[-1], 30)

        interp.run("""
        [ [] [] [[3]] ]
        [2 0 0] REC@
        """)
        self.assertEqual(interp.stack[-1], 3)

        interp.run("""
        [ ["alpha" [["alpha1" 20]] REC]
          ["beta" [["beta1"  [10 20 30]]] REC]
        ] REC
        ["beta" "beta1" 1] REC@
        """)
        self.assertEqual(interp.stack[-1], 20)

    def test_l_rec_bang(self):
        # Case: Set value on a record
        interp = Interpreter()
        interp.run("""
        [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
        700 'beta' <REC! 'beta' REC@
        """)
        self.assertEqual(len(interp.stack), 1)
        self.assertEqual(interp.stack[0], 700)

        # Case: Set a nested value
        interp = Interpreter()
        interp.run("""
        [] REC "Green" ["2021-03-22" "TEST-1234"] <REC! ["2021-03-22" "TEST-1234"] REC@
        """)
        self.assertEqual(len(interp.stack), 1)
        self.assertEqual(interp.stack[0], "Green")

        # Case: Set value on a NULL
        interp = Interpreter()
        interp.run("""
        NULL 700 'beta' <REC! 'beta' REC@
        """)
        self.assertEqual(len(interp.stack), 1)
        self.assertEqual(interp.stack[0], 700)

    def test_SCREEN_bang(self):
        interp = Interpreter()
        interp.run("""
        'Screen content' 'my-screen' SCREEN!
        """)
        self.assertEqual('Screen content', interp.app_module.get_screen('my-screen'))

    def test_SCREEN(self):
        interp = Interpreter()
        interp.run("""
        'Screen content' 'my-screen' SCREEN!
        'my-screen' SCREEN
        """)
        self.assertEqual(interp.stack[0], 'Screen content')

    def test_LOAD_SCREEN(self):
        # Test normal load
        interp = Interpreter()
        interp.run("""
        ': MESSAGE   "Howdy!";' 'message' SCREEN!
        'message' LOAD-SCREEN
        MESSAGE
        """)
        self.assertEqual(interp.stack[0], 'Howdy!')

        # Test that recursive loads are prevented
        def load_recursive_screen():
            interp = Interpreter()
            interp.run("""
            ': MESSAGE   "Howdy!";  "message" LOAD-SCREEN' 'message' SCREEN!
            'message' LOAD-SCREEN
            """)

        self.assertRaises(GlobalModuleError, load_recursive_screen)

    def test_append(self):
        # Test append to array
        interp = Interpreter()
        interp.run("""
        [ 1 2 3 ] 4 APPEND
        """)
        self.assertEqual(len(interp.stack), 1)

        array = interp.stack[-1]
        self.assertEqual(array, [1, 2, 3, 4])

        # Test append to record
        interp = Interpreter()
        interp.run("""
        [["a" 1] ["b" 2]] REC  ["c" 3] APPEND
        """)
        self.assertEqual(len(interp.stack), 1)

        rec = interp.stack[-1]
        values = [rec[k] for k in ["a", "b", "c"]]
        self.assertEqual(values, [1, 2, 3])

    def test_reverse(self):
        interp = Interpreter()
        interp.run("""
        [ 1 2 3 ] REVERSE
        """)
        self.assertEqual(len(interp.stack), 1)

        array = interp.stack[-1]
        self.assertEqual(array, [3, 2, 1])

    def test_unique(self):
        interp = Interpreter()
        interp.run("""
        [ 1 2 3 3 2 ] UNIQUE
        """)

        array = interp.stack[-1]
        self.assertEqual(array, [1, 2, 3])

        interp = Interpreter()
        interp.run("""
        [["a" 1] ["b" 2] ["c" 2] ["d" 1]] REC  UNIQUE
        """)
        rec = interp.stack[-1]
        self.assertEqual(sorted(rec.values()), [1, 2])

    def test_del(self):
        interp = Interpreter()
        interp.run("""
        [ "a" "b" "c" ] 1 <DEL
        """)

        array = interp.stack[-1]
        self.assertEqual(array, ["a", "c"])

        interp = Interpreter()
        interp.run("""
        [["a" 1] ["b" 2] ["c" 3]] REC  "b" <DEL
        """)
        rec = interp.stack[-1]
        self.assertEqual(sorted(rec.keys()), ["a", "c"])

        interp = Interpreter()
        interp.run("""
        [["a" 1] ["b" 2] ["c" 3]] REC  "d" <DEL
        """)
        rec = interp.stack[-1]
        self.assertEqual(sorted(rec.keys()), ["a", "b", "c"])

    def test_relabel(self):
        interp = Interpreter()
        interp.run("""
        [ "a" "b" "c" ] [0 2] [25 23] RELABEL
        """)

        self.assertEqual(len(interp.stack), 1)
        array = interp.stack[0]
        self.assertEqual(array, ["c", "a"])

        interp = Interpreter()
        interp.run("""
        [["a" 1] ["b" 2] ["c" 3]] REC  ["a" "c"] ["alpha" "gamma"] RELABEL
        """)
        self.assertEqual(len(interp.stack), 1)
        rec = interp.stack[0]
        self.assertEqual(sorted(rec.keys()), ["alpha", "gamma"])
        self.assertEqual([rec[k] for k in ["alpha", "gamma"]], [1, 3])

    def make_records(self):
        result = []
        data = [[100, "user1", "OPEN"],
                [101, "user1", "OPEN"],
                [102, "user1", "IN PROGRESS"],
                [103, "user1", "CLOSED"],
                [104, "user2", "IN PROGRESS"],
                [105, "user2", "OPEN"],
                [106, "user2", "CLOSED"]]
        for d in data:
            rec = {"key": d[0], "assignee": d[1], "status": d[2]}
            result.append(rec)
        return result

    def make_status_to_manager_to_ids(self):
        result = {
            "open": {
                "manager1": [101, 102],
                "manager2": [103]
            },
            "blocked": {
                "manager3": [104]
            },
            "closed": {
                "manager1": [10, 11],
                "manager2": [12, 13]
            }
        }
        return result

    def test_by_field(self):
        interp = Interpreter()
        interp.stack_push(self.make_records())
        interp.run("'key' BY-FIELD")
        grouped = interp.stack[0]
        self.assertEqual(grouped[104]['status'], "IN PROGRESS")

    def test_by_field_with_nulls(self):
        interp = Interpreter()
        interp.stack_push(self.make_records() + [None, None])
        interp.run("'key' BY-FIELD")
        grouped = interp.stack[0]
        self.assertEqual(grouped[104]['status'], "IN PROGRESS")

    def test_group_by_field(self):
        interp = Interpreter()
        interp.stack_push(self.make_records())
        interp.run("'assignee' GROUP-BY-FIELD")
        grouped = interp.stack[0]
        self.assertEqual(sorted(grouped.keys()), ["user1", "user2"])
        self.assertEqual(len(grouped["user1"]), 4)
        self.assertEqual(len(grouped["user2"]), 3)

        # Test grouping a record
        interp = Interpreter()

        # First, set up the record
        records = self.make_records()
        by_key = {}
        for rec in records:
            by_key[rec["key"]] = rec
        interp.stack_push(by_key)

        # Now group a record
        interp.run("'assignee' GROUP-BY-FIELD")
        grouped_rec = interp.stack[0]
        self.assertEqual(sorted(grouped_rec.keys()), ["user1", "user2"])
        self.assertEqual(len(grouped_rec["user1"]), 4)
        self.assertEqual(len(grouped_rec["user2"]), 3)
        self.assertEqual(grouped, grouped_rec)

        # Test grouping a list-valued field
        interp.stack_push([{"id": 1, "attrs":["blue", "important"]}, {"id": 2, "attrs":["red"]}])
        interp.run("'attrs' GROUP-BY-FIELD")
        grouped_rec = interp.stack[-1]
        self.assertEqual(1, grouped_rec["blue"][0]["id"])
        self.assertEqual(1, grouped_rec["important"][0]["id"])
        self.assertEqual(2, grouped_rec["red"][0]["id"])

    def test_group_by(self):
        interp = Interpreter()
        interp.stack_push(self.make_records())
        interp.run("""
        "'assignee' REC@" GROUP-BY
        """)
        grouped = interp.stack[0]
        self.assertEqual(sorted(grouped.keys()), ["user1", "user2"])
        self.assertEqual(len(grouped["user1"]), 4)
        self.assertEqual(len(grouped["user2"]), 3)

        # Test grouping a record
        interp = Interpreter()

        # First, set up the record
        records = self.make_records()
        by_key = {}
        for rec in records:
            by_key[rec["key"]] = rec
        interp.stack_push(by_key)

        # Now group a record
        interp.run("""
        "'assignee' REC@" GROUP-BY
        """)
        grouped_rec = interp.stack[0]
        self.assertEqual(sorted(grouped_rec.keys()), ["user1", "user2"])
        self.assertEqual(len(grouped_rec["user1"]), 4)
        self.assertEqual(len(grouped_rec["user2"]), 3)
        self.assertEqual(grouped, grouped_rec)

    def test_group_by_w_key(self):
        interp = Interpreter()
        interp.stack_push(self.make_records())
        interp.run("""
        ['key' 'val'] VARIABLES
        "val ! key ! key @ 3 MOD" !WITH-KEY GROUP-BY
        """)
        grouped = interp.stack[0]
        self.assertEqual(sorted(grouped.keys()), [0, 1, 2])
        self.assertEqual(len(grouped[0]), 3)
        self.assertEqual(len(grouped[1]), 2)
        self.assertEqual(len(grouped[2]), 2)

        # Test grouping a record
        interp = Interpreter()

        # First, set up the record
        records = self.make_records()
        by_key = {}
        for rec in records:
            by_key[rec["key"]] = rec
        interp.stack_push(by_key)

        # Now group a record
        interp.run("""
        ['key' 'val'] VARIABLES
        "val ! key ! key @ 2 *" !WITH-KEY GROUP-BY
        """)
        grouped_rec = interp.stack[0]
        self.assertEqual(sorted(list(grouped_rec.keys())), [200, 202, 204, 206, 208, 210, 212])

    def test_groups_of(self):
        interp = Interpreter()
        interp.run("""
        [1 2 3 4 5 6 7 8] 3 GROUPS-OF
        """)
        groups = interp.stack[0]
        self.assertEqual(groups[0], [1, 2, 3])
        self.assertEqual(groups[1], [4, 5, 6])
        self.assertEqual(groups[2], [7, 8])

        # Test grouping a record
        interp = Interpreter()

        # First, set up the record
        records = self.make_records()
        by_key = {}
        for rec in records:
            by_key[rec["key"]] = rec
        interp.stack_push(by_key)

        # Now group a record
        interp.run("""
        3 GROUPS-OF
        """)
        recs = interp.stack[0]
        self.assertEqual(len(recs[0]), 3)
        self.assertEqual(len(recs[1]), 3)
        self.assertEqual(len(recs[2]), 1)

    def test_INDEX(self):
        interp = Interpreter()
        interp.run("""
        : |KEYS   "'key' REC@" MAP;
        : TICKETS [
            [['key'   101] ['Labels'  ['alpha' 'beta']]] REC
            [['key'   102] ['Labels'  ['alpha' 'gamma']]] REC
            [['key'   103] ['Labels'  ['alpha']]] REC
            [['key'   104] ['Labels'  ['beta']]] REC
        ];

        TICKETS "'Labels' REC@" INDEX  "|KEYS" MAP
        """)
        index_record = interp.stack[0]
        self.assertEqual(index_record['alpha'], [101, 102, 103])
        self.assertEqual(index_record['beta'], [101, 104])
        self.assertEqual(index_record['gamma'], [102])

    def test_map(self):
        interp = Interpreter()
        interp.run("""
        [1 2 3 4 5] '2 *' MAP
        """)
        array = interp.stack[0]
        self.assertEqual(array, [2, 4, 6, 8, 10])

        # Test mapping over a record
        interp = Interpreter()

        # First, set up the record
        records = self.make_records()
        by_key = {}
        for rec in records:
            by_key[rec["key"]] = rec
        interp.stack_push(by_key)

        interp.run("""
        "'status' REC@" MAP
        """)
        record = interp.stack[0]
        self.assertEqual(record[100], "OPEN")
        self.assertEqual(record[102], "IN PROGRESS")
        self.assertEqual(record[106], "CLOSED")

        # Test map in module
        interp.run("""
        {my-module
            : DOUBLE   2 *;
            : RUN   [1 2 3 4 5] "DOUBLE" MAP;
        }
        {my-module RUN}
        """)
        array = interp.stack[-1]
        self.assertEqual(array, [2, 4, 6, 8, 10])

    def test_map_depth(self):
        interp = Interpreter()
        interp.run('''
        : k1-REC   [
            ["l1"  [["m"  2]] REC]
            ["l2"  [["m"  3]] REC]
        ] REC;

        : k2-REC   [
            ["l1"  [["m"  3]] REC]
            ["l2"  [["m"  4]] REC]
        ] REC;

        : DEEP-RECORD [
            ["k1"  k1-REC]
            ["k2"  k2-REC]
        ] REC;

        DEEP-RECORD "2 *" 2 !DEPTH MAP
        # {'k1': {'l1': {'m': 4}, 'l2': {'m': 6}}, 'k2': {'l1': {'m': 6}, 'l2': {'m': 8}}}
        ''')
        record = interp.stack[-1]
        self.assertDictEqual(record, {'k1': {'l1': {'m': 4}, 'l2': {'m': 6}}, 'k2': {'l1': {'m': 6}, 'l2': {'m': 8}}})

    def test_map_depth_over_array(self):
        interp = Interpreter()
        interp.run('''
        : DEEP-LIST [ [ [[["m"  2]] REC [["m"  3]] REC] ] [ [[["m"  3]] REC [["m"  4]] REC] ] ];

        DEEP-LIST "2 *" 3 !DEPTH MAP >JSON
        # [ [ [4 6] ] [ [6 8] ] ]
        ''')
        array_json = interp.stack[-1]
        self.assertEqual(array_json, '[[[{"m": 4}, {"m": 6}]], [[{"m": 6}, {"m": 8}]]]')

    def test_map_depth_over_array_of_maps(self):
        interp = Interpreter()
        interp.run('''
        : DEEP-LIST [ [ [2 3] ] [ [3 4] ] ];

        DEEP-LIST "2 *" 2 !DEPTH MAP
        ''')
        array = interp.stack[-1]
        self.assertEqual(array, [ [ [4, 6] ], [ [6, 8] ] ])

    def test_map_depth_w_error(self):
        interp = Interpreter()
        interp.run('''
        : k1-REC   [
            ["l1"  [["m"  2]] REC]
            ["l2"  [["m"  3]] REC]
        ] REC;

        : k2-REC   [
            ["l1"  [["m"  'GARBAGE']] REC]
            ["l2"  [["m"  4]] REC]
        ] REC;

        : DEEP-RECORD [
            ["k1"  k1-REC]
            ["k2"  k2-REC]
        ] REC;

        DEEP-RECORD ">STR INTERPRET" 2 !DEPTH !PUSH-ERROR MAP
        # {'k1': {'l1': {'m': 2}, 'l2': {'m': 3}}, 'k2': {'l1': {'m': None}, 'l2': {'m': 4}}}
        ''')
        errors = interp.stack[-1]
        record = interp.stack[-2]
        self.assertDictEqual(record, {'k1': {'l1': {'m': 2}, 'l2': {'m': 3}}, 'k2': {'l1': {'m': None}, 'l2': {'m': 4}}})
        self.assertEqual([str(e) for e in errors], ['None', 'None', "Unknown word: 'GARBAGE'", 'None'])

    def test_map_w_key(self):
        interp = Interpreter()
        interp.run("""
        [1 2 3 4 5] '+ 2 *' !WITH-KEY MAP
        """)
        array = interp.stack[0]
        self.assertEqual(array, [2, 6, 10, 14, 18])

        # Test mapping over a record
        interp = Interpreter()

        # First, set up the record
        records = self.make_records()
        by_key = {}
        for rec in records:
            by_key[rec["key"]] = rec
        interp.stack_push(by_key)

        interp.run("""
        ["k" "v"] VARIABLES
        "v ! k ! k @ >STR v @ 'status' REC@ CONCAT" !WITH-KEY MAP
        """)
        record = interp.stack[0]
        self.assertEqual(record[100], "100OPEN")
        self.assertEqual(record[102], "102IN PROGRESS")
        self.assertEqual(record[106], "106CLOSED")

    def test_foreach(self):
        interp = Interpreter()
        interp.run("""
        0 [1 2 3 4 5] '+' FOREACH
        """)
        sum = interp.stack[0]
        self.assertEqual(sum, 15)

        # Test grouping a record
        interp = Interpreter()

        # First, set up the record
        records = self.make_records()
        by_key = {}
        for rec in records:
            by_key[rec["key"]] = rec
        interp.stack_push(by_key)

        interp.run("""
        "" SWAP "'status' REC@ CONCAT" FOREACH
        """)
        string = interp.stack[0]
        self.assertEqual(string, "OPENOPENIN PROGRESSCLOSEDIN PROGRESSOPENCLOSED")

    def test_foreach_w_key(self):
        interp = Interpreter()
        interp.run("""
        0 [1 2 3 4 5] '+ +' !WITH-KEY FOREACH
        """)
        sum = interp.stack[0]
        self.assertEqual(sum, 25)

        # Test grouping a record
        interp = Interpreter()

        # First, set up the record
        records = self.make_records()
        by_key = {}
        for rec in records:
            by_key[rec["key"]] = rec
        interp.stack_push(by_key)

        interp.run("""
        "" SWAP "'status' REC@ CONCAT CONCAT" !WITH-KEY FOREACH
        """)
        string = interp.stack[0]
        self.assertEqual(string, "100OPEN101OPEN102IN PROGRESS103CLOSED104IN PROGRESS105OPEN106CLOSED")

    def test_foreach_to_errors(self):
        interp = Interpreter()
        interp.run("""
        ['2' '3' 'GARBAGE' '+'] 'INTERPRET' !PUSH-ERROR FOREACH
        """)
        errors = interp.stack[-1]
        self.assertIsNone(errors[0])
        self.assertIsNone(errors[1])
        self.assertIsNotNone(errors[2])
        self.assertIsNone(errors[3])
        res = interp.stack[-2]
        self.assertEqual(res, 5)

    def test_invert_keys(self):
        interp = Interpreter()
        status_to_manager_to_ids = self.make_status_to_manager_to_ids()
        interp.stack_push(status_to_manager_to_ids)
        interp.run("INVERT-KEYS")
        res = interp.stack_pop()
        expected = {
            "manager1": {
                "open": [101, 102],
                "closed": [10, 11]
            },
            "manager2": {
                "open": [103],
                "closed": [12, 13]
            },
            "manager3": {
                "blocked": [104]
            }
        }

        self.assertEqual(res, expected)

    def test_zip(self):
        interp = Interpreter()
        interp.run("""
        ['a' 'b'] [1 2] ZIP
        """)
        array = interp.stack[0]
        self.assertEqual(array[0], ['a', 1])
        self.assertEqual(array[1], ['b', 2])

        # Zip a record
        interp = Interpreter()

        # First, set up the record
        interp.run("""
        [['a' 100] ['b' 200] ['z' 300]] REC [['a' 'Hi'] ['b' 'Bye'] ['c' '?']] REC ZIP
        """)
        record = interp.stack[0]
        self.assertEqual(sorted(record.keys()), ['a', 'b', 'z'])
        self.assertEqual(record['a'], [100, 'Hi'])
        self.assertEqual(record['b'], [200, 'Bye'])
        self.assertEqual(record['z'], [300, None])

    def test_zip_with(self):
        interp = Interpreter()
        interp.run("""
        [10 20] [1 2] "+" ZIP-WITH
        """)
        array = interp.stack[0]
        self.assertEqual(array[0], 11)
        self.assertEqual(array[1], 22)

        # Zip a record
        interp = Interpreter()

        # First, set up the record
        interp.run("""
        [['a' 1] ['b' 2]] REC [['a' 10] ['b' 20]] REC "+" ZIP-WITH
        """)
        record = interp.stack[0]
        self.assertEqual(sorted(record.keys()), ['a', 'b'])
        self.assertEqual(record['a'], 11)
        self.assertEqual(record['b'], 22)

    def test_keys(self):
        interp = Interpreter()
        interp.run("""
        ['a' 'b' 'c'] KEYS
        """)
        array = interp.stack[0]
        self.assertEqual(array, [0, 1, 2])

        # Test record
        interp = Interpreter()

        # First, set up the record
        interp.run("""
        [['a' 1] ['b' 2]] REC KEYS
        """)
        array = interp.stack[0]
        self.assertEqual(sorted(array), ['a', 'b'])

    def test_values(self):
        interp = Interpreter()
        interp.run("""
        ['a' 'b' 'c'] VALUES
        """)
        array = interp.stack[0]
        self.assertEqual(array, ['a', 'b', 'c'])

        # Test record
        interp = Interpreter()

        # First, set up the record
        interp.run("""
        [['a' 1] ['b' 2]] REC VALUES
        """)
        array = interp.stack[0]
        self.assertEqual(sorted(array), [1, 2])

    def test_length(self):
        interp = Interpreter()
        interp.run("""
        ['a' 'b' 'c'] LENGTH
        "Howdy" LENGTH
        """)
        self.assertEqual(interp.stack[0], 3)
        self.assertEqual(interp.stack[1], 5)

        # Test record
        interp = Interpreter()

        interp.run("""
        [['a' 1] ['b' 2]] REC LENGTH
        """)
        length = interp.stack[0]
        self.assertEqual(length, 2)

    def test_RANGE(self):
        interp = Interpreter()
        interp.run("""
        : EVEN?   2 MOD  0 ==;
        : ODD?    2 MOD  1 ==;
        [1 2 3 4 5] "EVEN?" "ODD?" RANGE
        """)
        self.assertEqual(interp.stack[0], [1, 2])

        # Test record
        interp = Interpreter()

        interp.run("""
        [['a' 1] ['b' 2]] REC LENGTH
        """)
        length = interp.stack[0]
        self.assertEqual(length, 2)

    def test_slice(self):
        interp = Interpreter()
        interp.run("""
        ['x'] VARIABLES
        ['a' 'b' 'c' 'd' 'e' 'f' 'g'] x !
        x @ 0 2 SLICE
        x @ 1 3 SLICE
        x @ 5 3 SLICE
        x @ -1 -2 SLICE
        x @ 4 -2 SLICE
        x @ 5 8 SLICE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], ['a', 'b', 'c'])
        self.assertEqual(stack[1], ['b', 'c', 'd'])
        self.assertEqual(stack[2], ['f', 'e', 'd'])
        self.assertEqual(stack[3], ['g', 'f'])
        self.assertEqual(stack[4], ['e', 'f'])
        self.assertEqual(stack[5], ['f', 'g', None, None])

        # Slice records
        interp = Interpreter()
        interp.run("""
        ['x'] VARIABLES
        [['a' 1] ['b' 2] ['c' 3]] REC x !
        x @ 0 1 SLICE
        x @ -1 -2 SLICE
        x @ 5 7 SLICE
        """)
        stack = interp.stack
        self.assertEqual(sorted(list(stack[0].keys())), ['a', 'b'])
        self.assertEqual(sorted(list(stack[1].keys())), ['b', 'c'])
        self.assertEqual(stack[2], {})

    def test_difference(self):
        interp = Interpreter()
        interp.run("""
        ['x' 'y'] VARIABLES
        ['a' 'b' 'c'] x !
        ['a' 'c' 'd'] y !
        x @ y @ DIFFERENCE
        y @ x @ DIFFERENCE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], ['b'])
        self.assertEqual(stack[1], ['d'])

        # Records
        interp = Interpreter()
        interp.run("""
        ['x' 'y'] VARIABLES
        [['a' 1] ['b' 2] ['c' 3]] REC x !
        [['a' 20] ['c' 40] ['d' 10]] REC y !
        x @ y @ DIFFERENCE
        y @ x @ DIFFERENCE
        """)
        stack = interp.stack
        self.assertEqual(list(stack[0].keys()), ['b'])
        self.assertEqual(list(stack[0].values()), [2])
        self.assertEqual(list(stack[1].keys()), ['d'])
        self.assertEqual(list(stack[1].values()), [10])

    def test_intersection(self):
        interp = Interpreter()
        interp.run("""
        ['x' 'y'] VARIABLES
        ['a' 'b' 'c'] x !
        ['a' 'c' 'd'] y !
        x @ y @ INTERSECTION
        """)
        stack = interp.stack
        self.assertEqual(sorted(stack[0]), ['a', 'c'])

        # Records
        interp = Interpreter()
        interp.run("""
        ['x' 'y'] VARIABLES
        [['a' 1] ['b' 2] ['f' 3]] REC x !
        [['a' 20] ['c' 40] ['d' 10]] REC y !
        x @ y @ INTERSECTION
        """)
        stack = interp.stack
        self.assertEqual(list(stack[0].keys()), ['a'])
        self.assertEqual(list(stack[0].values()), [1])

    def test_UNION(self):
        interp = Interpreter()
        interp.run("""
        ['x' 'y'] VARIABLES
        ['a' 'b' 'c'] x !
        ['a' 'c' 'd'] y !
        x @ y @ UNION
        """)
        stack = interp.stack
        self.assertEqual(sorted(stack[0]), ['a', 'b', 'c', 'd'])

        # Records
        interp = Interpreter()
        interp.run("""
        ['x' 'y'] VARIABLES
        [['a' 1] ['b' 2] ['f' 3]] REC x !
        [['a' 20] ['c' 40] ['d' 10]] REC y !
        x @ y @ UNION
        """)
        stack = interp.stack
        self.assertEqual(sorted(list(stack[0].keys())), ['a', 'b', 'c', 'd', 'f'])
        self.assertEqual(sorted(list(stack[0].values())), [1, 2, 3, 10, 40])

    def test_select(self):
        interp = Interpreter()
        interp.run("""
        [0 1 2 3 4 5 6] "2 MOD 1 ==" SELECT
        """)
        stack = interp.stack
        self.assertEqual(stack[0], [1, 3, 5])

        # Slice records
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] REC  "2 MOD 0 ==" SELECT
        """)
        stack = interp.stack
        self.assertEqual(list(stack[0].keys()), ['b'])
        self.assertEqual(list(stack[0].values()), [2])

    def test_select_w_key(self):
        interp = Interpreter()
        interp.run("""
        [0 1 2 3 4 5 6] "+ 3 MOD 1 ==" !WITH-KEY SELECT
        """)
        stack = interp.stack
        self.assertEqual(stack[0], [2, 5])

        # Slice records
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] REC  "CONCAT 'c3' ==" !WITH-KEY SELECT
        """)
        stack = interp.stack
        self.assertEqual(list(stack[0].keys()), ['c'])
        self.assertEqual(list(stack[0].values()), [3])

    def test_take(self):
        interp = Interpreter()
        interp.run("""
        [0 1 2 3 4 5 6] 3 TAKE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], [0, 1, 2])

        # Take records
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] REC  2 TAKE
        """)
        stack = interp.stack
        self.assertEqual(len(stack[0]), 2)

    def test_take_with_rest(self):
        interp = Interpreter()
        interp.run("""
        [0 1 2 3 4 5 6] 3 !PUSH-REST TAKE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], [0, 1, 2])
        self.assertEqual(stack[1], [3, 4, 5, 6])

        # Take records
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] REC  2 !PUSH-REST TAKE
        """)
        stack = interp.stack
        self.assertEqual(len(stack[0]), 2)
        self.assertEqual(len(stack[1]), 1)


    def test_drop(self):
        interp = Interpreter()
        interp.run("""
        [0 1 2 3 4 5 6] 4 DROP
        """)
        stack = interp.stack
        self.assertEqual(stack[0], [4, 5, 6])

        # Drop records
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] REC  2 DROP
        """)
        stack = interp.stack
        self.assertEqual(len(stack[0]), 1)

    def test_rotate(self):
        interp = Interpreter()
        interp.run("""
        ['a' 'b' 'c' 'd'] ROTATE
        ['b'] ROTATE
        [] ROTATE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], ['d', 'a', 'b', 'c'])
        self.assertEqual(stack[1], ['b'])
        self.assertEqual(stack[2], [])

    def test_shuffle(self):
        interp = Interpreter()
        interp.run("""
        [0 1 2 3 4 5 6] SHUFFLE
        """)
        stack = interp.stack
        self.assertEqual(len(stack[0]), 7)

        # Shuffle record (no-op)
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] REC  SHUFFLE
        """)
        stack = interp.stack
        self.assertEqual(len(stack[0]), 3)

    def test_sort(self):
        interp = Interpreter()
        interp.run("""
        [2 8 1 4 7 3] SORT
        """)
        stack = interp.stack
        self.assertEqual(stack[0], [1, 2, 3, 4, 7, 8])

        # Sort record
        interp = Interpreter()
        interp.run("""
        [['a' 3] ['b' 1] ['c' 2]] REC  SORT
        """)
        stack = interp.stack
        self.assertEqual(len(stack[0]), 3)
        self.assertEqual(list(stack[0].keys()), ['b', 'c', 'a'])

    def test_sort_with_null(self):
        interp = Interpreter()
        interp.run("""
        [2 8 1 NULL 4 7 NULL 3] SORT
        """)
        stack = interp.stack
        self.assertEqual(stack[0], [1, 2, 3, 4, 7, 8, None, None])

    def test_sort_w_forthic(self):
        interp = Interpreter()
        interp.run("""
        [2 8 1 4 7 3] "-1 *" !COMPARATOR SORT
        """)
        stack = interp.stack
        self.assertEqual(stack[0], [8, 7, 4, 3, 2, 1])

        # Sort record (no-op)
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] REC  "-1 *" !COMPARATOR SORT
        """)
        stack = interp.stack
        self.assertEqual(len(stack[0]), 3)
        self.assertEqual(list(stack[0].keys()), ['c', 'b', 'a'])

    def test_sort_w_key_func(self):
        interp = Interpreter()
        interp.stack_push(self.make_records())
        interp.run("""
        'status' FIELD-KEY-FUNC !COMPARATOR SORT
        """)
        stack = interp.stack
        self.assertEqual(stack[0][0]["status"], "CLOSED")
        self.assertEqual(stack[0][1]["status"], "CLOSED")
        self.assertEqual(stack[0][2]["status"], "IN PROGRESS")
        self.assertEqual(stack[0][3]["status"], "IN PROGRESS")
        self.assertEqual(stack[0][4]["status"], "OPEN")
        self.assertEqual(stack[0][5]["status"], "OPEN")
        self.assertEqual(stack[0][6]["status"], "OPEN")

        # Sort record (no-op)
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] NULL !COMPARATOR SORT
        """)
        stack = interp.stack
        self.assertEqual(len(stack[0]), 3)

    def test_nth(self):
        interp = Interpreter()
        interp.run("""
        ["x"] VARIABLES
        [0 1 2 3 4 5 6] x !
        x @ 0 NTH
        x @ 5 NTH
        x @ 55 NTH
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 0)
        self.assertEqual(stack[1], 5)
        self.assertIsNone(stack[2])

        # For record
        interp = Interpreter()
        interp.run("""
        ["x"] VARIABLES
        [['a' 1] ['b' 2] ['c' 3]] REC  x !
        x @ 0 NTH
        x @ 2 NTH
        x @ 55 NTH
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 1)
        self.assertEqual(stack[1], 3)
        self.assertIsNone(stack[2])

    def test_last(self):
        interp = Interpreter()
        interp.run("""
        [0 1 2 3 4 5 6] LAST
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 6)

        # For record
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] REC  LAST
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 3)

    def test_unpack(self):
        interp = Interpreter()
        interp.run("""
        [0 1 2] UNPACK
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 0)
        self.assertEqual(stack[1], 1)
        self.assertEqual(stack[2], 2)

        # For record
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] REC UNPACK
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 1)
        self.assertEqual(stack[1], 2)
        self.assertEqual(stack[2], 3)

    def test_FLATTEN(self):
        interp = Interpreter()
        interp.run("""
        [0 [1 2 [3 [4]] ]] FLATTEN
        """)
        stack = interp.stack
        self.assertEqual(stack[0], [0, 1, 2, 3, 4])

        # For record
        interp = Interpreter()
        interp.run("""
        ['uno' 'alpha'] VARIABLES
        [['uno' 4] ['duo' 8]] REC uno !
        [['alpha' uno @]] REC alpha !
        [['a' 1] ['b' alpha @] ['c' 3]] REC FLATTEN
        """)
        stack = interp.stack
        record = stack[0]
        self.assertEqual(sorted(list(record.keys())), ['a', 'b\talpha\tduo', 'b\talpha\tuno', 'c'])

    def test_FLATTEN_depth(self):
        interp = Interpreter()
        interp.run("""
            [ [ [0 1] [2 3] ]
              [ [4 5]       ] ] 1 !DEPTH FLATTEN
        """)
        array = interp.stack[-1]
        self.assertEqual(array, [[0, 1], [2, 3], [4, 5]])

        interp.run("""
            [ [ [0 1] [2 3] ]
              [ [4 5]       ] ] 0 !DEPTH FLATTEN
        """)
        array = interp.stack[-1]
        self.assertEqual(array, [[[0, 1] , [2, 3]], [[4, 5]]])

        interp.run("""
            [ [ [0 1] [2 3] ]
              [ [4 5]       ] ] 2 !DEPTH FLATTEN
        """)
        array = interp.stack[-1]
        self.assertEqual(array, [0, 1, 2, 3, 4, 5])
        return

    def test_FLATTEN_one_level_record(self):
        interp = Interpreter()
        interp.run("""
            ['uno' 'alpha'] VARIABLES
            [['uno' 4] ['duo' 8]] REC uno !
            [['alpha' uno @]] REC alpha !
            [['a' 1] ['b' alpha @] ['c' 3]] REC 1 !DEPTH FLATTEN
        """)
        record = interp.stack[0]
        self.assertEqual(sorted(record.keys()), ['a', 'b\talpha', 'c'])
        return


    def test_key_of(self):
        interp = Interpreter()
        interp.run("""
        ['x'] VARIABLES
        ['a' 'b' 'c' 'd'] x !
        x @  'c' KEY-OF
        x @  'z' KEY-OF
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 2)
        self.assertIsNone(stack[1])

        # For record
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] REC  2 KEY-OF
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 'b')

    def test_reduce(self):
        interp = Interpreter()
        interp.run("""
        [1 2 3 4 5] 10 "+" REDUCE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 25)

        # For record
        interp = Interpreter()
        interp.run("""
        [['a' 1] ['b' 2] ['c' 3]] REC  20 "+" REDUCE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 26)

    def test_cumulative_dist(self):
        def get_sample_records():
            res = []
            for i in range(20):
                res.append({"x": i})

            # Add records with no "x" field
            res.append({})
            res.append({})
            return res

        # Inputs
        sample_records = get_sample_records()
        field = "x"
        breakpoints = [5, 10, 20]

        # ---------------------------------------
        # Normal case
        interp = Interpreter()
        interp.stack_push(sample_records)
        interp.stack_push(field)
        interp.stack_push(breakpoints)
        interp.run("CUMULATIVE-DIST")
        result = interp.stack_pop()

        # Should get inputs back
        self.assertEqual(result.get("records"), sample_records)
        self.assertEqual(result.get("field"), field)
        self.assertEqual(result.get("breakpoints"), breakpoints)

        # Record breakpoint indexes should be correct
        record_breakpoint_indexes = result.get("record_breakpoint_indexes")
        self.assertEqual(0, record_breakpoint_indexes[0])
        self.assertEqual(0, record_breakpoint_indexes[5])
        self.assertEqual(1, record_breakpoint_indexes[6])
        self.assertEqual(1, record_breakpoint_indexes[10])
        self.assertEqual(2, record_breakpoint_indexes[11])
        self.assertEqual(2, record_breakpoint_indexes[19])
        self.assertEqual(1003, record_breakpoint_indexes[20])  # Have x being NULL
        self.assertEqual(1003, record_breakpoint_indexes[21])  # Have x being NULL

        # Breakpoint counts should be correct
        breakpoint_counts = result.get("breakpoint_counts")
        self.assertEqual(6, breakpoint_counts[0])
        self.assertEqual(11, breakpoint_counts[1])
        self.assertEqual(20, breakpoint_counts[2])

        # ---------------------------------------
        # Empty records
        interp = Interpreter()
        interp.stack_push([])
        interp.stack_push(field)
        interp.stack_push(breakpoints)
        interp.run("CUMULATIVE-DIST")
        result = interp.stack_pop()
        self.assertEqual([], result.get("record_breakpoint_indexes"))
        self.assertEqual([0, 0, 0], result.get("breakpoint_counts"))

        # ---------------------------------------
        # Incorrect field
        interp = Interpreter()
        interp.stack_push(sample_records)
        interp.stack_push("bad_field")
        interp.stack_push(breakpoints)
        interp.run("CUMULATIVE-DIST")
        result = interp.stack_pop()
        self.assertEqual([1003] * 22, result.get("record_breakpoint_indexes"))
        self.assertEqual([0, 0, 0], result.get("breakpoint_counts"))

        return

    def test_pop(self):
        interp = Interpreter()
        interp.run("""
        1 2 3 4 5 POP
        """)
        stack = interp.stack
        self.assertEqual(len(stack), 4)
        self.assertEqual(stack[-1], 4)

    def test_dup(self):
        interp = Interpreter()
        interp.run("""
        5 DUP
        """)
        stack = interp.stack
        self.assertEqual(len(stack), 2)
        self.assertEqual(stack[0], 5)
        self.assertEqual(stack[1], 5)

    def test_swap(self):
        interp = Interpreter()
        interp.run("""
        6 8 SWAP
        """)
        stack = interp.stack
        self.assertEqual(len(stack), 2)
        self.assertEqual(stack[0], 8)
        self.assertEqual(stack[1], 6)

    def test_split(self):
        interp = Interpreter()
        interp.run("""
        'Now is the time' ' ' SPLIT
        """)
        stack = interp.stack
        self.assertEqual(len(stack), 1)
        self.assertEqual(stack[0], ["Now", "is", "the", "time"])

    def test_join(self):
        interp = Interpreter()
        interp.run("""
        ["Now" "is" "the" "time"] "--" JOIN
        """)
        stack = interp.stack
        self.assertEqual(len(stack), 1)
        self.assertEqual(stack[0], "Now--is--the--time")

    def test_special_chars(self):
        interp = Interpreter()
        interp.run("""
        /R /N /T
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "\r")
        self.assertEqual(stack[1], "\n")
        self.assertEqual(stack[2], "\t")

    def test_LOWERCASE(self):
        interp = Interpreter()
        interp.run("""
        "HOWDY, Everyone!" LOWERCASE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "howdy, everyone!")

    def test_ascii(self):
        interp = Interpreter()
        interp.run("""
        "“HOWDY, Everyone!”" ASCII
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "HOWDY, Everyone!")

    def test_strip(self):
        interp = Interpreter()
        interp.run("""
        "  howdy  " STRIP
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "howdy")

    def test_replace(self):
        interp = Interpreter()
        interp.run("""
        "1-40 2-20" "-" "." REPLACE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "1.40 2.20")

    def test_re_replace(self):
        interp = Interpreter()
        interp.run(r"""
        "Howdy https://www.linkedin.com" "(https?://\S+)" "=HYPERLINK('\1', '\1')" RE-REPLACE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "Howdy =HYPERLINK('https://www.linkedin.com', 'https://www.linkedin.com')")

    def test_match(self):
        interp = Interpreter()
        interp.run(r"""
        "123message456" "\d{3}.*\d{3}" RE-MATCH
        """)
        stack = interp.stack
        self.assertTrue(stack[0])

    def test_match_group(self):
        interp = Interpreter()
        interp.run(r"""
        "123message456" "\d{3}(.*)\d{3}" RE-MATCH 1 RE-MATCH-GROUP
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "message")

    def test_match_all(self):
        interp = Interpreter()
        interp.run("""
        "mr-android ios my-android web test-web" ".*?(android|ios|web|seo)" RE-MATCH-ALL
        """)
        stack = interp.stack
        self.assertEqual(stack[0], ['android', 'ios', 'android', 'web', 'web'])

    def test_URL_ENCODE(self):
        interp = Interpreter()
        interp.run("""
        "now/is the time" URL-ENCODE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "now%2Fis+the+time")

    def test_URL_DECODE(self):
        interp = Interpreter()
        interp.run("""
        "now%2Fis%20the%20time" URL-DECODE
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "now/is the time")

    def test_default(self):
        interp = Interpreter()
        interp.run("""
        NULL 22.4 DEFAULT
        0 22.4 DEFAULT
        "" "Howdy" DEFAULT
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 22.4)
        self.assertEqual(stack[1], 0)
        self.assertEqual(stack[2], "Howdy")

    def test_star_DEFAULT(self):
        interp = Interpreter()
        interp.run("""
        NULL "3.1 5 +" *DEFAULT
        0 "22.4" *DEFAULT
        "" "['Howdy, ' 'Everyone!'] CONCAT" *DEFAULT
        """)
        stack = interp.stack
        self.assertAlmostEqual(stack[0], 8.1)
        self.assertEqual(stack[1], 0)
        self.assertEqual(stack[2], "Howdy, Everyone!")

    def test_l_repeat(self):
        interp = Interpreter()
        interp.run("""
        [0 "1 +" 6 <REPEAT]
        """)
        stack = interp.stack
        self.assertEqual(stack[0], [0, 1, 2, 3, 4, 5, 6])

    def test_to_fixed(self):
        interp = Interpreter()
        interp.run("""
        22 7 / 2 >FIXED
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "3.14")

    def test_to_json(self):
        interp = Interpreter()
        interp.run("""
        [["a" 1] ["b" 2]] REC >JSON
        """)
        stack = interp.stack
        self.assertEqual(stack[0], '{"a": 1, "b": 2}')

    def test_json_to(self):
        interp = Interpreter()
        interp.run("""
        '{"a": 1, "b": 2}' JSON>
        """)
        stack = interp.stack
        self.assertEqual(sorted(stack[0].keys()), ['a', 'b'])
        self.assertEqual(stack[0]['a'], 1)
        self.assertEqual(stack[0]['b'], 2)

    def test_to_tsv(self):
        interp = Interpreter()
        interp.run("""
        [['alpha' 'beta' 'gamma'] [1 2 3]] >TSV
        [['a\t1' 'b\t2' 'c\n3'] [4 5 6]] >TSV
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "alpha\tbeta\tgamma\r\n1\t2\t3\r\n")
        self.assertEqual(stack[1], '"a\t1"\t"b\t2"\t"c\n3"\r\n4\t5\t6\r\n')

    def test_tsv_to(self):
        interp = Interpreter()
        interp.run("""
        "alpha\tbeta\tgamma\r\n1\t2\t3\r\n" TSV>
        """)
        stack = interp.stack
        self.assertEqual(stack[0], [['alpha', 'beta', 'gamma'], ['1', '2', '3']])

    def test_recs_to_tsv(self):
        interp = Interpreter()
        interp.run("""
        [
            ['alpha' 'beta' 'gamma'] [1 2 3] ZIP REC
            ['alpha' 'beta' 'gamma'] [2 4 6] ZIP REC
        ] ['alpha' 'gamma'] RECS>TSV
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "alpha\tgamma\r\n1\t3\r\n2\t6\r\n")

    def test_tsv_to_recs(self):
        interp = Interpreter()
        interp.run("""
        "alpha\tgamma\r\n1\t3\r\n2\t6\r\n" TSV>RECS
        """)
        stack = interp.stack
        self.assertEqual(sorted(stack[0][0].keys()), ['alpha', 'gamma'])
        self.assertEqual(sorted(stack[0][1].keys()), ['alpha', 'gamma'])
        self.assertEqual(stack[0][0]['alpha'], '1')
        self.assertEqual(stack[0][0]['gamma'], '3')
        self.assertEqual(stack[0][1]['alpha'], '2')
        self.assertEqual(stack[0][1]['gamma'], '6')

    def test_now(self):
        now = datetime.datetime.now()
        interp = Interpreter()
        interp.run("NOW")
        stack = interp.stack
        self.assertEqual(stack[0].hour, now.hour)
        self.assertEqual(stack[0].minute, now.minute)

    def test_to_time(self):
        interp = Interpreter()
        interp.run("'10:52 PM' >TIME")
        stack = interp.stack
        self.assertEqual(stack[0].hour, 22)
        self.assertEqual(stack[0].minute, 52)

    def test_l_tz_bang(self):
        interp = Interpreter()
        interp.run("'10:52 PM' >TIME 'US/Eastern' <TZ!")
        stack = interp.stack
        self.assertEqual(stack[0].hour, 22)
        self.assertEqual(stack[0].minute, 52)

    def test_time_to_str(self):
        interp = Interpreter(timezone=pytz.timezone("US/Pacific"))
        interp.run("""
        '10:52 AM' >TIME 'US/Eastern' <TZ! TIME>STR
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "07:52")

    def test_to_date(self):
        interp = Interpreter()
        interp.run("""
        "Oct 21, 2020" >DATE
        """)
        stack = interp.stack
        self.assertEqual(stack[0].year, 2020)
        self.assertEqual(stack[0].month, 10)
        self.assertEqual(stack[0].day, 21)

    def test_today(self):
        interp = Interpreter()
        interp.run("""
        TODAY
        """)
        today = datetime.date.today()
        stack = interp.stack
        self.assertEqual(stack[0].year, today.year)
        self.assertEqual(stack[0].month, today.month)
        self.assertEqual(stack[0].day, today.day)

    def test_days_of_week(self):
        today = datetime.date.today()
        interp = Interpreter()
        interp.run("""
        MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY
        """)
        stack = interp.stack
        self.assertTrue(stack[0] <= today)
        self.assertTrue(stack[6] >= today)

    def test_add_days(self):
        interp = Interpreter()
        interp.run("""
        2020-10-21 12 ADD-DAYS
        """)
        stack = interp.stack
        self.assertEqual(stack[0].year, 2020)
        self.assertEqual(stack[0].month, 11)
        self.assertEqual(stack[0].day, 2)

    def test_subtract_dates(self):
        interp = Interpreter()
        interp.run("""
        2020-10-21 2020-11-02 SUBTRACT-DATES
        """)
        stack = interp.stack
        self.assertEqual(stack[0], -12)

    def test_date_to_str(self):
        interp = Interpreter()
        interp.run("""
        2020-11-02 DATE>STR
        """)
        stack = interp.stack
        self.assertEqual(stack[0], "2020-11-02")

    def test_date_time_to_datetime(self):
        interp = Interpreter()
        interp.run("""
        2020-11-02 10:25 PM DATE-TIME>DATETIME
        2020-11-02 10:25 PM DATE-TIME>DATETIME >DATE
        2020-11-02 10:25 PM DATE-TIME>DATETIME >TIME
        """)
        stack = interp.stack
        self.assertEqual(stack[0].year, 2020)
        self.assertEqual(stack[0].month, 11)
        self.assertEqual(stack[0].day, 2)
        self.assertEqual(stack[0].hour, 22)
        self.assertEqual(stack[0].minute, 25)
        self.assertEqual(stack[1].year, 2020)
        self.assertEqual(stack[1].month, 11)
        self.assertEqual(stack[1].day, 2)
        self.assertEqual(stack[2].hour, 22)
        self.assertEqual(stack[2].minute, 25)

    def test_datetime_to_timestamp(self):
        interp = Interpreter()
        interp.run("""
        2020-07-01 15:20 DATE-TIME>DATETIME DATETIME>TIMESTAMP
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 1593642000)

    def test_timestamp_to_datetime(self):
        interp = Interpreter()
        interp.run("""
        1593895532 TIMESTAMP>DATETIME
        """)
        stack = interp.stack
        self.assertEqual(stack[0].year, 2020)
        self.assertEqual(stack[0].month, 7)
        self.assertEqual(stack[0].day, 4)
        self.assertEqual(stack[0].hour, 13)
        self.assertEqual(stack[0].minute, 45)

    def test_arithmetic(self):
        interp = Interpreter()
        interp.run("""
        2 4 +
        2 4 -
        2 4 *
        2 4 /
        5 3 MOD
        2.51 ROUND
        [1 2 3] +
        [2 3 4] *
        """)
        stack = interp.stack
        self.assertEqual(stack[0], 6)
        self.assertEqual(stack[1], -2)
        self.assertEqual(stack[2], 8)
        self.assertEqual(stack[3], 0.5)
        self.assertEqual(stack[4], 2)
        self.assertEqual(stack[5], 3)
        self.assertEqual(stack[6], 6)
        self.assertEqual(stack[7], 24)

    def test_comparison(self):
        interp = Interpreter()
        interp.run("""
        2 4 ==
        2 4 !=
        2 4 <
        2 4 <=
        2 4 >
        2 4 >=
        """)
        stack = interp.stack
        self.assertFalse(stack[0])
        self.assertTrue(stack[1])
        self.assertTrue(stack[2])
        self.assertTrue(stack[3])
        self.assertFalse(stack[4])
        self.assertFalse(stack[5])

    def test_logic(self):
        interp = Interpreter()
        interp.run("""
        FALSE FALSE OR
        [FALSE FALSE TRUE FALSE] OR
        FALSE TRUE AND
        [FALSE FALSE TRUE FALSE] AND
        FALSE NOT
        """)
        stack = interp.stack
        self.assertFalse(stack[0])
        self.assertTrue(stack[1])
        self.assertFalse(stack[2])
        self.assertFalse(stack[3])
        self.assertTrue(stack[4])

    def test_in(self):
        interp = Interpreter()
        interp.run("""
        "alpha" ["beta" "gamma"] IN
        "alpha" ["beta" "gamma" "alpha"] IN
        """)
        stack = interp.stack
        self.assertFalse(stack[0])
        self.assertTrue(stack[1])

    def test_any(self):
        interp = Interpreter()
        interp.run("""
        ["alpha" "beta"] ["beta" "gamma"] ANY
        ["delta" "beta"] ["gamma" "alpha"] ANY
        ["alpha" "beta"] [] ANY
        """)
        stack = interp.stack
        self.assertTrue(stack[0])
        self.assertFalse(stack[1])
        self.assertTrue(stack[2])

    def test_all(self):
        interp = Interpreter()
        interp.run("""
        ["alpha" "beta"] ["beta" "gamma"] ALL
        ["delta" "beta"] ["beta"] ALL
        ["alpha" "beta"] [] ALL
        """)
        stack = interp.stack
        self.assertFalse(stack[0])
        self.assertTrue(stack[1])
        self.assertTrue(stack[2])

    def test_quoted(self):
        interp = Interpreter()
        interp.run(f"""
        "howdy" QUOTED
        "sinister{DLE}INJECT-BADNESS" QUOTED
        """)
        stack = interp.stack
        self.assertEqual(f"{DLE}howdy{DLE}", stack[0])
        self.assertEqual(f"{DLE}sinister INJECT-BADNESS{DLE}", stack[1])

    def test_math_converters(self):
        interp = Interpreter()
        interp.run("""
        NULL >BOOL
        0 >BOOL
        1 >BOOL
        "" >BOOL
        "Hi" >BOOL
        "3" >INT
        4 >INT
        4.6 >INT
        "1.2" >FLOAT
        2 >FLOAT
        """)
        stack = interp.stack
        self.assertFalse(stack[0])
        self.assertFalse(stack[1])
        self.assertTrue(stack[2])
        self.assertFalse(stack[3])
        self.assertTrue(stack[4])
        self.assertEqual(stack[5], 3)
        self.assertEqual(stack[6], 4)
        self.assertEqual(stack[7], 4)
        self.assertEqual(stack[8], 1.2)
        self.assertEqual(stack[9], 2.0)

    def test_profiling(self):
        interp = Interpreter()
        interp.run("""
        PROFILE-START
        [0 "1 +" 6 <REPEAT]
        PROFILE-END POP
        PROFILE-DATA
        """)
        stack = interp.stack
        profile_data = stack[-1]
        self.assertEqual(profile_data["word_counts"][0]["word"], "+")
        self.assertEqual(profile_data["word_counts"][0]["count"], 6)


if __name__ == '__main__':
    unittest.main()
