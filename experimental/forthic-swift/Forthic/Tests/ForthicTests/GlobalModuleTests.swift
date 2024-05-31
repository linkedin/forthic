import XCTest
import OrderedCollections
@testable import Forthic

final class GlobalModuleTests: XCTestCase {
    func test_literals() throws {
        let interp = Interpreter()
        try interp.run(forthic: "TRUE  2  3.14 2020-06-05 9:00 22:15 AM  11:30 PM")

        let date_formatter = DateFormatter()
        date_formatter.timeZone = interp.timezone
        date_formatter.dateFormat = "yyyy-MM-dd"

        let time_formatter = DateFormatter()
        time_formatter.dateFormat = "HH:mm"
        time_formatter.timeZone = interp.timezone

        XCTAssertEqual(interp.stack[0] as! Bool, true)
        XCTAssertEqual(interp.stack[1] as! Int, 2)
        XCTAssertEqual(interp.stack[2] as! Float, 3.14)
        XCTAssertEqual(interp.stack[3] as! Date, date_formatter.date(from: "2020-06-05")!)

        XCTAssertEqual(interp.stack[4] as! Date, time_formatter.date(from: "09:00")!)
        XCTAssertEqual(interp.stack[5] as! Date, time_formatter.date(from: "10:15")!)
        XCTAssertEqual(interp.stack[6] as! Date, time_formatter.date(from: "23:30")!)
    }

    func test_variables() throws {
        let interp = Interpreter()
        try interp.run(forthic: "['x' 'y']  VARIABLES")
        let variables = interp.app_module().variables
        XCTAssertNotNil(variables["x"])
        XCTAssertNotNil(variables["y"])
    }

    func test_set_get_variables() throws {
        let interp = Interpreter()
        try interp.run(forthic: "['x'] VARIABLES")

        // Set variable and verify
        try interp.run(forthic: "24 x !")
        let x_var: Variable = interp.app_module().variables["x"]!
        XCTAssertEqual(x_var.get_value() as! Int, 24)

        // Get variable value
        try interp.run(forthic: "x @")
        XCTAssertEqual(interp.stack.last as! Int, 24)
    }

    func test_bang_at() throws {
        let interp = Interpreter()
        try interp.run(forthic: "['x'] VARIABLES")
        try interp.run(forthic: "24 x !@")
        let x_var: Variable = interp.app_module().variables["x"]!
        XCTAssertEqual(x_var.get_value() as! Int, 24)
        XCTAssertEqual(interp.stack.last as! Int, 24)
    }

    func test_interpret() throws {
        let interp = Interpreter()

        // Interpret a simple string
        try interp.run(forthic: "'24' INTERPRET")
        XCTAssertEqual(interp.stack.last as! Int, 24)

        // Interpret something more interesting
        try interp.run(forthic:
         """
         '{module-A  : MESSAGE   "Hi" ;}' INTERPRET
         """)
        try interp.run(forthic: "{module-A MESSAGE}")
        XCTAssertEqual(interp.stack.last as! String, "Hi")
    }

    func test_plus() throws {
        let interp = Interpreter()
        try interp.run(forthic: "1 2 +")
        XCTAssertEqual(Int(interp.stack.last as! Double), 3)
    }

    func test_memo() throws {
        let interp = Interpreter()

        try interp.run(forthic:
         """
         ['count'] VARIABLES
         0 count !
         "COUNT"   "count @ 1 +  count !  count @"   MEMO
         """)

        try interp.run(forthic: "COUNT")
        XCTAssertEqual(Int(interp.stack.last as! Double), 1)
        XCTAssertEqual(interp.stack.count, 1)

        try interp.run(forthic: "COUNT")
        XCTAssertEqual(Int(interp.stack.last as! Double), 1)
        XCTAssertEqual(interp.stack.count, 2)

        try interp.run(forthic: "COUNT! COUNT")
        XCTAssertEqual(Int(interp.stack.last as! Double), 2)
        XCTAssertEqual(interp.stack.count, 3)

        try interp.run(forthic: "COUNT!@")
        XCTAssertEqual(Int(interp.stack.last as! Double), 3)
        XCTAssertEqual(interp.stack.count, 4)
    }

    func test_rec() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
         """)
        XCTAssertEqual(interp.stack.count, 1)

        let rec = interp.stack.last as! Record
        XCTAssertEqual(rec.keys, ["alpha", "beta", "gamma"])
        XCTAssertEqual(rec["alpha"] as! Int, 2)
        XCTAssertEqual(rec["beta"] as! Int, 3)
        XCTAssertEqual(rec["gamma"] as! Int, 4)
    }

    func test_rec_at() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC  'beta' REC@
         """)
        XCTAssertEqual(interp.stack.count, 1)
        XCTAssertEqual(interp.stack.last as! Int, 3)

        interp = Interpreter()
        try interp.run(forthic:
         """
         [ ["alpha" [["alpha1" 20]] REC] ] REC  ['alpha' 'alpha1'] REC@
         """)
        XCTAssertEqual(interp.stack.count, 1)
        XCTAssertEqual(interp.stack.last as! Int, 20)

        try interp.run(forthic:
         """
         [ ["alpha" [["alpha1" 20]] REC] ] REC  ['alpha' 'garbage'] REC@
         """)
        XCTAssertNil(interp.stack.last!)
    }

    func test_l_rec_bang() throws {
        // Case: Set value
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
         700 'beta' <REC! 'beta' REC@
         """)
        XCTAssertEqual(interp.stack.count, 1)
        XCTAssertEqual(interp.stack.last as! Int, 700)

        // Case: Set a nested value
        interp = Interpreter()
        try interp.run(forthic:
         """
         : REC1   [["TEST-1234"   "Yellow"]] REC;
         : REC2   [["2021-03-22"  REC1]] REC;
         REC2 "Green" ["2021-03-22" "TEST-1234"] <REC!  ["2021-03-22" "TEST-1234"] REC@
         """)
        XCTAssertEqual(interp.stack.count, 1)
        XCTAssertEqual(interp.stack.last as! String, "Green")

        // Case: Set a nested value on an empty record
        interp = Interpreter()
        try interp.run(forthic:
         """
         [] REC "Red" ["2021-03-22" "TEST-1234"] <REC! ["2021-03-22" "TEST-1234"] REC@
         """)
        XCTAssertEqual(interp.stack.count, 1)
        XCTAssertEqual(interp.stack.last as! String, "Red")

        // Case: Set value on a NULL
        interp = Interpreter()
        try interp.run(forthic:
         """
         NULL 715 'beta' <REC! 'beta' REC@
         """)
        XCTAssertEqual(interp.stack.count, 1)
        XCTAssertEqual(interp.stack.last as! Int, 715)
    }

    func test_SCREEN_bang() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         'Screen content' 'my-screen' SCREEN!
         """)
        XCTAssertEqual(try interp.app_module().get_screen(name: "my-screen"), "Screen content")
    }

    func test_SCREEN() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         'Screen content' 'my-screen' SCREEN!
         'my-screen' SCREEN
         """)
        XCTAssertEqual(interp.stack.last as! String, "Screen content")
    }

    func test_LOAD_SCREEN() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         ': MESSAGE   "Howdy!";'   'message' SCREEN!
         'message' LOAD-SCREEN
         MESSAGE
         """)
        XCTAssertEqual(interp.stack.last as! String, "Howdy!")

        func load_recursive_screen() throws {
            try interp.run(forthic:
            """
            ': MESSAGE   "Howdy!";  "message" LOAD-SCREEN' 'message' SCREEN!
            'message' LOAD-SCREEN
            """)
        }

        XCTAssertThrowsError(try load_recursive_screen())
    }

    func test_APPEND() throws {
        // Append to an array
        let interp = Interpreter()
        try interp.run(forthic:
         """
         [ 1 2 3 ] 4 APPEND
         """)
        XCTAssertEqual(interp.stack.last as! [Int], [1, 2, 3, 4])

        // Append to a record
        try interp.run(forthic:
         """
         [["a" 1] ["b" 2]] REC  ["c" 3] APPEND
         """)
        let rec = interp.stack.last as! Record
        XCTAssertEqual(rec.keys, ["a", "b", "c"])
        XCTAssertEqual(rec["c"] as! Int, 3)
    }

    func test_REVERSE() throws {
        let interp = Interpreter()

        // Reverse array
        try interp.run(forthic:
         """
         [1 2 3] REVERSE
         """)
        XCTAssertEqual(interp.stack.last as! [Int], [3, 2, 1])

        // Reverse record
        try interp.run(forthic:
         """
         [["a" 1] ["b" 2]] REC  REVERSE
         """)
        let record = interp.stack.last as! Record
        XCTAssertEqual(record.keys, ["b", "a"])
    }

    func test_UNIQUE() throws {
        let interp = Interpreter()

        // Get unique array values
        try interp.run(forthic:
         """
         [ 1 2 3 3 2 ] UNIQUE
         """)
        var numbers = interp.stack.last as! [Int]
        numbers.sort()
        XCTAssertEqual(numbers, [1, 2, 3])

        // Get unique record values
        try interp.run(forthic:
         """
         [["a" 1] ["b" 2] ["c" 2] ["d" 1]] REC  UNIQUE
         """)
        let rec = interp.stack.last as! Record
        let values = Array(rec.values) as! [Int]
        XCTAssertEqual(values.sorted(), [1, 2])
    }

    func test_l_DEL() throws {
        let interp = Interpreter()

        // Delete at index 1
        try interp.run(forthic:
         """
         ["a" "b" "c"] 1 <DEL
         """)
        XCTAssertEqual(interp.stack.last as! [String], ["a", "c"])

        // Delete record value
        try interp.run(forthic:
         """
         [["a" 1] ["b" 2] ["c" 3]] REC  "b" <DEL
         """)
        var record = interp.stack.last as! Record
        XCTAssertEqual(Set(record.keys), Set(["a", "c"]))

        // Delete missing record value
        try interp.run(forthic:
         """
         [["a" 1] ["b" 2] ["c" 3]] REC  "d" <DEL
         """)
        record = interp.stack.last as! Record
        XCTAssertEqual(Set(record.keys), Set(["a", "b", "c"]))
    }

    func test_RELABEL() throws {
        let interp = Interpreter()

        // Select 1st and 3rd elements and swap their order
        try interp.run(forthic:
         """
         [ "a" "b" "c" ] [0 2] [25 23] RELABEL
         """)
        XCTAssertEqual(interp.stack.last as! [String], ["c", "a"])

        try interp.run(forthic:
         """
         [["a" 1] ["b" 2] ["c" 3]] REC  ["a" "c"] ["alpha" "gamma"] RELABEL
         """)
        let rec = interp.stack.last as! Record
        XCTAssertEqual(Set(rec.keys), Set(["alpha", "gamma"]))
        XCTAssertEqual(rec["alpha"] as! Int, 1)
        XCTAssertEqual(rec["gamma"] as! Int, 3)
    }

    func test_BY_FIELD() throws {
        let interp = Interpreter()
        interp.stack_push(make_sample_records())
        try interp.run(forthic: "'key' BY-FIELD")
        let grouped = interp.stack.last as! Record
        let rec = grouped[104] as! Record
        XCTAssertEqual(rec["status"] as! String, "IN PROGRESS")

        // Test with Record
        interp.stack_push(grouped)
        try interp.run(forthic: "'key' BY-FIELD")
        let rec2 = grouped[101] as! Record
        XCTAssertEqual(rec2["status"] as! String, "OPEN")
    }

    func test_GROUP_BY_FIELD() throws {
        let interp = Interpreter()
        interp.stack_push(make_sample_records())
        try interp.run(forthic: "'assignee' GROUP-BY-FIELD")
        var grouped = interp.stack.last as! Record
        XCTAssertEqual(Set(grouped.keys), Set(["user1", "user2"]))
        XCTAssertEqual((grouped["user1"] as! List).count, 4)
        XCTAssertEqual((grouped["user2"] as! List).count, 3)

        // Test grouping a record
        interp.stack_push(make_sample_records())
        try interp.run(forthic: "'key' BY-FIELD")
        try interp.run(forthic: "'assignee' GROUP-BY-FIELD")
        grouped = interp.stack.last as! Record
        XCTAssertEqual(Set(grouped.keys), Set(["user1", "user2"]))
        XCTAssertEqual((grouped["user1"] as! List).count, 4)
        XCTAssertEqual((grouped["user2"] as! List).count, 3)
    }

    func test_GROUP_BY() throws {
        let interp = Interpreter()
        interp.stack_push(make_sample_records())
        try interp.run(forthic:
         """
         "'assignee' REC@" GROUP-BY
         """)
        var grouped = interp.stack.last as! Record
        XCTAssertEqual(Set(grouped.keys), Set(["user1", "user2"]))
        XCTAssertEqual((grouped["user1"] as! List).count, 4)
        XCTAssertEqual((grouped["user2"] as! List).count, 3)

        // Test grouping a record
        interp.stack_push(make_sample_records())
        try interp.run(forthic: "'key' BY-FIELD")
        try interp.run(forthic:
         """
         "'assignee' REC@" GROUP-BY
         """)
        grouped = interp.stack.last as! Record
        XCTAssertEqual(Set(grouped.keys), Set(["user1", "user2"]))
        XCTAssertEqual((grouped["user1"] as! List).count, 4)
        XCTAssertEqual((grouped["user2"] as! List).count, 3)
    }

    func test_GROUP_BY_w_KEY() throws {
        let interp = Interpreter()
        interp.stack_push(make_sample_records())
        try interp.run(forthic:
         """
         ['key' 'val'] VARIABLES
         "val ! key ! key @ 3 MOD" GROUP-BY-w/KEY
         """)
        var grouped = interp.stack.last as! Record
        XCTAssertEqual(Set(grouped.keys), Set([0, 1, 2]))
        XCTAssertEqual((grouped[0] as! List).count, 3)
        XCTAssertEqual((grouped[1] as! List).count, 2)
        XCTAssertEqual((grouped[2] as! List).count, 2)

        interp.stack_push(make_sample_records())
        try interp.run(forthic: "'key' BY-FIELD")
        try interp.run(forthic:
         """
         ['key' 'val'] VARIABLES
         "val ! key ! key @ 2 * >INT" GROUP-BY-w/KEY
         """)
        grouped = interp.stack.last as! Record
        XCTAssertEqual(Set(grouped.keys), Set([200, 202, 204, 206, 208, 210, 212]))
    }

    func test_GROUPS_OF() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         [1 2 3 4 5 6 7 8] 3 GROUPS-OF
         """)
        var groups = interp.stack.last as! List
        XCTAssertEqual(groups.count, 3)
        XCTAssertEqual(groups[0] as! [Int], [1, 2, 3])
        XCTAssertEqual(groups[1] as! [Int], [4, 5, 6])
        XCTAssertEqual(groups[2] as! [Int], [7, 8])

        // Test grouping a record
        interp.stack_push(make_sample_records())
        try interp.run(forthic:
        """
        'key' BY-FIELD
        3 GROUPS-OF
        """)
        groups = interp.stack.last as! List
        XCTAssertEqual(groups.count, 3)
        XCTAssertEqual((groups[0] as! Record).count, 3)
        XCTAssertEqual((groups[1] as! Record).count, 3)
        XCTAssertEqual((groups[2] as! Record).count, 1)
    }

    func test_MAP() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         [1 2 3 4 5] '2 * >INT' MAP
         """)
        let items = interp.stack.last as! [Int]
        XCTAssertEqual(items, [2, 4, 6, 8, 10])

        // Test mapping over a record
        interp.stack_push(make_sample_records())
        try interp.run(forthic:
        """
        'key' BY-FIELD
        "'status' REC@" MAP
        """)
        let record = interp.stack.last as! Record
        XCTAssertEqual(record[100] as! String, "OPEN")
        XCTAssertEqual(record[102] as! String, "IN PROGRESS")
        XCTAssertEqual(record[106] as! String, "CLOSED")
    }

    func test_MAP_w_KEY() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         [1 2 3 4 5] '+ 2 * >INT' MAP-w/KEY
         """)
        let items = interp.stack.last as! [Int]
        XCTAssertEqual(items, [2, 6, 10, 14, 18])

        // Test mapping over a record
        interp.stack_push(make_sample_records())
        try interp.run(forthic:
        """
        'key' BY-FIELD
        ["k" "v"] VARIABLES
        "(v ! k !)  k @ >STR v @ 'status' REC@ CONCAT" MAP-w/KEY
        """)
        let record = interp.stack.last as! Record
        XCTAssertEqual(record[100] as! String, "100OPEN")
        XCTAssertEqual(record[102] as! String, "102IN PROGRESS")
        XCTAssertEqual(record[106] as! String, "106CLOSED")
    }

    func test_FOREACH() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         0 [1 2 3 4 5] '+' FOREACH >INT
         """)
        XCTAssertEqual(interp.stack.last as! Int, 15)

        // Run against a record
        interp.stack_push(make_sample_records())
        try interp.run(forthic:
        """
        'key' BY-FIELD
        "" SWAP "'status' REC@ CONCAT" FOREACH
        """)
        XCTAssertEqual(interp.stack.last as! String, "OPENOPENIN PROGRESSCLOSEDIN PROGRESSOPENCLOSED")
    }

    func test_FOREACH_w_KEY() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         0 [1 2 3 4 5] '+ +' FOREACH-w/KEY >INT
         """)
        XCTAssertEqual(interp.stack.last as! Int, 25)

        // Run against a record
        interp.stack_push(make_sample_records())
        try interp.run(forthic:
        """
        'key' BY-FIELD
        "" SWAP "'status' REC@ CONCAT CONCAT" FOREACH-w/KEY
        """)
        XCTAssertEqual(interp.stack.last as! String, "100OPEN101OPEN102IN PROGRESS103CLOSED104IN PROGRESS105OPEN106CLOSED")
    }

    func test_FOREACH_to_ERRORS() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         ['2' '3' 'GARBAGE' '+ >INT'] 'INTERPRET' FOREACH>ERRORS
         """)
        let errors = interp.stack.popLast() as! List
        XCTAssertNil(errors[0])
        XCTAssertNil(errors[1])
        XCTAssertNotNil(errors[2])
        XCTAssertNil(errors[3])

        XCTAssertEqual(interp.stack.last as! Int, 5)
    }

    func test_ZIP() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         ['a' 'b'] [1 2] ZIP
         """)
        let list = interp.stack.last as! List
        var pair1 = list[0] as! List
        XCTAssertEqual(pair1[0] as! String, "a")
        XCTAssertEqual(pair1[1] as! Int, 1)
        var pair2 = list[1] as! List
        XCTAssertEqual(pair2[0] as! String, "b")
        XCTAssertEqual(pair2[1] as! Int, 2)

        // Zip a record
        try interp.run(forthic:
         """
         [['a' 100] ['b' 200] ['z' 300]] REC [['a' 'Hi'] ['b' 'Bye'] ['c' '?']] REC ZIP
         """)
        let record = interp.stack.last as! Record
        XCTAssertEqual(Set(record.keys), Set(["a", "b", "z"]))
        pair1 = record["a"] as! List
        pair2 = record["z"] as! List
        XCTAssertEqual(pair1[0] as! Int, 100)
        XCTAssertEqual(pair1[1] as! String, "Hi")
        XCTAssertEqual(pair2[0] as! Int, 300)
        XCTAssertNil(pair2[1])
    }

    func test_ZIP_WITH() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         [10 20] [1 2] "+ >INT" ZIP-WITH
         """)
        let list = interp.stack.last as! List
        XCTAssertEqual(list[0] as! Int, 11)
        XCTAssertEqual(list[1] as! Int, 22)

        // Zip a record
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2]] REC [['a' 10] ['b' 20]] REC "+ >INT" ZIP-WITH
         """)
        let record = interp.stack.last as! Record
        XCTAssertEqual(Set(record.keys), Set(["a", "b"]))
        XCTAssertEqual(record["a"] as! Int, 11)
        XCTAssertEqual(record["b"] as! Int, 22)
    }

    func test_KEYS() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         ['a' 'b' 'c'] KEYS
         """)
        XCTAssertEqual(try to_array(items: interp.stack.last as! List), [0, 1, 2])

        try interp.run(forthic:
         """
         [['a' 1] ['b' 2]] REC KEYS
         """)
        XCTAssertEqual(try to_array(items: interp.stack.last as! List), ["a", "b"])
    }

    func test_VALUES() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         ['a' 'b' 'c'] VALUES
         """)
        XCTAssertEqual(try to_array(items: interp.stack.last as! List), ["a", "b", "c"])

        try interp.run(forthic:
         """
         [['a' 1] ['b' 2]] REC VALUES
         """)
        XCTAssertEqual(try to_array(items: interp.stack.last as! List), [1, 2])
    }

    func test_LENGTH() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         ['a' 'b' 'c'] LENGTH
         """)
        XCTAssertEqual(interp.stack.last as! Int, 3)

        try interp.run(forthic:
         """
         [['a' 1] ['b' 2]] REC LENGTH
         """)
        XCTAssertEqual(interp.stack.last as! Int, 2)
    }

    func test_SLICE() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         ['x'] VARIABLES
         ['a' 'b' 'c' 'd' 'e' 'f' 'g'] x !
         x @ 0 2 SLICE
         x @ 1 3 SLICE
         x @ 5 3 SLICE
         x @ -1 -2 SLICE
         x @ 4 -2 SLICE
         x @ 5 8 SLICE
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), ["a", "b", "c"])
        XCTAssertEqual(try to_array(items: interp.stack[1] as! List), ["b", "c", "d"])
        XCTAssertEqual(try to_array(items: interp.stack[2] as! List), ["f", "e", "d"])
        XCTAssertEqual(try to_array(items: interp.stack[3] as! List), ["g", "f"])
        XCTAssertEqual(try to_array(items: interp.stack[4] as! List), ["e", "f"])
        let list = interp.stack[5] as! List
        XCTAssertEqual(list.count, 4)
        XCTAssertEqual(list[0] as! String, "f")
        XCTAssertEqual(list[1] as! String, "g")
        XCTAssertNil(list[2])
        XCTAssertNil(list[3])

        // Slice records
        interp = Interpreter()
        try interp.run(forthic:
         """
         ['x'] VARIABLES
         [['a' 1] ['b' 2] ['c' 3]] REC x !
         x @ 0 1 SLICE
         x @ -1 -2 SLICE
         x @ 5 7 SLICE
         """)
        let record1 = interp.stack[0] as! Record
        let record2 = interp.stack[1] as! Record
        let record3 = interp.stack[2] as! Record
        XCTAssertEqual(try to_array(items: Array(record1.keys)), ["a", "b"])
        XCTAssertEqual(try to_array(items: Array(record2.keys)), ["c", "b"])
        XCTAssertEqual(record3.count, 0)
    }

    func test_DIFFERENCE() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         ['x' 'y'] VARIABLES
         ['a' 'b' 'c'] x !
         ['a' 'c' 'd'] y !
         x @ y @ DIFFERENCE
         y @ x @ DIFFERENCE
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), ["b"])
        XCTAssertEqual(try to_array(items: interp.stack[1] as! List), ["d"])

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         ['x' 'y'] VARIABLES
         [['a' 1] ['b' 2] ['c' 3]] REC x !
         [['a' 20] ['c' 40] ['d' 10]] REC y !
         x @ y @ DIFFERENCE
         y @ x @ DIFFERENCE
         """)
        let record1 = interp.stack[0] as! Record
        let record2 = interp.stack[1] as! Record

        XCTAssertEqual(try to_array(items: Array(record1.keys)), ["b"])
        XCTAssertEqual(try to_array(items: Array(record1.values)), [2])
        XCTAssertEqual(try to_array(items: Array(record2.keys)), ["d"])
        XCTAssertEqual(try to_array(items: Array(record2.values)), [10])
    }

    func test_INTERSECTION() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         ['x' 'y'] VARIABLES
         ['a' 'b' 'c'] x !
         ['a' 'c' 'd'] y !
         x @ y @ INTERSECTION
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), ["a", "c"])

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         ['x' 'y'] VARIABLES
         [['a' 1] ['b' 2] ['f' 3]] REC x !
         [['a' 20] ['c' 40] ['d' 10]] REC y !
         x @ y @ INTERSECTION
         """)
        let record1 = interp.stack[0] as! Record

        XCTAssertEqual(try to_array(items: Array(record1.keys)), ["a"])
        XCTAssertEqual(try to_array(items: Array(record1.values)), [1])
    }

    func test_UNION() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         ['x' 'y'] VARIABLES
         ['a' 'b' 'c'] x !
         ['a' 'c' 'd'] y !
         x @ y @ UNION
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), ["a", "b", "c", "d"])

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         ['x' 'y'] VARIABLES
         [['a' 1] ['b' 2] ['f' 3]] REC x !
         [['a' 20] ['c' 40] ['d' 10]] REC y !
         x @ y @ UNION
         """)
        let record1 = interp.stack[0] as! Record

        XCTAssertEqual(try to_array(items: Array(record1.keys)), ["a", "b", "f", "c", "d"])
        XCTAssertEqual(try to_array(items: Array(record1.values)), [1, 2, 3, 40, 10])
    }

    func test_SELECT() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [0 1 2 3 4 5 6] "2 MOD 1 ==" SELECT
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), [1, 3, 5])

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2] ['c' 3]] REC  "2 MOD 0 ==" SELECT
         """)
        let record1 = interp.stack[0] as! Record

        XCTAssertEqual(try to_array(items: Array(record1.keys)), ["b"])
        XCTAssertEqual(try to_array(items: Array(record1.values)), [2])
    }

    func test_SELECT_w_KEY() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [0 1 2 3 4 5 6] "+ >INT 3 MOD  1 ==" SELECT-w/KEY
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), [2, 5])

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2] ['c' 3]] REC  "CONCAT 'c3' ==" SELECT-w/KEY
         """)
        let record1 = interp.stack[0] as! Record

        XCTAssertEqual(try to_array(items: Array(record1.keys)), ["c"])
        XCTAssertEqual(try to_array(items: Array(record1.values)), [3])
    }

    func test_TAKE() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [0 1 2 3 4 5 6] 3 TAKE
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), [3, 4, 5, 6])
        XCTAssertEqual(try to_array(items: interp.stack[1] as! List), [0, 1, 2])

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2] ['c' 3]] REC  2 TAKE
         """)
        let record_rest = interp.stack[0] as! Record
        let record_taken = interp.stack[1] as! Record

        XCTAssertEqual(try to_array(items: Array(record_rest.keys)), ["c"])
        XCTAssertEqual(try to_array(items: Array(record_rest.values)), [3])
        XCTAssertEqual(try to_array(items: Array(record_taken.keys)), ["a", "b"])
        XCTAssertEqual(try to_array(items: Array(record_taken.values)), [1, 2])
    }

    func test_DROP() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [0 1 2 3 4 5 6] 4 DROP
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), [4, 5, 6])

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2] ['c' 3]] REC  2 DROP
         """)
        let record = interp.stack[0] as! Record

        XCTAssertEqual(try to_array(items: Array(record.keys)), ["c"])
        XCTAssertEqual(try to_array(items: Array(record.values)), [3])
    }

    func test_ROTATE() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         ['a' 'b' 'c' 'd'] ROTATE
         ['b'] ROTATE
         [] ROTATE
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), ["d", "a", "b", "c"])
        XCTAssertEqual(try to_array(items: interp.stack[1] as! List), ["b"])
        XCTAssertEqual((interp.stack[2] as! List).count, 0)

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2] ['c' 3]] REC  ROTATE
         """)
        let record = interp.stack[0] as! Record

        XCTAssertEqual(try to_array(items: Array(record.keys)), ["c", "a", "b"])
    }

    func test_ROTATE_ELEMENT() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         ['a' 'b' 'c' 'd'] 'c' ROTATE-ELEMENT
         ['a' 'b' 'c' 'd'] 'x' ROTATE-ELEMENT
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), ["c", "a", "b", "d"])
        XCTAssertEqual(try to_array(items: interp.stack[1] as! List), ["a", "b", "c", "d"])

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2] ['c' 3]] REC  2 ROTATE-ELEMENT
         """)
        let record = interp.stack[0] as! Record

        XCTAssertEqual(try to_array(items: Array(record.keys)), ["b", "a", "c"])
    }

    func test_SHUFFLE() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [0 1 2 3 4 5 6] SHUFFLE
         """)
        XCTAssertEqual((interp.stack.last as! List).count, 7)

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2] ['c' 3]] REC  SHUFFLE
         """)
        let record = interp.stack[0] as! Record
        XCTAssertEqual(record.count, 3)
    }

    func test_SORT() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [2 8 1 4 7 3] SORT
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), [1, 2, 3, 4, 7, 8])

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 3] ['b' 1] ['c' 2]] REC  SORT
         """)
        let record = interp.stack[0] as! Record
        XCTAssertEqual(try to_array(items: Array(record.keys)), ["b", "c", "a"])
    }

    func test_SORT_w_FORTHIC() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [2 8 1 4 7 3] "-1 *" SORT-w/FORTHIC
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), [8, 7, 4, 3, 2, 1])

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 3] ['b' 1] ['c' 2]] REC  "-1 *" SORT-w/FORTHIC
         """)
        let record = interp.stack[0] as! Record
        XCTAssertEqual(try to_array(items: Array(record.keys)), ["a", "c", "b"])
    }

    func test_NTH() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         ["x"] VARIABLES
         [0 1 2 3 4 5 6] x !
         x @ 0 NTH
         x @ 5 NTH
         x @ 55 NTH
         """)
        XCTAssertEqual(interp.stack[0] as! Int, 0)
        XCTAssertEqual(interp.stack[1] as! Int, 5)
        XCTAssertNil(interp.stack[2])

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         ["x"] VARIABLES
         [['a' 1] ['b' 2] ['c' 3]] REC  x !
         x @ 0 NTH
         x @ 2 NTH
         x @ 55 NTH
         """)
        XCTAssertEqual(interp.stack[0] as! Int, 1)
        XCTAssertEqual(interp.stack[1] as! Int, 3)
        XCTAssertNil(interp.stack[2])
    }

    func test_LAST() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [0 1 2 3 4 5 6] LAST
         """)
        XCTAssertEqual(interp.stack[0] as! Int, 6)

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2] ['c' 3]] REC  LAST
         """)
        XCTAssertEqual(interp.stack[0] as! Int, 3)
    }

    func test_UNPACK() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [0 1 2] UNPACK
         """)
        XCTAssertEqual(interp.stack[0] as! Int, 0)
        XCTAssertEqual(interp.stack[1] as! Int, 1)
        XCTAssertEqual(interp.stack[2] as! Int, 2)

        // Test records
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2] ['c' 3]] REC UNPACK
         """)
        XCTAssertEqual(interp.stack[0] as! Int, 1)
        XCTAssertEqual(interp.stack[1] as! Int, 2)
        XCTAssertEqual(interp.stack[2] as! Int, 3)
    }

    func test_FLATTEN() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         [0 [1 2 [3 [4]] ]] FLATTEN
         """)
        XCTAssertEqual(try to_array(items: interp.stack.last as! List), [0, 1, 2, 3, 4])
    }

    func test_KEY_OF() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         ['x'] VARIABLES
         ['a' 'b' 'c' 'd'] x !
         x @  'c' KEY-OF
         x @  'z' KEY-OF
         """)
        XCTAssertEqual(interp.stack[0] as! Int, 2)
        XCTAssertNil(interp.stack[1])

        // Test record
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2] ['c' 3]] REC  2 KEY-OF
         [['a' 1] ['b' 2] ['c' 3]] REC  9 KEY-OF
         """)
        XCTAssertEqual(interp.stack[0] as! String, "b")
        XCTAssertNil(interp.stack[1])
    }


    func test_REDUCE() throws {
        var interp = Interpreter()
        try interp.run(forthic:
         """
         [1 2 3 4 5] 10 "+" REDUCE >INT
         """)
        XCTAssertEqual(interp.stack[0] as! Int, 25)

        // Test record
        interp = Interpreter()
        try interp.run(forthic:
         """
         [['a' 1] ['b' 2] ['c' 3]] REC  20 "+" REDUCE >INT
         """)
        XCTAssertEqual(interp.stack[0] as! Int, 26)
    }

    func test_SPLIT() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         'Now is the time' ' ' SPLIT
         """)
        XCTAssertEqual(try to_array(items: interp.stack.last as! List), ["Now", "is", "the", "time"])
    }

    func test_JOIN() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         ["Now" "is" "the" "time"] "--" JOIN
         """)
        XCTAssertEqual(interp.stack.last as! String, "Now--is--the--time")
    }

    func test_special_chars() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         /R /N /T
         """)
        XCTAssertEqual(interp.stack[0] as! String, "\r")
        XCTAssertEqual(interp.stack[1] as! String, "\n")
        XCTAssertEqual(interp.stack[2] as! String, "\t")
    }

    func test_LOWER() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         "HOWDY, Everyone!" LOWER
         """)
        XCTAssertEqual(interp.stack[0] as! String, "howdy, everyone!")
    }

    func test_STRIP() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         "  howdy  " STRIP
         """)
        XCTAssertEqual(interp.stack[0] as! String, "howdy")
    }

    func test_REPLACE() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         "1-40 2-20" "-" "." REPLACE
         """)
        XCTAssertEqual(interp.stack[0] as! String, "1.40 2.20")
    }

    func test_RE_MATCH() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         #"""
         "123message456" "(\d{3})(.*)\d{3}" RE-MATCH 2 NTH
         "12message4" "\d{3}.*\d{3}" RE-MATCH 2 NTH
         """#)
        XCTAssertEqual(interp.stack[0] as! String, "message")
        XCTAssertNil(interp.stack[1])
    }

    func test_RE_MATCH_ALL() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         #"""
         "mr-android ios my-android web test-web" ".*?(android|ios|web|seo)" RE-MATCH-ALL "1 NTH" MAP
         """#)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), ["android", "ios", "android", "web", "web"])
    }

    func test_URL_ENCODE() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         "now/is the time" URL-ENCODE
         """)
        XCTAssertEqual(interp.stack.last as! String, "now%2Fis%20the%20time")
    }

    func test_URL_DECODE() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         "now%2Fis%20the%20time" URL-DECODE
         """)
        XCTAssertEqual(interp.stack.last as! String, "now/is the time")
    }

    func test_DEFAULT() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         NULL 22.4 DEFAULT
         0 22.4 DEFAULT
         "" "Howdy" DEFAULT
         """)
        XCTAssertEqual(interp.stack[0] as! Float, 22.4, accuracy: 0.1)
        XCTAssertEqual(interp.stack[1] as! Int, 0)
        XCTAssertEqual(interp.stack[2] as! String, "Howdy")
    }

    func test_star_DEFAULT() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         NULL "3.1 5 +" *DEFAULT
         0 "22.4" *DEFAULT
         "" "['Howdy, ' 'Everyone!'] CONCAT" *DEFAULT
         """)
        XCTAssertEqual(interp.stack[0] as! Double, 8.1, accuracy: 0.1)
        XCTAssertEqual(interp.stack[1] as! Int, 0)
        XCTAssertEqual(interp.stack[2] as! String, "Howdy, Everyone!")
    }

    func test_QUOTE_CHAR() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         QUOTE-CHAR
         """)
        XCTAssertEqual(interp.stack[0] as! Character, DLE)
    }

    func test_QUOTED() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         'Now is the time' QUOTED
         """)
        XCTAssertEqual(interp.stack[0] as! String, "\(DLE)Now is the time\(DLE)")
    }

    func test_l_REPEAT() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         [0 "1 + >INT" 6 <REPEAT]
         """)
        XCTAssertEqual(try to_array(items: interp.stack[0] as! List), [0, 1, 2, 3, 4, 5, 6])
    }

    func test_to_FIXED() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         22 7 / 2 >FIXED
         """)
        XCTAssertEqual(interp.stack.last as! String, "3.14")
    }

    func test_to_JSON() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         ["alpha" "beta"] >JSON
         ["alpha" 1 3.14 TRUE] >JSON
         ["Howdy" ["alpha" "beta"]] >JSON
         [["a" 1] ["b" 2]] REC >JSON
         """)

        XCTAssertEqual(interp.stack[0] as! String, "[\n  \"alpha\",\n  \"beta\"\n]")
        XCTAssertEqual(interp.stack[1] as! String, "[\n  \"alpha\",\n  1,\n  3.1400001049041748,\n  true\n]")
        XCTAssertEqual(interp.stack[2] as! String, "[\n  \"Howdy\",\n  [\n    \"alpha\",\n    \"beta\"\n  ]\n]")
        let rec_json = interp.stack[3] as! String
        XCTAssert(rec_json == "{\n  \"b\" : 2,\n  \"a\" : 1\n}" ||
                  rec_json == "{\n  \"a\" : 1,\n  \"b\" : 2\n}")
    }


    func test_JSON_to() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         '{"a": 1, "b": 2}' JSON>
         '[{"a": 1, "b": 2}]' JSON>
         """)
        var record = interp.stack[0] as! Record
        XCTAssertEqual(Set(record.keys), Set(["a", "b"]))

        let list = interp.stack[1] as! List
        record = list[0] as! Record
        XCTAssertEqual(Set(record.keys), Set(["a", "b"]))
    }

    func test_NOW() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         NOW
         """)
        let calendar = Calendar.current
        let res = interp.stack.last as! Date
        let now = Date()
        XCTAssertEqual(calendar.component(.hour, from: res), calendar.component(.hour, from: now))
        XCTAssertEqual(calendar.component(.minute, from: res), calendar.component(.minute, from: now))
    }

    func test_TIME_to_STR() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         09:17 PM TIME>STR
         """)
        XCTAssertEqual(interp.stack.last as! String, "21:17")
    }

    func test_to_TIME() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         '22:52' >TIME
         """)
        let calendar = Calendar.current
        let res = interp.stack.last as! Date
        XCTAssertEqual(calendar.component(.hour, from: res), 22)
        XCTAssertEqual(calendar.component(.minute, from: res), 52)
    }

    func test_to_DATE() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         '2021-10-02' >DATE
         DUP >DATE
         """)
        let calendar = Calendar.current
        var res = interp.stack[0] as! Date
        XCTAssertEqual(calendar.component(.year, from: res), 2021)
        XCTAssertEqual(calendar.component(.month, from: res), 10)
        XCTAssertEqual(calendar.component(.day, from: res), 2)

        res = interp.stack[1] as! Date
        XCTAssertEqual(calendar.component(.year, from: res), 2021)
        XCTAssertEqual(calendar.component(.month, from: res), 10)
        XCTAssertEqual(calendar.component(.day, from: res), 2)
    }

    func test_TODAY() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         TODAY
         """)
        let calendar = Calendar.current
        let res = interp.stack.last as! Date
        let now = Date()
        XCTAssertEqual(calendar.component(.year, from: res), calendar.component(.year, from: now))
        XCTAssertEqual(calendar.component(.month, from: res), calendar.component(.month, from: now))
        XCTAssertEqual(calendar.component(.day, from: res), calendar.component(.day, from: now))
    }

    func test_days_of_week() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY
         """)
        let calendar = Calendar.current
        let monday = interp.stack[0] as! Date
        let tuesday = interp.stack[1] as! Date
        let wednesday = interp.stack[2] as! Date
        let thursday = interp.stack[3] as! Date
        let friday = interp.stack[4] as! Date
        let saturday = interp.stack[5] as! Date
        let sunday = interp.stack[6] as! Date
        XCTAssertEqual(calendar.component(.weekday, from: monday), 2)
        XCTAssertEqual(calendar.component(.weekday, from: tuesday), 3)
        XCTAssertEqual(calendar.component(.weekday, from: wednesday), 4)
        XCTAssertEqual(calendar.component(.weekday, from: thursday), 5)
        XCTAssertEqual(calendar.component(.weekday, from: friday), 6)
        XCTAssertEqual(calendar.component(.weekday, from: saturday), 7)
        XCTAssertEqual(calendar.component(.weekday, from: sunday), 1)
    }

    func test_NEXT() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         MONDAY NEXT
         """)
        let calendar = Calendar.current
        let monday = interp.stack[0] as! Date
        let today = Date()
        XCTAssertEqual(calendar.component(.weekday, from: monday), 2)
        XCTAssert(monday > today)
    }

    func test_ADD_DAYS() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         2020-10-21 12 ADD-DAYS
         """)
        let calendar = Calendar.current
        let date = interp.stack[0] as! Date
        XCTAssertEqual(calendar.component(.year, from: date), 2020)
        XCTAssertEqual(calendar.component(.month, from: date), 11)
        XCTAssertEqual(calendar.component(.day, from: date), 2)
    }

    func test_SUBTRACT_DATES() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         2020-10-21 2020-11-02 SUBTRACT-DATES
         """)
        let num_days = interp.stack[0] as! Int
        XCTAssertEqual(num_days, -12)
    }

    func test_SUBTRACT_TIMES() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         12:30 10:00 SUBTRACT-TIMES
         """)
        let num_secs = interp.stack[0] as! Int
        XCTAssertEqual(num_secs, 9000)
    }

    func test_DATE_to_STR() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         2020-11-02 DATE>STR
         """)
        let string = interp.stack[0] as! String
        XCTAssertEqual(string, "2020-11-02")
    }

    func test_DATE_TIME_to_DATETIME() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         2020-11-02 10:25 PM DATE-TIME>DATETIME
         """)
        let date1 = interp.stack[0] as! Date
        let calendar = Calendar.current
        XCTAssertEqual(calendar.component(.year, from: date1), 2020)
        XCTAssertEqual(calendar.component(.month, from: date1), 11)
        XCTAssertEqual(calendar.component(.day, from: date1), 2)
        XCTAssertEqual(calendar.component(.hour, from: date1), 22)
        XCTAssertEqual(calendar.component(.minute, from: date1), 25)
    }

    func test_DATETIME_to_TIMESTAMP() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         2020-07-01 15:20 DATE-TIME>DATETIME DATETIME>TIMESTAMP
         """)
        let timestamp = interp.stack[0] as! Int
        XCTAssertEqual(timestamp, 1593642000)
    }

    func test_TIMESTAMP_to_DATETIME() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         1593895532 TIMESTAMP>DATETIME
         """)
        let date = interp.stack[0] as! Date
        let calendar = Calendar.current

        XCTAssertEqual(calendar.component(.year, from: date), 2020)
        XCTAssertEqual(calendar.component(.month, from: date), 7)
        XCTAssertEqual(calendar.component(.day, from: date), 4)
        XCTAssertEqual(calendar.component(.hour, from: date), 13)
        XCTAssertEqual(calendar.component(.minute, from: date), 45)
    }

    func test_arithmetic() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         2 4 +
         2 4 -
         2 4 *
         2 4 /
         5 3 MOD
         [1 2 3] +
         [2 3 4] *
         """)

        XCTAssertEqual(interp.stack[0] as! Double, 6, accuracy: 0.1)
        XCTAssertEqual(interp.stack[1] as! Double, -2, accuracy: 0.1)
        XCTAssertEqual(interp.stack[2] as! Double, 8, accuracy: 0.1)
        XCTAssertEqual(interp.stack[3] as! Double, 0.5, accuracy: 0.1)
        XCTAssertEqual(interp.stack[4] as! Int, 2)
        XCTAssertEqual(interp.stack[5] as! Double, 6, accuracy: 0.1)
        XCTAssertEqual(interp.stack[6] as! Double, 24, accuracy: 0.1)
    }

    func test_MAX() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         [3 2 12] MAX
         ["now" "is" "the" "time"] MAX
         [2021-08-05 2021-09-10 2020-12-14] MAX 2021-09-10 ==
         """)

        XCTAssertEqual(interp.stack[0] as! Int, 12)
        XCTAssertEqual(interp.stack[1] as! String, "time")
        XCTAssertEqual(interp.stack[2] as! Bool, true)
    }

    func test_MIN() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         [3 2 12] MIN
         ["now" "is" "the" "time"] MIN
         [2021-08-05 2021-09-10 2020-12-14] MIN  2020-12-14 ==
         """)

        XCTAssertEqual(interp.stack[0] as! Int, 2)
        XCTAssertEqual(interp.stack[1] as! String, "is")
        XCTAssertEqual(interp.stack[2] as! Bool, true)
    }

    func test_comparison() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         2 4 ==
         2 4 !=
         2 4 <
         2 4 <=
         2 4 >
         2 4 >=
         """)

        XCTAssertEqual(interp.stack[0] as! Bool, false)
        XCTAssertEqual(interp.stack[1] as! Bool, true)
        XCTAssertEqual(interp.stack[2] as! Bool, true)
        XCTAssertEqual(interp.stack[3] as! Bool, true)
        XCTAssertEqual(interp.stack[4] as! Bool, false)
        XCTAssertEqual(interp.stack[5] as! Bool, false)
    }

    func test_logic() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         FALSE FALSE OR
         [FALSE FALSE TRUE FALSE] OR
         FALSE TRUE AND
         [FALSE FALSE TRUE FALSE] AND
         FALSE NOT
         """)

        XCTAssertEqual(interp.stack[0] as! Bool, false)
        XCTAssertEqual(interp.stack[1] as! Bool, true)
        XCTAssertEqual(interp.stack[2] as! Bool, false)
        XCTAssertEqual(interp.stack[3] as! Bool, false)
        XCTAssertEqual(interp.stack[4] as! Bool, true)
    }
    
    func test_IN() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         "alpha" ["beta" "gamma"] IN
         "alpha" ["beta" "gamma" "alpha"] IN
         """)

        XCTAssertEqual(interp.stack[0] as! Bool, false)
        XCTAssertEqual(interp.stack[1] as! Bool, true)
    }

    func test_ANY() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         ["alpha" "beta"] ["beta" "gamma"] ANY
         ["delta" "beta"] ["gamma" "alpha"] ANY
         ["alpha" "beta"] [] ANY
         """)

        XCTAssertEqual(interp.stack[0] as! Bool, true)
        XCTAssertEqual(interp.stack[1] as! Bool, false)
        XCTAssertEqual(interp.stack[2] as! Bool, true)
    }
    
    func test_ALL() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         ["alpha" "beta"] ["beta" "gamma"] ALL
         ["delta" "beta"] ["beta"] ALL
         ["alpha" "beta"] [] ALL
         """)

        XCTAssertEqual(interp.stack[0] as! Bool, false)
        XCTAssertEqual(interp.stack[1] as! Bool, true)
        XCTAssertEqual(interp.stack[2] as! Bool, true)
    }

    func test_math_converters() throws {
        let interp = Interpreter()
        try interp.run(forthic:
         """
         NULL >BOOL
         0 >BOOL
         1 >BOOL
         "" >BOOL
         "Hi" >BOOL
         "3" >INT
         4 >INT
         4.6 >INT
         "1.2" >DOUBLE
         2 >DOUBLE
         """)

        XCTAssertEqual(interp.stack[0] as! Bool, false)
        XCTAssertEqual(interp.stack[1] as! Bool, false)
        XCTAssertEqual(interp.stack[2] as! Bool, true)
        XCTAssertEqual(interp.stack[3] as! Bool, false)
        XCTAssertEqual(interp.stack[4] as! Bool, true)
        
        XCTAssertEqual(interp.stack[5] as! Int, 3)
        XCTAssertEqual(interp.stack[6] as! Int, 4)
        XCTAssertEqual(interp.stack[7] as! Int, 4)
        XCTAssertEqual(interp.stack[8] as! Double, 1.2, accuracy: 0.1)
        XCTAssertEqual(interp.stack[9] as! Double, 2.0, accuracy: 0.1)
    }
}

// ----- Helper functions -------------------------------------------------------------------
func to_array<T>(items: List) throws -> [T] {
    var res: [T] = []
    for item in items {
        res.append(item as! T)
    }
    return res
}

func make_sample_records() -> [Record] {
    let data = [
        [100, "user1", "OPEN"],
        [101, "user1", "OPEN"],
        [102, "user1", "IN PROGRESS"],
        [103, "user1", "CLOSED"],
        [104, "user2", "IN PROGRESS"],
        [105, "user2", "OPEN"],
        [106, "user2", "CLOSED"]
    ]

    var result: [Record] = []
    for d in data {
        var rec: Record = [:]
        rec["key"] = d[0]
        rec["assignee"] = d[1]
        rec["status"] = d[2]
        result.append(rec)
    }
    return result
}
