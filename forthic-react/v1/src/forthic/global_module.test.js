import {Interpreter} from "./interpreter"

let DLE = String.fromCharCode(16);  // ASCII char for "Data Link Escape" used as an untypeable quote

it('Has support for word literals', async () => {
    let interp = new Interpreter()
    await interp.run("TRUE FALSE 2  3.14")
    expect(interp.stack).toEqual([true, false, 2, 3.14])
});

it('Has variables', async () => {
    let interp = new Interpreter()
    let variables = interp.app_module.variables
    expect(variables['x']).toBeUndefined()
    expect(variables['y']).toBeUndefined()
    await interp.run("['x' 'y']  VARIABLES")
    expect(variables['x']).toBeTruthy()
    expect(variables['y']).toBeTruthy()
})

it('Can set variables', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['x']  VARIABLES
        24 x !
    `)
    expect(interp.app_module.variables['x'].value).toEqual(24)

    await interp.run("x @")
    expect(interp.stack[0]).toEqual(24)
})

it('Can set and retrieve the value of a variable in one step', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['x']  VARIABLES
        24 x !@
    `)
    expect(interp.stack[0]).toEqual(24)
})

it('Can interpret arbitrary Forthic strings', async () => {
    let interp = new Interpreter()
    await interp.run("'24' INTERPRET")
    expect(interp.stack[0]).toEqual(24)

    interp = new Interpreter()
    await interp.run(`
        '{module-A  : MESSAGE   "Hi" ;}' INTERPRET
        {module-A MESSAGE}
    `)
    expect(interp.stack[0]).toEqual('Hi')
})

it('Can MEMO-ize definitions', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['count'] VARIABLES
        0 count !
        @: COUNT   count @ 1 +  count !@;
    `)

    await interp.run("COUNT")
    expect(interp.stack[0]).toEqual(1)

    await interp.run("COUNT")
    expect(interp.stack[1]).toEqual(1)

    await interp.run("COUNT! COUNT")
    expect(interp.stack[2]).toEqual(2)

    await interp.run("COUNT!@")
    expect(interp.stack[3]).toEqual(3)
})

it('Can create a record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
    `)
    expect(interp.stack[0]).toEqual({alpha: 2, beta: 3, gamma: 4})

})


it('Can retrieve container values using REC@', async () => {
    // Get value from record
    let interp = new Interpreter()
    await interp.run(`
        [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
        'beta' REC@
        `)
    expect(interp.stack).toEqual([3])

    // Get value from array
    interp.stack = []
    await interp.run(`
        [10 20 30 40 50] 3 REC@
    `)
    expect(interp.stack).toEqual([40])
});

it('Can retrieve nested container values using REC@', async () => {
    // Get value from nested record
    let interp = new Interpreter()
    await interp.run(`
        [ ["alpha" [["alpha1" 20]] REC]
        ["beta" [["beta1"  30]] REC]
        ] REC
        ["beta" "beta1"] REC@
    `)
    expect(interp.stack).toEqual([30])

    // Get value from nested array
    interp.stack = []
    await interp.run(`
        [ [] [] [[3]] ]
        [2 0 0] REC@
    `)
    expect(interp.stack).toEqual([3])

    // Get value from nested record/arrays
    interp.stack = []
    await interp.run(`
        [ ["alpha" [["alpha1" 20]] REC]
        ["beta" [["beta1"  [10 20 30]]] REC]
        ] REC
        ["beta" "beta1" 1] REC@
    `)
    expect(interp.stack).toEqual([20])
});


it('Can set a record field value', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [ ["alpha" 2] ["beta" 3] ["gamma" 4] ] REC
        700 'beta' <REC!
    `)
    expect(interp.stack[0]).toEqual({alpha: 2, beta: 700, gamma: 4})
})

it('Can set a nested record value', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [] REC "Green" ["2021-03-22" "TEST-1234"] <REC!
    `)
    expect(interp.stack[0]).toEqual({"2021-03-22": {"TEST-1234": "Green"}})
})

it('Can set a value on a null record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        NULL 700 'beta' <REC!
    `)
    expect(interp.stack[0]).toEqual({beta: 700})
})

it('Can append elements to an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [1 2 3] 4 APPEND
    `)
    expect(interp.stack[0]).toEqual([1, 2, 3, 4])
})

it('Can appen elements to a record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [["a" 1] ["b" 2]] REC  ["c" 3] APPEND
    `)
    expect(interp.stack[0]).toEqual({a: 1, b: 2, c: 3})
})

it('Can reverse an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [1 2 3] REVERSE
    `)
    expect(interp.stack[0]).toEqual([3, 2, 1])
})

it('Can return an array with unique elements', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [1 2 3 3 2] UNIQUE ">INT" MAP
    `)
    expect(interp.stack[0]).toEqual([1, 2, 3])
})

it('Can delete an element of an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ["a" "b" "c"] 1 <DEL
    `)
    expect(interp.stack[0]).toEqual(["a", "c"])
})

it('Can delete a field from a record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [["a" 1] ["b" 2] ["c" 3]] REC  "b" <DEL
    `)
    expect(interp.stack[0]).toEqual({a: 1, c: 3})

    interp = new Interpreter()
    await interp.run(`
        [["a" 1] ["b" 2] ["c" 3]] REC  "d" <DEL
    `)
    expect(interp.stack[0]).toEqual({a: 1, b: 2, c: 3})
})

it('Can reorder elements of an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ["a" "b" "c"] [0 2] [25 23] RELABEL
    `)
    expect(interp.stack[0]).toEqual(["c", "a"])
})

it('Can relabel fields of a record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [["a" 1] ["b" 2] ["c" 3]] REC  ["a" "c"] ["alpha" "gamma"] RELABEL
    `)
    expect(interp.stack[0]).toEqual({alpha: 1, gamma: 3})
})

it('Can organize an array by field', async () => {
    let interp = new Interpreter()
    interp.stack_push(make_records())
    await interp.run("'key' BY-FIELD")
    expect(interp.stack[0][104]).toEqual({"assignee": "user2", "key": 104, "status": "IN PROGRESS"})
});

it('Can organize an array with NULLs by field', async () => {
    let interp = new Interpreter()
    interp.stack_push(make_records().concat([null, null]))
    await interp.run("'key' BY-FIELD")
    expect(interp.stack[0][104]).toEqual({"assignee": "user2", "key": 104, "status": "IN PROGRESS"})
});

it('Can group an array of items by field', async () => {
    let interp = new Interpreter()
    interp.stack_push(make_records())
    await interp.run("'assignee' GROUP-BY-FIELD")
    let grouped = interp.stack[0]
    expect(Object.keys(grouped).sort()).toEqual(["user1", "user2"])
    expect(grouped['user1'].length).toEqual(4)
    expect(grouped['user2'].length).toEqual(3)
})

it('Can group a record by field', async () => {
    let interp = new Interpreter()
    let by_key = make_records_by_key()
    interp.stack_push(by_key)

    await interp.run("'assignee' GROUP-BY-FIELD")
    let grouped_rec = interp.stack[0]
    expect(Object.keys(grouped_rec).sort()).toEqual(["user1", "user2"])
    expect(grouped_rec['user1'].length).toEqual(4)
    expect(grouped_rec['user2'].length).toEqual(3)
})

it('Can group a list-valued field', async () => {
    let interp = new Interpreter()
    interp.stack_push([{"id": 1, "attrs":["blue", "important"]}, {"id": 2, "attrs":["red"]}])
    await interp.run("'attrs' GROUP-BY-FIELD")
    let grouped_rec = interp.stack[0]
    expect(grouped_rec["blue"][0]["id"]).toEqual(1)
    expect(grouped_rec["important"][0]["id"]).toEqual(1)
    expect(grouped_rec["red"][0]["id"]).toEqual(2)
})

it('Can group an array by arbitrary Forthic', async () => {
    let interp = new Interpreter()
    interp.stack_push(make_records())
    await interp.run(`
        "'assignee' REC@" GROUP-BY
    `)
    let grouped = interp.stack[0]
    expect(Object.keys(grouped).sort()).toEqual(["user1", "user2"])
    expect(grouped['user1'].length).toEqual(4)
    expect(grouped['user2'].length).toEqual(3)
})

it('Can group a record by arbitrary Forthic', async () => {
    let interp = new Interpreter()
    let by_key = make_records_by_key()
    interp.stack_push(by_key)

    await interp.run(`
        "'assignee' REC@" GROUP-BY
    `)
    let grouped = interp.stack[0]
    expect(Object.keys(grouped).sort()).toEqual(["user1", "user2"])
    expect(grouped['user1'].length).toEqual(4)
    expect(grouped['user2'].length).toEqual(3)
})

it('Can group an array by arbitrary Forthic that also expects the array index', async () => {
    let interp = new Interpreter()
    interp.stack_push(make_records())
    await interp.run(`
        ['key' 'val'] VARIABLES
        "val ! key ! key @ 3 MOD" !WITH-KEY GROUP-BY
    `)
    let grouped = interp.stack[0]
    expect(Object.keys(grouped).sort()).toEqual(['0', '1', '2'])
    expect(grouped[0].length).toEqual(3)
    expect(grouped[1].length).toEqual(2)
    expect(grouped[2].length).toEqual(2)
})

it('Can group a record by arbitrary Forthic that also expects a record key', async () => {
    let interp = new Interpreter()
    let by_key = make_records_by_key()
    interp.stack_push(by_key)

    await interp.run(`
        ['key' 'val'] VARIABLES
        "val ! key ! key @ 2 *" !WITH-KEY GROUP-BY
    `)
    let grouped = interp.stack[0]
    expect(Object.keys(grouped).sort()).toEqual(['200', '202', '204', '206', '208', '210', '212'])
})

it('Can create groups of elements from an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [1 2 3 4 5 6 7 8] 3 GROUPS-OF
    `)
    expect(interp.stack[0]).toEqual([[1, 2, 3], [4, 5, 6], [7, 8]])
})

it('Can create groups of elements from a record', async () => {
    let interp = new Interpreter()
    let by_key = make_records_by_key()
    interp.stack_push(by_key)
    await interp.run(`
        3 GROUPS-OF
    `)
    let recs = interp.stack[0]
    expect(Object.keys(by_key).length).toEqual(7)
    expect(Object.keys(recs[0]).length).toEqual(3)
    expect(Object.keys(recs[1]).length).toEqual(3)
    expect(Object.keys(recs[2]).length).toEqual(1)
})

it('Can construct indexes on multi-valued fields', async () => {
    let interp = new Interpreter()
    await interp.run(`
        : TICKETS [
            [['key'   101] ['Labels'  ['alpha' 'beta']]] REC
            [['key'   102] ['Labels'  ['alpha' 'gamma']]] REC
            [['key'   103] ['Labels'  ['alpha']]] REC
            [['key'   104] ['Labels'  ['beta']]] REC
        ];

        TICKETS "'Labels' REC@" INDEX
    `)
    function to_keys(items) {
        return items.map(item => item.key)
    }
    expect(to_keys(interp.stack[0]['alpha'])).toEqual([101, 102, 103])
    expect(to_keys(interp.stack[0]['beta'])).toEqual([101, 104])
    expect(to_keys(interp.stack[0]['gamma'])).toEqual([102])
})


it('Can map Forthic over an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [1 2 3 4 5] '2 *' MAP
    `)
    let array = interp.stack[0]
    expect(array).toEqual([2, 4, 6, 8, 10])
})

it ('Can map Forthic over a record', async () => {
    let interp = new Interpreter()
    let by_key = make_records_by_key()
    interp.stack_push(by_key)
    await interp.run(`
        "'status' REC@" MAP
    `)
    let record = interp.stack[0]
    expect(record[100]).toEqual("OPEN")
    expect(record[102]).toEqual("IN PROGRESS")
    expect(record[106]).toEqual("CLOSED")
})

it ('Can use MAP in a module', async () => {
    let interp = new Interpreter()
    await interp.run(`
        {my-module
            : DOUBLE   2 *;
            : RUN   [1 2 3 4 5] "DOUBLE" MAP;
        }
        {my-module RUN}
    `)
    let array = interp.stack[0]
    expect(array).toEqual([2, 4, 6, 8, 10])
})

it ('Can apply MAP at arbitrary depth in a nested record', async () => {
    let interp = new Interpreter()
    await interp.run(`
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
    `)
    let record = interp.stack[0]
    expect(record).toEqual({'k1': {'l1': {'m': 4}, 'l2': {'m': 6}}, 'k2': {'l1': {'m': 6}, 'l2': {'m': 8}}})
})

it ('Can apply MAP at arbitrary depth in a nested array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        : DEEP-LIST [ [ [2 3] ] [ [3 4] ] ];

        DEEP-LIST "2 *" 2 !DEPTH MAP
        `)
    let array = interp.stack[0]
    expect(array).toEqual([ [ [4, 6] ], [ [6, 8] ] ])
})

it ('Can apply MAP at arbitrary depth in a nested array/record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        : DEEP-LIST [ [ [[["m"  2]] REC [["m"  3]] REC] ] [ [[["m"  3]] REC [["m"  4]] REC] ] ];

        DEEP-LIST "2 *" 3 !DEPTH MAP
        # [ [ [4 6] ] [ [6 8] ] ]
    `)
    let array = interp.stack[0]
    expect(array).toEqual([[[{"m": 4}, {"m": 6}]], [[{"m": 6}, {"m": 8}]]])
})

it ('Can MAP at depth, pushing errors', async () => {
    let interp = new Interpreter()
    await interp.run(`
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
    `)
    let record = interp.stack[0]
    let errors = interp.stack[1]
    expect(record).toEqual({'k1': {'l1': {'m': 2}, 'l2': {'m': 3}}, 'k2': {'l1': {'m': null}, 'l2': {'m': 4}}})
    expect(errors).toEqual([null, null, "Unknown word: GARBAGE", null])
})

it ('Can MAP a Forthic string, expecting a key, over an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [1 2 3 4 5] '+ 2 *' !WITH-KEY MAP
    `)
    let array = interp.stack[0]
    expect(array).toEqual([2, 6, 10, 14, 18])
})

it ('Can MAP a Forthic string, expecting a key, over a record', async () => {
    let interp = new Interpreter()
    let by_key = make_records_by_key()
    interp.stack_push(by_key)
    await interp.run(`
        ["k" "v"] VARIABLES
        "v ! k ! k @ >STR v @ 'status' REC@ CONCAT" !WITH-KEY MAP
    `)
    let record = interp.stack[0]
    expect(record[100]).toEqual("100OPEN")
    expect(record[102]).toEqual("102IN PROGRESS")
    expect(record[106]).toEqual("106CLOSED")
})

it ('Can execute Forthic for each element in an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        0 [1 2 3 4 5] '+' FOREACH
    `)
    let sum = interp.stack[0]
    expect(sum).toEqual(15)
})

it ('Can execute Forthic for each element in a record', async () => {
    let interp = new Interpreter()
    let by_key = make_records_by_key()
    interp.stack_push(by_key)
    await interp.run(`
        "" SWAP "'status' REC@ CONCAT" FOREACH
    `)
    let string = interp.stack[0]
    expect(string).toEqual("OPENOPENIN PROGRESSCLOSEDIN PROGRESSOPENCLOSED")
})

it ('Can execute Forthic for each element in an array, pushing the index', async () => {
    let interp = new Interpreter()
    await interp.run(`
        0 [1 2 3 4 5] '+ +' !WITH-KEY FOREACH
    `)
    let sum = interp.stack[0]
    expect(sum).toEqual(25)
})

it ('Can execute Forthic for each element in a record, pushing the key', async () => {
    let interp = new Interpreter()
    let by_key = make_records_by_key()
    interp.stack_push(by_key)
    await interp.run(`
        "" SWAP "'status' REC@ CONCAT CONCAT" !WITH-KEY FOREACH
    `)
    let string = interp.stack[0]
    expect(string).toEqual("100OPEN101OPEN102IN PROGRESS103CLOSED104IN PROGRESS105OPEN106CLOSED")
})

it ('Can execute Forthic for each element in an array, pushing errors at the end', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['2' '3' 'GARBAGE' '+'] 'INTERPRET' !PUSH-ERROR FOREACH
    `)
    let result = interp.stack[0]
    let errors = interp.stack[1]
    expect(result).toEqual(5)
    expect(errors).toEqual([null, null, "Unknown word: GARBAGE", null])
})

it ('Can invert the top two levels of a nested record', async () => {
    let interp = new Interpreter()
    let status_to_manager_to_ids = make_status_to_manager_to_ids()
    interp.stack_push(status_to_manager_to_ids)
    await interp.run(`
        INVERT-KEYS
    `)
    expect(interp.stack[0]).toEqual({
      manager1: {
        open: [101, 102],
        closed: [10, 11],
      },
      manager2: {
        open: [103],
        closed: [12, 13],
      },
      manager3: {
        blocked: [104],
      },
    });
})

it ('Can zip two arrays together', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['a' 'b'] [1 2] ZIP
    `)
    expect(interp.stack[0]).toEqual([['a', 1], ['b', 2]])
})

it ('Can zip two records together', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [['a' 100] ['b' 200] ['z' 300]] REC [['a' 'Hi'] ['b' 'Bye'] ['c' '?']] REC ZIP
    `)
    expect(interp.stack[0]).toEqual({
        a: [100, 'Hi'],
        b: [200, 'Bye'],
        z: [300, undefined]
    })

})

it ('Can zip an array with a Forthic string', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [10 20] [1 2] "+" ZIP-WITH
    `)
    expect(interp.stack[0]).toEqual([11, 22])
})

it ('Can zip a record with a Forthic string', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [['a' 1] ['b' 2]] REC [['a' 10] ['b' 20]] REC "+" ZIP-WITH
    `)
    expect(interp.stack[0]).toEqual({
        a: 11,
        b: 22
    })
})

it ('Can return the indexes of an array of items', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['a' 'b' 'c'] KEYS
    `)
    expect(interp.stack[0]).toEqual([0, 1, 2])
})

it ('Can return the keys of a record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [['a' 1] ['b' 2]] REC KEYS
    `)
    expect(interp.stack[0]).toEqual(['a', 'b'])
})

it ('Can return the values of an array (a no-op)', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['a' 'b' 'c'] VALUES
    `)
    expect(interp.stack[0]).toEqual(['a', 'b', 'c'])
})

it ('Can return the values of a record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [['a' 1] ['b' 2]] REC VALUES
    `)
    expect(interp.stack[0]).toEqual([1, 2])
})

it ('Can return the length of an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['a' 'b' 'c'] LENGTH
    `)
    expect(interp.stack[0]).toEqual(3)
})

it ('Can return the length of a record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [['a' 1] ['b' 2]] REC LENGTH
    `)
    expect(interp.stack[0]).toEqual(2)
})

it ('Can slice arrays Pythonically-ish', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['x'] VARIABLES
        ['a' 'b' 'c' 'd' 'e' 'f' 'g'] x !
        x @ 0 2 SLICE
        x @ 1 3 SLICE
        x @ 5 3 SLICE
        x @ -1 -2 SLICE
        x @ 4 -2 SLICE
        x @ 5 8 SLICE
    `)
    expect(interp.stack[0]).toEqual(['a', 'b', 'c'])
    expect(interp.stack[1]).toEqual(['b', 'c', 'd'])
    expect(interp.stack[2]).toEqual(['f', 'e', 'd'])
    expect(interp.stack[3]).toEqual(['g', 'f'])
    expect(interp.stack[4]).toEqual(['e', 'f'])
    expect(interp.stack[5]).toEqual(['f', 'g', null, null])
})

it ('Can take the difference between two arrays', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['x' 'y'] VARIABLES
        ['a' 'b' 'c'] x !
        ['a' 'c' 'd'] y !
        x @ y @ DIFFERENCE
        y @ x @ DIFFERENCE
        `)
    expect(interp.stack[0]).toEqual(['b'])
    expect(interp.stack[1]).toEqual(['d'])
})

it ('Can take the difference between two records', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['x' 'y'] VARIABLES
        [['a' 1] ['b' 2] ['c' 3]] REC x !
        [['a' 20] ['c' 40] ['d' 10]] REC y !
        x @ y @ DIFFERENCE
        y @ x @ DIFFERENCE
    `)
    expect(interp.stack[0]).toEqual({b: 2})
    expect(interp.stack[1]).toEqual({d: 10})
})

it ('Can find the intersection between two arrays', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['x' 'y'] VARIABLES
        ['a' 'b' 'c'] x !
        ['a' 'c' 'd'] y !
        x @ y @ INTERSECTION
    `)
    expect(interp.stack[0]).toEqual(['a', 'c'])
})

it ('Can find the intersection between two records by key', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['x' 'y'] VARIABLES
        [['a' 1] ['b' 2] ['d' 3]] REC x !
        [['a' 20] ['c' 40] ['d' 10]] REC y !
        x @ y @ INTERSECTION
    `)
    expect(interp.stack[0]).toEqual({a: 1, d: 3})
})

it ('Can find the union between two arrays', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['x' 'y'] VARIABLES
        ['a' 'b' 'c'] x !
        ['a' 'c' 'd'] y !
        x @ y @ UNION
    `)
    expect(interp.stack[0]).toEqual(['a', 'b', 'c', 'd'])
})

it ('Can find the union between two records by key', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['x' 'y'] VARIABLES
        [['a' 1] ['b' 2] ['f' 3]] REC x !
        [['a' 20] ['c' 40] ['d' 10]] REC y !
        x @ y @ UNION
    `)
    expect(interp.stack[0]).toEqual({a: 1, b: 2, c: 40, d: 10, f: 3})
})

it ('Can select items from an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [0 1 2 3 4 5 6] "2 MOD 1 ==" SELECT
    `)
    expect(interp.stack[0]).toEqual([1, 3, 5])
})

it ('Can select items from a record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [['a' 1] ['b' 2] ['c' 3]] REC  "2 MOD 1 ==" SELECT
    `)
    expect(interp.stack[0]).toEqual({a:1, c: 3})
})

it ('Can select items from an array using the array index', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [0 1 2 3 4 5 6] "+ 3 MOD 1 ==" !WITH-KEY SELECT
    `)
    expect(interp.stack[0]).toEqual([2, 5])
})

it ('Can select items from a record using the field key', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [['a' 1] ['b' 2] ['c' 3]] REC  "CONCAT 'c3' ==" !WITH-KEY SELECT
    `)
    expect(interp.stack[0]).toEqual({c: 3})
})


it ('Can take elements from an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [0 1 2 3 4 5 6] 3 TAKE
    `)
    let array = interp.stack[0]
    expect(array).toEqual([0, 1, 2])
})

it ('Can take elements from a record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [['a' 1] ['b' 2] ['c' 3]] REC  2 TAKE
    `)
    let record = interp.stack[0]
    expect(Object.keys(record).length).toEqual(2)
})

it ('Can take elements from an array, returning the rest', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [0 1 2 3 4 5 6] 3 !PUSH-REST TAKE
    `)
    let array = interp.stack[0]
    let rest = interp.stack[1]
    expect(array).toEqual([0, 1, 2])
    expect(rest).toEqual([3, 4, 5, 6])
})

it ('Can drop elements from an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [0 1 2 3 4 5 6] 4 DROP
    `)
    expect(interp.stack[0]).toEqual([4, 5, 6])
})

it ('Can rotate an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['a' 'b' 'c' 'd'] ROTATE
        ['b'] ROTATE
        [] ROTATE
    `)
    expect(interp.stack[0]).toEqual(['d', 'a', 'b', 'c'])
    expect(interp.stack[1]).toEqual(['b'])
    expect(interp.stack[2]).toEqual([])
})


it ('Can shuffle array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [0 1 2 3 4 5 6] SHUFFLE
    `)
    expect(interp.stack[0].length).toEqual(7)
})

it ('Can sort an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [2 8 1 4 7 3] SORT
    `)
    expect(interp.stack[0]).toEqual([1, 2, 3, 4, 7, 8])
})

it ('Can sort an array with NULLs', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [2 8 1 NULL 4 7 NULL 3] SORT
    `)
    expect(interp.stack[0]).toEqual([1, 2, 3, 4, 7, 8, null, null])
})

it ('Can sort an array with a Forthic string', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [2 8 1 4 7 3] "-1 *" !COMPARATOR SORT
    `)
    expect(interp.stack[0]).toEqual([8, 7, 4, 3, 2, 1])
})


it ('Can sort an array with a key func', async () => {
    let interp = new Interpreter()
    interp.stack_push(make_records())
    await interp.run(`
        'status' FIELD-KEY-FUNC !COMPARATOR SORT
    `)
    expect(interp.stack[0][0]["status"]).toEqual("CLOSED")
})

it ('Can get the nth element of an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ["x"] VARIABLES
        [0 1 2 3 4 5 6] x !
        x @ 0 NTH
        x @ 5 NTH
        x @ 55 NTH
    `)
    expect(interp.stack[0]).toEqual(0)
    expect(interp.stack[1]).toEqual(5)
    expect(interp.stack[2]).toEqual(null)
})

it ('Can get the last element of an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [0 1 2 3 4 5 6] LAST
    `)
    expect(interp.stack[0]).toEqual(6)
})

it ('Can unpack an array onto the stack', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [0 1 2] UNPACK
    `)
    expect(interp.stack[0]).toEqual(0)
    expect(interp.stack[1]).toEqual(1)
    expect(interp.stack[2]).toEqual(2)
})

it ('Can flatten all levels of a nested array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [0 [1 2 [3 [4]] ]] FLATTEN
    `)
    let array = interp.stack[0]
    expect(array).toEqual([0, 1, 2, 3, 4])
})

it ('Can flatten all levels of a nested record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['uno' 'alpha'] VARIABLES
        [['uno' 4] ['duo' 8]] REC uno !
        [['alpha' uno @]] REC alpha !
        [['a' 1] ['b' alpha @] ['c' 3]] REC FLATTEN
    `)
    let record = interp.stack[0]
    expect(Object.keys(record).sort()).toEqual(['a', 'b\talpha\tduo', 'b\talpha\tuno', 'c'])
})


it ('Can flatten one level of a nested array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [ [ [0 1] [2 3] ]
          [ [4 5]       ] ] 1 !DEPTH FLATTEN
    `)
    let array = interp.stack[0]
    expect(array).toEqual([[0, 1] , [2, 3], [4, 5]])
})

it ('Can flatten one level of a nested record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['uno' 'alpha'] VARIABLES
        [['uno' 4] ['duo' 8]] REC uno !
        [['alpha' uno @]] REC alpha !
        [['a' 1] ['b' alpha @] ['c' 3]] REC 1 !DEPTH FLATTEN
    `)
    let record = interp.stack[0]
    expect(Object.keys(record).sort()).toEqual(['a', 'b\talpha', 'c'])
})

it ('Can return the index of the first value in an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ['a' 'b' 'c' 'd'] 'c' KEY-OF
        ['a' 'b' 'c' 'd'] 'z' KEY-OF
    `)
    expect(interp.stack[0]).toEqual(2)
    expect(interp.stack[1]).toEqual(null)
})

it ('Can return the key of the first value in a record', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [['a' 1] ['b' 2] ['c' 3]] REC  2 KEY-OF
        [['a' 1] ['b' 2] ['c' 3]] REC  100 KEY-OF
    `)
    expect(interp.stack[0]).toEqual('b')
    expect(interp.stack[1]).toEqual(null)
})

it ('Can reduce the values of an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [1 2 3 4 5] 10 "+" REDUCE
    `)
    expect(interp.stack[0]).toEqual(25)
})

it ('Can pop elements from the stack', async () => {
    let interp = new Interpreter()
    await interp.run(`
        1 2 3 4 5 POP
    `)
    expect(interp.stack).toEqual([1, 2, 3, 4])
})

it ('Can duplicate the top element of the stack', async () => {
    let interp = new Interpreter()
    await interp.run(`
        1 2 3 4 5 DUP
    `)
    expect(interp.stack).toEqual([1, 2, 3, 4, 5, 5])
})

it ('Can swap the top two elements of the stack', async () => {
    let interp = new Interpreter()
    await interp.run(`
        1 2 3 4 5 SWAP
    `)
    expect(interp.stack).toEqual([1, 2, 3, 5, 4])
})


it ('Can split a string with a separator', async () => {
    let interp = new Interpreter()
    await interp.run(`
        'Now is the time' ' ' SPLIT
    `)
    expect(interp.stack[0]).toEqual(["Now", "is", "the", "time"])
})

it ('Can join an array of strings into a string', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ["Now" "is" "the" "time"] "--" JOIN
    `)
    expect(interp.stack[0]).toEqual("Now--is--the--time")
})

it ('Can construct special characters', async () => {
    let interp = new Interpreter()
    await interp.run(`
        /R /N /T
    `)
    expect(interp.stack).toEqual(['\r', '\n', '\t'])
})

it ('Can lowercase a string', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "HOWDY, Everyone!" LOWERCASE
    `)
    expect(interp.stack[0]).toEqual("howdy, everyone!")
})

it ('Can uppercase a string', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "HOWDY, Everyone!" UPPERCASE
    `)
    expect(interp.stack[0]).toEqual("HOWDY, EVERYONE!")
})

it ('Can remove non-ASCII chars from a string', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "HOWDY, “Everyone!”" |ASCII
    `)
    expect(interp.stack[0]).toEqual("HOWDY, Everyone!")
})

it ('Can strip leading and trailing whitespace', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "  howdy  " STRIP
    `)
    expect(interp.stack[0]).toEqual("howdy")
})

it ('Can replace substrings', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "1-40 2-20" "-" "." REPLACE
    `)
    expect(interp.stack[0]).toEqual("1.40 2.20")
})

it ('Can replace substrings using regex', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "Howdy https://www.linkedin.com" "(https?://\\S+)" "=HYPERLINK('$1', '$1')" REPLACE
    `)
    expect(interp.stack[0]).toEqual("Howdy =HYPERLINK('https://www.linkedin.com', 'https://www.linkedin.com')")
})


it ('Can match regex', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "123message456" "\\d{3}.*\\d{3}" RE-MATCH
    `)
    expect(interp.stack[0]).toBeTruthy()
})

it ('Can extract groups from a regex match', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "123message456" "\\d{3}(.*)\\d{3}" RE-MATCH 1 RE-MATCH-GROUP
    `)
    expect(interp.stack[0]).toEqual("message")
})

it ('Can extract all regex matches', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "mr-android ios my-android web test-web" ".*?(android|ios|web|seo)" RE-MATCH-ALL
    `)
    expect(interp.stack[0]).toEqual(['android', 'ios', 'android', 'web', 'web'])
})

it ('Can encode a URL', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "now/is the time" URL-ENCODE
    `)
    expect(interp.stack[0]).toEqual("now%2Fis%20the%20time")
})

it ('Can decode a URL', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "now%2Fis%20the%20time" URL-DECODE
    `)
    expect(interp.stack[0]).toEqual("now/is the time")
})

it ('Can provide default values', async () => {
    let interp = new Interpreter()
    interp.stack_push(undefined)
    await interp.run(`
             1 DEFAULT
        NULL 2 DEFAULT
        0    3 DEFAULT
        "" "Howdy" DEFAULT
    `)
    expect(interp.stack).toEqual([1, 2, 0, "Howdy"])
})

it ('Can compute a default value', async () => {
    let interp = new Interpreter()
    interp.stack_push(undefined)
    await interp.run(`
               "2 1 +" *DEFAULT
        NULL   "3.1 5 +" *DEFAULT
        0      "22.4" *DEFAULT
        ""     "['Howdy, ' 'Everyone!'] CONCAT" *DEFAULT
    `)
    expect(interp.stack).toEqual([3, 8.1, 0, "Howdy, Everyone!"])
})

it ('Can repeat the execution of a Forthic string', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [0 "1 +" 6 <REPEAT]
    `)
    expect(interp.stack[0]).toEqual([0, 1, 2, 3, 4, 5, 6])
})

it ('Can round numbers to a precision', async () => {
    let interp = new Interpreter()
    await interp.run(`
        22 7 / 2 >FIXED
    `)
    expect(interp.stack[0]).toEqual("3.14")
})

it ('Can convert objects to JSON', async () => {
    let interp = new Interpreter()
    await interp.run(`
        [["a" 1] ["b" 2]] REC >JSON
    `)
    expect(interp.stack[0]).toEqual('{"a":1,"b":2}')
})

it ('Can convert nested strings to JSON', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "" >JSON
        '''He said "Ouch! He's crazy!"'''
    `)
    expect(interp.stack[0]).toEqual('""')
    expect(interp.stack[1]).toEqual('He said "Ouch! He\'s crazy!"')
})

it ('Can JSON to objects', async () => {
    let interp = new Interpreter()
    await interp.run(`
        '{"a": 1, "b": 2}' JSON>
    `)
    expect(interp.stack[0]).toEqual({a: 1, b: 2})
})

it ('Can JSON to strings', async () => {
    let interp = new Interpreter()
    await interp.run(`
        '''"He's crazy!"''' JSON>
    `)
    expect(interp.stack[0]).toEqual("He's crazy!")
})

it ('Can get the current time', async () => {
    let interp = new Interpreter()
    await interp.run(`
        NOW
    `)
    let date = interp.stack[0]
    let now = new Date()
    expect([date.getHours(), date.getMinutes()]).toEqual([now.getHours(), now.getMinutes()])
})

it ('Can convert strings to time', async () => {
    let interp = new Interpreter()
    await interp.run(`
        '10:52 PM' >TIME
    `)
    let time = interp.stack[0]
    expect([time.getHours(), time.getMinutes()]).toEqual([22, 52])
})

it ('Can convert time to string', async () => {
    let interp = new Interpreter()
    await interp.run(`
        '10:52 AM' >TIME TIME>STR
    `)
    expect(interp.stack[0]).toEqual("10:52")
})

it ('Can convert strings to dates', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "Oct 21, 2020" >DATE
    `)
    let date = interp.stack[0]
    expect([date.getMonth(), date.getDate(), date.getFullYear()]).toEqual([9, 21, 2020])  // 0-based months
})

it ('Can convert dates to strings', async () => {
    let interp = new Interpreter()
    await interp.run(`
        2020-11-02 DATE>STR
    `)
    expect(interp.stack[0]).toEqual("2020-11-02")
})

it ('Can get today\'s date', async () => {
    let interp = new Interpreter()
    await interp.run(`
        TODAY
    `)
    let date = interp.stack[0]
    let today = new Date()
    expect([date.getMonth(), date.getDate(), date.getFullYear()]).toEqual([today.getMonth(), today.getDate(), today.getFullYear()])
})

it ('Can add days to a date', async () => {
    let interp = new Interpreter()
    await interp.run(`
        2020-10-21 12 ADD-DAYS
    `)
    let date = interp.stack[0]
    expect([date.getMonth(), date.getDate(), date.getFullYear()]).toEqual([10, 2, 2020])
})

it ('Can subtract dates', async () => {
    let interp = new Interpreter()
    await interp.run(`
        2020-10-21 2020-11-02 SUBTRACT-DATES
    `)
    expect(interp.stack[0]).toEqual(-12)
})


it ('Can convert dates and times to datetimes', async () => {
    let interp = new Interpreter()
    await interp.run(`
        2020-11-02 10:25 PM DATE-TIME>DATETIME
    `)
    let datetime = interp.stack[0]
    expect([datetime.getFullYear(), datetime.getMonth(), datetime.getDate(), datetime.getHours(), datetime.getMinutes()]).toEqual([2020, 10, 2, 22, 25])
})

it ('Can convert datetimes to timestamps', async () => {
    let interp = new Interpreter()
    await interp.run(`
        2020-07-01 15:20 DATE-TIME>DATETIME DATETIME>TIMESTAMP
    `)
    expect(interp.stack[0]).toEqual(1593642000)
})

it ('Can convert timestamps to datetimes', async () => {
    let interp = new Interpreter()
    await interp.run(`
        1593895532 TIMESTAMP>DATETIME
    `)
    let date = interp.stack[0]
    expect([date.getFullYear(), date.getMonth(), date.getDate(), date.getHours(), date.getMinutes()]).toEqual([2020, 6, 4, 13, 45])
})

it ('Can do arithmetic', async () => {
    let interp = new Interpreter()
    await interp.run(`
        2 4 +
        2 4 -
        2 4 *
        2 4 /
        5 3 MOD
        2.51 ROUND
        [1 2 3] +
        [2 3 4] *
    `)
    expect(interp.stack).toEqual([6, -2, 8, 0.5, 2, 3, 6, 24])
})

it ('Can perform comparisons', async () => {
    let interp = new Interpreter()
    await interp.run(`
        2 4 ==
        2 4 !=
        2 4 <
        2 4 <=
        2 4 >
        2 4 >=
    `)
    expect(interp.stack).toEqual([false, true, true, true, false, false])
})

it ('Can perform logic operations', async () => {
    let interp = new Interpreter()
    await interp.run(`
        FALSE FALSE OR
        [FALSE FALSE TRUE FALSE] OR
        FALSE TRUE AND
        [FALSE FALSE TRUE FALSE] AND
        FALSE NOT
    `)
    expect(interp.stack).toEqual([false, true, false, false, true])
})


it ('Can check if an element is in an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "alpha" ["beta" "gamma"] IN
        "alpha" ["beta" "gamma" "alpha"] IN
    `)
    expect(interp.stack).toEqual([false, true])
})

it ('Can check if any of an array of elements is in an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ["alpha" "beta"] ["beta" "gamma"] ANY
        ["delta" "beta"] ["gamma" "alpha"] ANY
        ["alpha" "beta"] [] ANY
    `)
    expect(interp.stack).toEqual([true, false, true])
})

it ('Can check if all items of an array of elements are in an array', async () => {
    let interp = new Interpreter()
    await interp.run(`
        ["alpha" "beta"] ["beta" "gamma"] ALL
        ["delta" "beta"] ["beta"] ALL
        ["alpha" "beta"] [] ALL
    `)
    expect(interp.stack).toEqual([false, true, true])
})

it ('Can quote arbitrary text with DLE characters', async () => {
    let interp = new Interpreter()
    await interp.run(`
        "howdy" QUOTED
        "sinister${DLE}INJECT-BADNESS" QUOTED
    `)
    expect(interp.stack[0]).toEqual(`${DLE}howdy${DLE}`)
    expect(interp.stack[1]).toEqual(`${DLE}sinister INJECT-BADNESS${DLE}`)
})

it ('Can perform math conversions', async () => {
    let interp = new Interpreter()
    await interp.run(`
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
    `)
    expect(interp.stack).toEqual([false, false, true, false, true, 3, 4, 4, 1.2, 2.0])
})

it ('Can profile code', async () => {
    let interp = new Interpreter()
    await interp.run(`
        PROFILE-START
        [0 "1 +" 6 <REPEAT]
        PROFILE-END POP
        PROFILE-DATA
    `)
    let profile_data = interp.stack[0]
    let top_words = [profile_data["word_counts"][0]["word"], profile_data["word_counts"][1]["word"]].sort()
    expect(top_words).toEqual(["+", "1"])
    expect(profile_data["word_counts"][0]["count"]).toEqual(6)
    expect(profile_data["word_counts"][1]["count"]).toEqual(6)
    expect(profile_data["word_counts"][2]["count"]).toEqual(1)
})


// ----- Helpers ------------------------------------------------------------------------
function make_records() {
    let result = []
    let data = [
      [100, "user1", "OPEN"],
      [101, "user1", "OPEN"],
      [102, "user1", "IN PROGRESS"],
      [103, "user1", "CLOSED"],
      [104, "user2", "IN PROGRESS"],
      [105, "user2", "OPEN"],
      [106, "user2", "CLOSED"],
    ];
    for (const d of data) {
        let rec = {
            key: d[0],
            assignee: d[1],
            status: d[2]
        }
        result.push(rec)
    }
    return result
}

function make_records_by_key() {
    let result = {}
    let records = make_records()
    for (const rec of records) {
        result[rec["key"]] = rec
    }
    return result
}

function make_status_to_manager_to_ids() {
    let result = {
        open: {
            manager1: [101, 102],
            manager2: [103]
        },
        "blocked": {
            manager3: [104]
        },
        "closed": {
            manager1: [10, 11],
            manager2: [12, 13]
        }
    }
    return result
}
