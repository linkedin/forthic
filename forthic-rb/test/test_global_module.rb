# # frozen_string_literal: true

require "minitest/autorun"
require_relative "../lib/forthic/interpreter"

def make_records
  [
    { key: 100, assignee: "user1", status: "OPEN" },
    { key: 101, assignee: "user1", status: "OPEN" },
    { key: 102, assignee: "user1", status: "IN PROGRESS" },
    { key: 103, assignee: "user1", status: "CLOSED" },
    { key: 104, assignee: "user2", status: "IN PROGRESS" },
    { key: 105, assignee: "user2", status: "OPEN" },
    { key: 106, assignee: "user2", status: "CLOSED" },
  ]
end

def makeStatusToManagerToIds
  {
    open: {
      manager1: [101, 102],
      manager2: [103],
    },
    blocked: {
      manager3: [104],
    },
    closed: {
      manager1: [10, 11],
      manager2: [12, 13],
    },
  }
end


class TestGlobalModule < Minitest::Test
  def setup
    @interp = Forthic::Interpreter.new
  end

  def test_literal_values
    @interp.run("TRUE FALSE 2 3.14 2020-06-05")
    assert_equal true, @interp.stack[0]
    assert_equal false, @interp.stack[1]
    assert_equal 2, @interp.stack[2]
    assert_equal 3.14, @interp.stack[3]
    assert_equal Date.new(2020, 6, 5), @interp.stack[4]
  end

  def test_literal_time_values
    @interp.run("9:00")
    date = @interp.stack_pop
    assert_equal 9, date.hour
    assert_equal 0, date.min

    @interp.run("11:30 PM")
    date2 = @interp.stack_pop
    assert_equal 23, date2.hour
    assert_equal 30, date2.min

    @interp.run("22:15 AM")
    date3 = @interp.stack_pop
    assert_equal 10, date3.hour
    assert_equal 15, date3.min
  end

  def test_variables
    @interp.run("['x' 'y']  VARIABLES")
    variables = @interp.app_module.variables
    refute_nil variables["x"]
    refute_nil variables["y"]
  end

  def test_set_and_get_variables
    @interp.run("['x']  VARIABLES")
    @interp.run("24 x !")
    x_var = @interp.app_module.variables["x"]
    assert_equal 24, x_var.get_value
    @interp.run("x @")
    assert_equal 24, @interp.stack_pop
  end

  def test_bang_at
    @interp.run("['x']  VARIABLES")
    @interp.run("24 x !@")
    x_var = @interp.app_module.variables["x"]
    assert_equal 24, x_var.get_value
    assert_equal 24, @interp.stack_pop
  end

  def test_interpret
    @interp.run("'24' INTERPRET")
    assert_equal 24, @interp.stack_pop

    @interp.run("'{module-A  : MESSAGE   \"Hi\" ;}' INTERPRET")
    @interp.run("{module-A MESSAGE}")
    assert_equal "Hi", @interp.stack_pop
  end

  def test_module_id
    global_module = @interp.global_module
    assert_match(/<GLOBAL>-\d*/, global_module.module_id)
  end

  # def test_dates
  #   # This is just to exercise the date functions
  #   # @interp.run("TODAY")
  #   # puts "TODAY", @interp.stack_pop
  #   # @interp.run("SATURDAY")
  #   # puts @interp.stack_pop
  # end


  def test_rec
    @interp.run("[ ['alpha' 2] ['beta' 3] ['gamma' 4] ] REC")
    assert_equal 1, @interp.stack.length

    rec = @interp.stack_pop
    assert_equal 2, rec["alpha"]
    assert_equal 4, rec["gamma"]
  end

  def test_rec_at
    @interp.run("[ ['alpha' 2] ['beta' 3] ['gamma' 4] ] REC 'beta' REC@")
    assert_equal 1, @interp.stack.length
    assert_equal 3, @interp.stack_pop

    @interp.run("[10 20 30 40 50] 3 REC@")
    assert_equal 40, @interp.stack_pop
  end

  def test_nested_rec_at
    @interp.run(%(
      [ ["alpha" [["alpha1" 20]] REC]
        ["beta" [["beta1"  30]] REC]
      ] REC
      ["beta" "beta1"] REC@
    ))
    assert_equal 30, @interp.stack_pop

    @interp.run(%(
      [ [] [] [[3]] ]
      [2 0 0] REC@
    ))
    assert_equal 3, @interp.stack_pop

    @interp.run(%(
      [ ["alpha" [["alpha1" 20]] REC]
        ["beta" [["beta1"  [10 20 30]]] REC]
      ] REC
      ["beta" "beta1" 1] REC@
    ))
    assert_equal 20, @interp.stack_pop
  end

  def test_rec_bang
    # Case: Set value on a record
    @interp.run(%(
      [["alpha" 2] ["beta" 3] ["gamma" 4]] REC
      700 'beta' <REC! 'beta' REC@
    ))
    assert_equal 1, @interp.stack.length
    assert_equal 700, @interp.stack_pop

    # Case: Set a nested value
    @interp.run(%(
      [] REC "Green" ["2021-03-22" "TEST-1234"] <REC! ["2021-03-22" "TEST-1234"] REC@
    ))
    assert_equal 1, @interp.stack.length
    assert_equal "Green", @interp.stack_pop

    # Case: Set value on a NULL
    @interp.run(%(
      NULL 700 'beta' <REC! 'beta' REC@
    ))
    assert_equal 1, @interp.stack.length
    assert_equal 700, @interp.stack_pop
  end

  def test_append
    # Test append to array
    @interp.run("[ 1 2 3 ] 4 APPEND")
    assert_equal 1, @interp.stack.length

    array = @interp.stack_pop
    assert_equal [1, 2, 3, 4], array

    # Test append to record
    @interp.run('[["a" 1] ["b" 2]] REC  ["c" 3] APPEND')
    assert_equal 1, @interp.stack.length

    rec = @interp.stack_pop
    values = ["a", "b", "c"].map { |k| rec[k] }
    assert_equal [1, 2, 3], values
  end

  def test_reverse
    @interp.run("[ 1 2 3 ] REVERSE")
    assert_equal 1, @interp.stack.length

    assert_equal [3, 2, 1], @interp.stack_pop
  end

  def test_unique
    @interp.run("[ 1 2 3 3 2 ] UNIQUE")
    assert_equal [1, 2, 3], @interp.stack_pop
  end

  def test_delete
    @interp.run('[ "a" "b" "c" ] 1 <DEL')
    assert_equal ["a", "c"], @interp.stack_pop

    @interp.run('[["a" 1] ["b" 2] ["c" 3]] REC  "b" <DEL')
    rec = @interp.stack_pop
    assert_equal ["a", "c"], rec.keys.sort

    @interp.run('[["a" 1] ["b" 2] ["c" 3]] REC  "d" <DEL')
    rec = @interp.stack_pop
    assert_equal ["a", "b", "c"], rec.keys.sort
  end

  def test_relabel
    @interp.run('[ "a" "b" "c" ] [0 2] [25 23] RELABEL')
    assert_equal 1, @interp.stack.length
    array = @interp.stack_pop
    assert_equal ["c", "a"], array

    @interp.run('[["a" 1] ["b" 2] ["c" 3]] REC  ["a" "c"] ["alpha" "gamma"] RELABEL')
    assert_equal 1, @interp.stack.length
    rec = @interp.stack_pop
    assert_equal ["alpha", "gamma"], rec.keys.sort
    assert_equal [1, 3], ["alpha", "gamma"].map { |k| rec[k] }
  end

  def test_by_field
    @interp.stack_push(make_records)
    @interp.run("'key' >SYM BY-FIELD")
    grouped = @interp.stack_pop
    assert_equal "IN PROGRESS", grouped[104][:status]
  end

  def test_by_field_with_nulls
    @interp.stack_push(make_records + [nil, nil])
    @interp.run("'key' >SYM BY-FIELD")
    grouped = @interp.stack_pop
    assert_equal "IN PROGRESS", grouped[104][:status]
  end

  def test_group_by_field
    @interp.stack_push(make_records)
    @interp.run("'assignee' >SYM GROUP-BY-FIELD")
    grouped = @interp.stack_pop
    assert_equal ["user1", "user2"], grouped.keys.sort
    assert_equal 4, grouped["user1"].length
    assert_equal 3, grouped["user2"].length

    # Test grouping a record
    @interp = Forthic::Interpreter.new

    # First, set up the record
    records = make_records
    by_key = {}
    records.each do |rec|
      by_key[rec[:key]] = rec
    end
    @interp.stack_push(by_key)

    # Now group a record
    @interp.run("'assignee' >SYM GROUP-BY-FIELD")
    grouped_rec = @interp.stack_pop
    assert_equal ["user1", "user2"], grouped_rec.keys.sort
    assert_equal 4, grouped_rec["user1"].length
    assert_equal 3, grouped_rec["user2"].length
    assert_equal grouped, grouped_rec

    # Test grouping a list-valued field
    @interp.stack_push([
      { "id" => 1, "attrs" => ["blue", "important"] },
      { "id" => 2, "attrs" => ["red"] },
    ])
    @interp.run("'attrs' GROUP-BY-FIELD")
    grouped_rec = @interp.stack_pop
    assert_equal 1, grouped_rec["blue"][0]["id"]
    assert_equal 1, grouped_rec["important"][0]["id"]
    assert_equal 2, grouped_rec["red"][0]["id"]
  end

  def test_group_by
    @interp.stack_push(make_records)
    @interp.run("\"'assignee' >SYM REC@\" GROUP-BY")
    grouped = @interp.stack_pop
    assert_equal ["user1", "user2"], grouped.keys.sort
    assert_equal 4, grouped["user1"].length
    assert_equal 3, grouped["user2"].length

    # Test grouping a record
    @interp = Forthic::Interpreter.new

    # First, set up the record
    records = make_records
    by_key = {}
    records.each do |rec|
      by_key[rec[:key]] = rec
    end
    @interp.stack_push(by_key)

    # Now group a record
    @interp.run("\"'assignee' >SYM REC@\" GROUP-BY")
    grouped_rec = @interp.stack_pop
    assert_equal ["user1", "user2"], grouped_rec.keys.sort
    assert_equal 4, grouped_rec["user1"].length
    assert_equal 3, grouped_rec["user2"].length
    assert_equal grouped, grouped_rec
  end

  def test_group_by_with_key
    @interp.stack_push(make_records)
    @interp.run(%(
      ['key' 'val'] VARIABLES
      "val ! key ! key @ 3 MOD" !WITH-KEY GROUP-BY
    ))
    grouped = @interp.stack_pop
    assert_equal [0, 1, 2], grouped.keys.sort
    assert_equal 3, grouped[0].length
    assert_equal 2, grouped[1].length
    assert_equal 2, grouped[2].length

    # Test grouping a record
    @interp = Forthic::Interpreter.new

    # First, set up the record
    records = make_records
    by_key = {}
    records.each do |rec|
      by_key[rec[:key]] = rec
    end
    @interp.stack_push(by_key)

    # Now group a record
    @interp.run(%(
      ['key' 'val'] VARIABLES
      "val ! key ! key @ 2 *" !WITH-KEY GROUP-BY
    ))
    grouped_rec = @interp.stack_pop
    assert_equal [200, 202, 204, 206, 208, 210, 212], grouped_rec.keys.sort
  end

  def test_groups_of_record
    @interp.run(%(
      [
        ['a' 1]
        ['b' 2]
        ['c' 3]
        ['d' 4]
        ['e' 5]
        ['f' 6]
        ['g' 7]
        ['h' 8]
      ] REC 3 GROUPS-OF
    ))
    groups = @interp.stack_pop
    assert_equal({ 'a' => 1, 'b' => 2, 'c' => 3 }, groups[0])
    assert_equal({ 'd' => 4, 'e' => 5, 'f' => 6 }, groups[1])
    assert_equal({ 'g' => 7, 'h' => 8 }, groups[2])
  end

  def test_groups_of_using_record
    # Test grouping a record
    @interp = Forthic::Interpreter.new

    # First, set up the record
    records = make_records
    by_key = {}
    records.each do |rec|
      by_key[rec[:key]] = rec
    end
    @interp.stack_push(by_key)

    # Now group a record
    @interp.run("3 GROUPS-OF")
    recs = @interp.stack_pop
    assert_equal 3, recs[0].keys.length
    assert_equal 3, recs[1].keys.length
    assert_equal 1, recs[2].keys.length
  end

  def test_index
    @interp.run(%(
      : |KEYS   "'key' REC@" MAP;
      : TICKETS [
        [['key'   101] ['Labels'  ['alpha' 'beta']]] REC
        [['key'   102] ['Labels'  ['alpha' 'gamma']]] REC
        [['key'   103] ['Labels'  ['alpha']]] REC
        [['key'   104] ['Labels'  ['beta']]] REC
      ];

      TICKETS "'Labels' REC@" INDEX  "|KEYS" MAP
    ))
    index_record = @interp.stack_pop
    assert_equal [101, 102, 103], index_record["alpha"]
    assert_equal [101, 104], index_record["beta"]
    assert_equal [102], index_record["gamma"]
  end

  def test_map
    @interp.run(%(
      [1 2 3 4 5] '2 *' MAP
    ))
    array = @interp.stack_pop
    assert_equal [2, 4, 6, 8, 10], array

    # First, set up the record
    records = make_records
    by_key = {}
    records.each do |rec|
      by_key[rec[:key]] = rec
    end
    @interp.stack_push(by_key)

    @interp.run(%(
      "'status' >SYM REC@" MAP
    ))
    record = @interp.stack_pop
    assert_equal "OPEN", record[100]
    assert_equal "IN PROGRESS", record[102]
    assert_equal "CLOSED", record[106]

    # Test map in module
    @interp.run(%(
      {my-module
        : DOUBLE   2 *;
        : RUN   [1 2 3 4 5] "DOUBLE" MAP;
      }
      {my-module RUN}
    ))
    array = @interp.stack_pop
    assert_equal [2, 4, 6, 8, 10], array
  end

  def test_map_depth
    @interp.run(%(
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
    ))
    record = @interp.stack_pop
    expected = {
      "k1" => { "l1" => { "m" => 4 }, "l2" => { "m" => 6 } },
      "k2" => { "l1" => { "m" => 6 }, "l2" => { "m" => 8 } }
    }
    assert_equal expected, record
  end

  def test_map_depth_over_array
    @interp.run(%(
      : DEEP-LIST [ [ [[["m"  2]] REC [["m"  3]] REC] ] [ [[["m"  3]] REC [["m"  4]] REC] ] ];

      DEEP-LIST "2 *" 3 !DEPTH MAP
    ))
    array = @interp.stack_pop
    expected = [
      [[{ "m" => 4 }, { "m" => 6 }]],
      [[{ "m" => 6 }, { "m" => 8 }]]
    ]
    assert_equal expected, array
  end

  def test_map_depth_over_array_of_maps
    @interp.run(%(
      : DEEP-LIST [ [ [2 3] ] [ [3 4] ] ];

      DEEP-LIST "2 *" 2 !DEPTH MAP
    ))
    array = @interp.stack_pop
    expected = [[[4, 6]], [[6, 8]]]
    assert_equal expected, array
  end

  def test_map_depth_with_error
    @interp.run(%(
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
    ))
    errors = @interp.stack_pop
    record = @interp.stack_pop

    expected_record = {
      "k1" => { "l1" => { "m" => 2 }, "l2" => { "m" => 3 } },
      "k2" => { "l1" => { "m" => nil }, "l2" => { "m" => 4 } }
    }
    assert_equal expected_record, record
    assert_nil errors[0]
    assert_nil errors[1]
    refute_nil errors[2]
    assert_nil errors[3]
  end

  def test_map_with_key
    @interp.run(%(
      [1 2 3 4 5] '+ 2 *' !WITH-KEY MAP
    ))
    array = @interp.stack_pop
    assert_equal [2, 6, 10, 14, 18], array

    # First, set up the record
    records = make_records
    by_key = {}
    records.each do |rec|
      by_key[rec[:key]] = rec
    end
    @interp.stack_push(by_key)

    @interp.run(%(
      ["k" "v"] VARIABLES
      "v ! k !  k @ v @ 'status' >SYM REC@ CONCAT" !WITH-KEY MAP
    ))
    record = @interp.stack_pop
    assert_equal "100OPEN", record[100]
    assert_equal "102IN PROGRESS", record[102]
    assert_equal "106CLOSED", record[106]
  end

  def test_foreach
    @interp.run(%(
      0 [1 2 3 4 5] '+' FOREACH
    ))
    sum = @interp.stack_pop
    assert_equal 15, sum

    # First, set up the record
    records = make_records
    by_key = {}
    records.each do |rec|
      by_key[rec[:key]] = rec
    end
    @interp.stack_push(by_key)

    @interp.run(%(
      "" SWAP "'status' >SYM REC@ CONCAT" FOREACH
    ))
    string = @interp.stack_pop
    assert_equal "OPENOPENIN PROGRESSCLOSEDIN PROGRESSOPENCLOSED", string
  end

  def test_foreach_with_key
    @interp.run(%(
      0 [1 2 3 4 5] '+ +' !WITH-KEY FOREACH
    ))
    sum = @interp.stack_pop
    assert_equal 25, sum

    # First, set up the record
    records = make_records
    by_key = {}
    records.each do |rec|
      by_key[rec[:key]] = rec
    end
    @interp.stack_push(by_key)

    @interp.run(%(
      "" SWAP "'status' >SYM REC@ CONCAT CONCAT" !WITH-KEY FOREACH
    ))
    string = @interp.stack_pop
    assert_equal "100OPEN101OPEN102IN PROGRESS103CLOSED104IN PROGRESS105OPEN106CLOSED", string
  end

  def test_foreach_to_errors
    @interp.run(%(
      ['2' '3' 'GARBAGE' '+'] 'INTERPRET' !PUSH-ERROR FOREACH
    ))
    errors = @interp.stack_pop
    assert_nil errors[0]
    assert_nil errors[1]
    refute_nil errors[2]
    assert_nil errors[3]
    sum = @interp.stack_pop
    assert_equal 5, sum
  end

  def test_invert_keys
    status_to_manager_to_ids = makeStatusToManagerToIds()
    @interp.stack_push(status_to_manager_to_ids)
    @interp.run("INVERT-KEYS")
    res = @interp.stack_pop
    expected = {
      :manager1 => {
        :open => [101, 102],
        :closed => [10, 11],
      },
      :manager2 => {
        :open => [103],
        :closed => [12, 13],
      },
      :manager3 => {
        :blocked => [104],
      },
    }
    assert_equal expected, res
  end

  def test_zip
    @interp.run(%(
      ['a' 'b'] [1 2] ZIP
    ))
    array = @interp.stack_pop
    assert_equal ["a", 1], array[0]
    assert_equal ["b", 2], array[1]

    # First, set up the record
    @interp.run(%(
      [['a' 100] ['b' 200] ['z' 300]] REC [['a' 'Hi'] ['b' 'Bye'] ['c' '?']] REC ZIP
    ))
    record = @interp.stack_pop
    assert_equal ["a", "b", "z"], record.keys.sort
    assert_equal [100, "Hi"], record["a"]
    assert_equal [200, "Bye"], record["b"]
    assert_equal [300, nil], record["z"]
  end

  def test_zip_with
    @interp.run(%(
      [10 20] [1 2] "+" ZIP-WITH
    ))
    array = @interp.stack_pop
    assert_equal 11, array[0]
    assert_equal 22, array[1]

    # First, set up the record
    @interp.run(%(
      [['a' 1] ['b' 2]] REC [['a' 10] ['b' 20]] REC "+" ZIP-WITH
    ))
    record = @interp.stack_pop
    assert_equal ["a", "b"], record.keys.sort
    assert_equal 11, record["a"]
    assert_equal 22, record["b"]
  end

  def test_keys
    @interp.run(%(
      ['a' 'b' 'c'] KEYS
    ))
    array = @interp.stack_pop
    assert_equal [0, 1, 2], array

    # First, set up the record
    @interp.run(%(
      [['a' 1] ['b' 2]] REC KEYS
    ))
    array = @interp.stack_pop
    assert_equal ["a", "b"], array.sort
  end

  def test_values
    @interp.run(%(
      ['a' 'b' 'c'] VALUES
    ))
    array = @interp.stack_pop
    assert_equal ["a", "b", "c"], array

    # First, set up the record
    @interp.run(%(
      [['a' 1] ['b' 2]] REC VALUES
    ))
    array = @interp.stack_pop
    assert_equal [1, 2], array.sort
  end

  def test_length
    @interp.run(%(
      ['a' 'b' 'c'] LENGTH
      "Howdy" LENGTH
    ))
    assert_equal 5, @interp.stack_pop
    assert_equal 3, @interp.stack_pop

    # Test record
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      [['a' 1] ['b' 2]] REC LENGTH
    ))
    length = @interp.stack_pop
    assert_equal 2, length
  end

  def test_range
    @interp.run(%(
      : EVEN?   2 MOD  0 ==;
      : ODD?    2 MOD  1 ==;
      [1 2 3 4 5] "EVEN?" "ODD?" RANGE
    ))
    array = @interp.stack_pop
    assert_equal [1, 2], array
  end

  def test_slice
    @interp.run(%(
      ['x'] VARIABLES
      ['a' 'b' 'c' 'd' 'e' 'f' 'g'] x !
      x @ 0 2 SLICE
      x @ 1 3 SLICE
      x @ 5 3 SLICE
      x @ -1 -2 SLICE
      x @ 4 -2 SLICE
      x @ 5 8 SLICE
    ))
    stack = @interp.stack
    assert_equal ["a", "b", "c"], stack[0]
    assert_equal ["b", "c", "d"], stack[1]
    assert_equal ["f", "e", "d"], stack[2]
    assert_equal ["g", "f"], stack[3]
    assert_equal ["e", "f"], stack[4]
    assert_equal ["f", "g", nil, nil], stack[5]

    # Slice records
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      ['x'] VARIABLES
      [['a' 1] ['b' 2] ['c' 3]] REC x !
      x @ 0 1 SLICE
      x @ -1 -2 SLICE
      x @ 5 7 SLICE
    ))
    stack2 = @interp.stack
    assert_equal ["a", "b"], stack2[0].keys.sort
    assert_equal ["b", "c"], stack2[1].keys.sort
    assert_equal({}, stack2[2])
  end

  def test_difference
    @interp.run(%(
      ['x' 'y'] VARIABLES
      ['a' 'b' 'c'] x !
      ['a' 'c' 'd'] y !
      x @ y @ DIFFERENCE
      y @ x @ DIFFERENCE
    ))
    stack = @interp.stack
    assert_equal ["b"], stack[0]
    assert_equal ["d"], stack[1]

    # Records
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      ['x' 'y'] VARIABLES
      [['a' 1] ['b' 2] ['c' 3]] REC x !
      [['a' 20] ['c' 40] ['d' 10]] REC y !
      x @ y @ DIFFERENCE
      y @ x @ DIFFERENCE
    ))
    stack2 = @interp.stack
    assert_equal ["b"], stack2[0].keys
    assert_equal [2], stack2[0].values
    assert_equal ["d"], stack2[1].keys
    assert_equal [10], stack2[1].values
  end

  def test_intersection
    @interp.run(%(
      ['x' 'y'] VARIABLES
      ['a' 'b' 'c'] x !
      ['a' 'c' 'd'] y !
      x @ y @ INTERSECTION
    ))
    stack = @interp.stack
    assert_equal ["a", "c"], stack[0].sort

    # Records
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      ['x' 'y'] VARIABLES
      [['a' 1] ['b' 2] ['f' 3]] REC x !
      [['a' 20] ['c' 40] ['d' 10]] REC y !
      x @ y @ INTERSECTION
    ))
    stack = @interp.stack
    assert_equal ["a"], stack[0].keys
    assert_equal [1], stack[0].values
  end

  def test_union
    @interp.run(%(
      ['x' 'y'] VARIABLES
      ['a' 'b' 'c'] x !
      ['a' 'c' 'd'] y !
      x @ y @ UNION
    ))
    stack = @interp.stack
    assert_equal ["a", "b", "c", "d"], stack[0].sort

    # Records
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      ['x' 'y'] VARIABLES
      [['a' 1] ['b' 2] ['f' 3]] REC x !
      [['a' 20] ['c' 40] ['d' 10]] REC y !
      x @ y @ UNION
    ))
    stack = @interp.stack
    assert_equal ["a", "b", "c", "d", "f"], stack[0].keys.sort
    assert_equal [1, 2, 3, 10, 40], stack[0].values.sort
  end

  def test_select
    @interp.run(%(
      [0 1 2 3 4 5 6] "2 MOD 1 ==" SELECT
    ))
    stack = @interp.stack
    assert_equal [1, 3, 5], stack[0]

    # Slice records
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      [['a' 1] ['b' 2] ['c' 3]] REC "2 MOD 0 ==" SELECT
    ))
    stack = @interp.stack
    assert_equal ["b"], stack[0].keys
    assert_equal [2], stack[0].values
  end

  def test_select_with_key
    @interp.run(%(
      [0 1 2 3 4 5 6] "+ 3 MOD 1 ==" !WITH-KEY SELECT
    ))
    stack = @interp.stack
    assert_equal [2, 5], stack[0]

    # Slice records
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      [['a' 1] ['b' 2] ['c' 3]] REC "CONCAT 'c3' ==" !WITH-KEY SELECT
    ))
    stack = @interp.stack
    assert_equal ["c"], stack[0].keys
    assert_equal [3], stack[0].values
  end


  def test_take
    @interp.run(%(
      [0 1 2 3 4 5 6] 3 TAKE
    ))
    stack = @interp.stack
    assert_equal [0, 1, 2], stack[0]

    # Take records
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      [['a' 1] ['b' 2] ['c' 3]] REC 2 TAKE
    ))
    stack = @interp.stack
    assert_equal 2, stack[0].length
  end

  def test_take_with_rest
    @interp.run(%(
      [0 1 2 3 4 5 6] 3 !PUSH-REST TAKE
    ))
    stack = @interp.stack
    assert_equal [0, 1, 2], stack[0]
    assert_equal [3, 4, 5, 6], stack[1]

    # Take records
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      [['a' 1] ['b' 2] ['c' 3]] REC 2 !PUSH-REST TAKE
    ))
    stack = @interp.stack
    assert_equal 2, stack[0].length
    assert_equal 1, stack[1].length
  end

  def test_drop
    @interp.run(%(
      [0 1 2 3 4 5 6] 4 DROP
    ))
    stack = @interp.stack
    assert_equal [4, 5, 6], stack[0]

    # Drop records
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      [['a' 1] ['b' 2] ['c' 3]] REC 2 DROP
    ))
    stack = @interp.stack
    assert_equal 1, stack[0].length
  end

  def test_rotate
    @interp.run(%(
      ['a' 'b' 'c' 'd'] ROTATE
      ['b'] ROTATE
      [] ROTATE
    ))
    stack = @interp.stack
    assert_equal ["d", "a", "b", "c"], stack[0]
    assert_equal ["b"], stack[1]
    assert_equal [], stack[2]
  end

  def test_array?
    @interp.run(%(
      ['a' 'b' 'c' 'd'] ARRAY?
      'b' ARRAY?
      0 ARRAY?
    ))
    stack = @interp.stack
    assert_equal true, stack[0]
    assert_equal false, stack[1]
    assert_equal false, stack[2]
  end

  def test_shuffle
    @interp.run(%(
      [0 1 2 3 4 5 6] SHUFFLE
    ))
    stack = @interp.stack
    assert_equal 7, stack[0].length
  end

  def test_sort
    @interp.run(%(
      [2 8 1 4 7 3] SORT
    ))
    stack = @interp.stack
    assert_equal [1, 2, 3, 4, 7, 8], stack[0]
  end

  def test_sort_with_null
    @interp.run(%(
      [2 8 1 NULL 4 7 NULL 3] SORT
    ))
    stack = @interp.stack
    assert_equal [1, 2, 3, 4, 7, 8, nil, nil], stack[0]
  end

  def test_sort_with_forthic
    @interp.run(%(
      [2 8 1 4 7 3] "-1 *" !COMPARATOR SORT
    ))
    stack = @interp.stack
    assert_equal [8, 7, 4, 3, 2, 1], stack[0]
  end

  def test_sort_with_key_func
    @interp.stack_push(make_records)
    @interp.run(%(
      'status' >SYM FIELD-KEY-FUNC !COMPARATOR SORT
    ))
    stack = @interp.stack
    assert_equal "CLOSED", stack[0][0][:status]
    assert_equal "CLOSED", stack[0][1][:status]
    assert_equal "IN PROGRESS", stack[0][2][:status]
    assert_equal "IN PROGRESS", stack[0][3][:status]
    assert_equal "OPEN", stack[0][4][:status]
    assert_equal "OPEN", stack[0][5][:status]
    assert_equal "OPEN", stack[0][6][:status]
  end

  def test_nth
    @interp.run(%(
      ["x"] VARIABLES
      [0 1 2 3 4 5 6] x !
      x @ 0 NTH
      x @ 5 NTH
      x @ 55 NTH
    ))
    stack = @interp.stack
    assert_equal 0, stack[0]
    assert_equal 5, stack[1]
    assert_nil stack[2]
  end

  def test_last
    @interp.run(%(
      [0 1 2 3 4 5 6] LAST
    ))
    stack = @interp.stack
    assert_equal 6, stack[0]
  end

  def test_unpack
    @interp.run(%(
      [0 1 2] UNPACK
    ))
    stack = @interp.stack
    assert_equal 0, stack[0]
    assert_equal 1, stack[1]
    assert_equal 2, stack[2]

    # For record
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      [['a' 1] ['b' 2] ['c' 3]] REC UNPACK
    ))
    stack = @interp.stack
    assert_equal 1, stack[0]
    assert_equal 2, stack[1]
    assert_equal 3, stack[2]
  end

  def test_flatten
    @interp.run(%(
      [0 [1 2 [3 [4]] ]] FLATTEN
    ))
    stack = @interp.stack
    assert_equal [0, 1, 2, 3, 4], stack[0]

    # For record
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      ['uno' 'alpha'] VARIABLES
      [['uno' 4] ['duo' 8]] REC uno !
      [['alpha' uno @]] REC alpha !
      [['a' 1] ['b' alpha @] ['c' 3]] REC FLATTEN
    ))
    stack = @interp.stack
    record = stack[0]
    assert_equal ["a", "b\talpha\tduo", "b\talpha\tuno", "c"], record.keys.sort
  end

  def test_flatten_depth
    @interp.run(%(
      [ [ [0 1] [2 3] ]
        [ [4 5]       ] ] 1 !DEPTH FLATTEN
    ))
    array = @interp.stack.last
    assert_equal [[0, 1], [2, 3], [4, 5]], array

    @interp.run(%(
      [ [ [0 1] [2 3] ]
        [ [4 5]       ] ] 0 !DEPTH FLATTEN
    ))
    array = @interp.stack.last
    assert_equal [[[0, 1], [2, 3]], [[4, 5]]], array

    @interp.run(%(
      [ [ [0 1] [2 3] ]
        [ [4 5]       ] ] 2 !DEPTH FLATTEN
    ))
    array = @interp.stack.last
    assert_equal [0, 1, 2, 3, 4, 5], array
  end

  def test_flatten_one_level_record
    @interp.run(%(
      ['uno' 'alpha'] VARIABLES
      [['uno' 4] ['duo' 8]] REC uno !
      [['alpha' uno @]] REC alpha !
      [['a' 1] ['b' alpha @] ['c' 3]] REC 1 !DEPTH FLATTEN
    ))
    record = @interp.stack.last
    assert_equal ["a", "b\talpha", "c"], record.keys.sort
  end

  def test_key_of
    @interp.run(%(
      ['x'] VARIABLES
      ['a' 'b' 'c' 'd'] x !
      x @ 'c' KEY-OF
      x @ 'z' KEY-OF
    ))
    stack = @interp.stack
    assert_equal 2, stack[0]
    assert_nil stack[1]

    # For record
    @interp = Forthic::Interpreter.new
    @interp.run(%(
      [['a' 1] ['b' 2] ['c' 3]] REC 2 KEY-OF
    ))
    stack = @interp.stack
    assert_equal "b", stack[0]
  end

  def test_reduce
    @interp.run(%(
      [1 2 3 4 5] 10 "+" REDUCE
    ))
    assert_equal 25, @interp.stack_pop

    # For record
    @interp.run(%(
      [['a' 1] ['b' 2] ['c' 3]] REC 20 "+" REDUCE
    ))
    assert_equal 26, @interp.stack_pop
  end

  def test_pop
    @interp.run(%(
      1 2 3 4 5 POP
    ))
    stack = @interp.stack
    assert_equal 4, stack.length
    assert_equal 4, stack.last
  end

  def test_dup
    @interp.run(%(
      5 DUP
    ))
    stack = @interp.stack
    assert_equal 2, stack.length
    assert_equal 5, stack[0]
    assert_equal 5, stack[1]
  end

  def test_swap
    @interp.run(%(
      6 8 SWAP
    ))
    stack = @interp.stack
    assert_equal 2, stack.length
    assert_equal 8, stack[0]
    assert_equal 6, stack[1]
  end

  def test_split
    @interp.run(%(
      'Now is the time' ' ' SPLIT
    ))
    stack = @interp.stack
    assert_equal 1, stack.length
    assert_equal ["Now", "is", "the", "time"], stack[0]
  end

  def test_join
    @interp.run(%(
      ["Now" "is" "the" "time"] "--" JOIN
    ))
    stack = @interp.stack
    assert_equal 1, stack.length
    assert_equal "Now--is--the--time", stack[0]
  end

  def test_special_chars
    @interp.run(%(
      /R /N /T
    ))
    stack = @interp.stack
    assert_equal "\r", stack[0]
    assert_equal "\n", stack[1]
    assert_equal "\t", stack[2]
  end

  def test_lowercase
    @interp.run(%(
      "HOWDY, Everyone!" LOWERCASE
    ))
    stack = @interp.stack
    assert_equal "howdy, everyone!", stack[0]
  end

  def test_ascii
    @interp.run(%(
      "“HOWDY, Everyone!”" ASCII
    ))
    stack = @interp.stack
    assert_equal "HOWDY, Everyone!", stack[0]
  end

  def test_strip
    @interp.run(%(
      "  howdy  " STRIP
    ))
    stack = @interp.stack
    assert_equal "howdy", stack[0]
  end

  def test_replace
    @interp.run(%(
      "1-40 2-20" "-" "." REPLACE
    ))
    stack = @interp.stack
    assert_equal "1.40 2.20", stack[0]
  end

  def test_re_match
    @interp.run(%(
      "123message456" "\\d{3}.*\\d{3}" RE-MATCH
    ))
    stack = @interp.stack
    refute_nil stack[0]
  end

  def test_re_match_group
    @interp.run(%(
      "123message456" "\\d{3}(.*)\\d{3}" RE-MATCH 1 RE-MATCH-GROUP
    ))
    stack = @interp.stack
    assert_equal "message", stack[0]
  end

  def test_re_match_all
    @interp.run(%(
      "mr-android ios my-android web test-web" ".*?(android|ios|web|seo)" RE-MATCH-ALL
    ))
    stack = @interp.stack
    assert_equal ["android", "ios", "android", "web", "web"], stack[0]
  end

  def test_default
    @interp.run(%(
      NULL 22.4 DEFAULT
      0 22.4 DEFAULT
      "" "Howdy" DEFAULT
    ))
    stack = @interp.stack
    assert_equal 22.4, stack[0]
    assert_equal 0, stack[1]
    assert_equal "Howdy", stack[2]
  end

  def test_star_default
    @interp.run(%(
      NULL "3.1 5 +" *DEFAULT
      0 "22.4" *DEFAULT
      "" "['Howdy, ' 'Everyone!'] CONCAT" *DEFAULT
    ))
    stack = @interp.stack
    assert_in_delta 8.1, stack[0], 0.1
    assert_equal 0, stack[1]
    assert_equal "Howdy, Everyone!", stack[2]
  end

  def test_repeat
    @interp.run(%(
      [0 "1 +" 6 <REPEAT]
    ))
    stack = @interp.stack
    assert_equal [0, 1, 2, 3, 4, 5, 6], stack[0]
  end

  def test_to_fixed
    @interp.run(%(
      22.0 7 / 2 >FIXED
    ))
    stack = @interp.stack
    assert_equal "3.14", stack[0]
  end

  def test_to_json
    @interp.run(%(
      [["a" 1] ["b" 2]] REC >JSON
    ))
    result = @interp.stack_pop
    assert_equal '{"a":1,"b":2}', result
  end

  def test_json_to
    @interp.run(%(
      '{"a": 1, "b": 2}' JSON>
    ))
    stack = @interp.stack
    assert_equal ["a", "b"], stack[0].keys.sort
    assert_equal 1, stack[0]["a"]
    assert_equal 2, stack[0]["b"]
  end

  def test_now
    now = Time.now
    @interp.run("NOW")
    result = @interp.stack_pop
    assert_equal now.hour, result.hour
    assert_equal now.min, result.min
  end

  def test_to_time
    @interp.run("'10:52 PM' >TIME")
    result = @interp.stack_pop
    assert_equal 22, result.hour
    assert_equal 52, result.min
  end

  def test_to_date
    @interp.run('"Oct 21, 2020" >DATE')
    date = @interp.stack_pop
    assert_equal 2020, date.year
    assert_equal 10, date.month
    assert_equal 21, date.day
  end

  def test_today
    @interp.run("TODAY")
    result = @interp.stack_pop
    today = Date.today
    assert_equal today.year, result.year
    assert_equal today.month, result.month
    assert_equal today.day, result.day
  end

  def test_days_of_week
    @interp.run("MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY")
    stack = @interp.stack
    today = Date.today
    assert stack[0] <= today
    assert stack[6] >= today
  end

  def test_add_days
    @interp.run('2020-10-21 12 ADD-DAYS')
    date = @interp.stack_pop
    assert_equal 2020, date.year
    assert_equal 11, date.month # Months are 1-based in Ruby
    assert_equal 2, date.day
  end

  def test_subtract_dates
    @interp.run('2020-10-21 2020-11-02 SUBTRACT-DATES')
    stack = @interp.stack
    assert_equal (-12), stack[0]
  end

  def test_date_to_str
    @interp.run(%(
      2020-11-02 DATE>STR
    ))
    stack = @interp.stack
    assert_equal "2020-11-02", stack[0]
  end

  def test_time_to_str
    @interp.run(%(
      10:25 PM TIME>STR
    ))
    stack = @interp.stack
    assert_equal "22:25", stack[0]
  end

  def test_date_time_to_datetime
    @interp.run(%(
      2020-11-02 10:25 PM DATE-TIME>DATETIME
      2020-11-02 10:25 PM DATE-TIME>DATETIME >DATE
      2020-11-02 10:25 PM DATE-TIME>DATETIME >TIME
    ))
    stack = @interp.stack
    p stack
    datetime = stack[0]
    assert_equal 2020, datetime.year
    assert_equal 11, datetime.month
    assert_equal 2, datetime.day
    assert_equal 22, datetime.hour
    assert_equal 25, datetime.min

    date = stack[1]
    assert_equal 2020, date.year
    assert_equal 11, date.month
    assert_equal 2, date.day

    time = stack[2]
    assert_equal 22, time.hour
    assert_equal 25, time.min
  end

  def test_datetime_to_timestamp
    @interp.run(%(
      2020-07-01 15:20 DATE-TIME>DATETIME DATETIME>TIMESTAMP
    ))
    stack = @interp.stack
    assert_equal 1593642000, stack[0]
  end

  def test_timestamp_to_datetime
    @interp.run(%(
      1593642000 TIMESTAMP>DATETIME
    ))
    datetime = @interp.stack_pop
    assert_equal 2020, datetime.year
    assert_equal 7, datetime.month
    assert_equal 1, datetime.day
    assert_equal 15, datetime.hour
    assert_equal 20, datetime.min
  end

  def test_arithmetic
    @interp.run(%(
      2 4 +
      2 4 -
      2 4 *
      2.0 4 /
      5 3 MOD
      2.51 ROUND
      [1 2 3] +
      [2 3 4] *
    ))
    stack = @interp.stack
    assert_equal 6, stack[0]
    assert_equal (-2), stack[1]
    assert_equal 8, stack[2]
    assert_equal 0.5, stack[3]
    assert_equal 2, stack[4]
    assert_equal 3, stack[5]
    assert_equal 6, stack[6]
    assert_equal 24, stack[7]
  end

  def test_mean
    @interp.run("[1 2 3 4 5] MEAN")
    stack = @interp.stack
    assert_equal 3, stack.last

    @interp.run("[4] MEAN")
    stack = @interp.stack
    assert_equal 4, stack.last

    @interp.run("[] MEAN")
    stack = @interp.stack
    assert_equal 0, stack.last

    @interp.run("NULL MEAN")
    stack = @interp.stack
    assert_equal 0, stack.last
  end

  def test_comparison
    @interp.run(%(
      2 4 ==
      2 4 !=
      2 4 <
      2 4 <=
      2 4 >
      2 4 >=
    ))
    stack = @interp.stack
    assert_equal false, stack[0]
    assert_equal true, stack[1]
    assert_equal true, stack[2]
    assert_equal true, stack[3]
    assert_equal false, stack[4]
    assert_equal false, stack[5]
  end

  def test_logic
    @interp.run(%(
      FALSE FALSE OR
      [FALSE FALSE TRUE FALSE] OR
      FALSE TRUE AND
      [FALSE FALSE TRUE FALSE] AND
      FALSE NOT
    ))
    stack = @interp.stack
    assert_equal false, stack[0]
    assert_equal true, stack[1]
    assert_equal false, stack[2]
    assert_equal false, stack[3]
    assert_equal true, stack[4]
  end

  def test_in
    @interp.run(%(
      "alpha" ["beta" "gamma"] IN
      "alpha" ["beta" "gamma" "alpha"] IN
    ))
    stack = @interp.stack
    assert_equal false, stack[0]
    assert_equal true, stack[1]
  end

  def test_any
    @interp.run(%(
      ["alpha" "beta"] ["beta" "gamma"] ANY
      ["delta" "beta"] ["gamma" "alpha"] ANY
      ["alpha" "beta"] [] ANY
    ))
    stack = @interp.stack
    assert_equal true, stack[0]
    assert_equal false, stack[1]
    assert_equal true, stack[2]
  end

  def test_all
    @interp.run(%(
      ["alpha" "beta"] ["beta" "gamma"] ALL
      ["delta" "beta"] ["beta"] ALL
      ["alpha" "beta"] [] ALL
    ))
    stack = @interp.stack
    assert_equal false, stack[0]
    assert_equal true, stack[1]
    assert_equal true, stack[2]
  end

  def test_range_index
    @interp.run(%(
      0 [0 1 2] RANGE-INDEX
      1 [0 1 2] RANGE-INDEX
      2 [0 1 2] RANGE-INDEX
      3 [0 1 2] RANGE-INDEX
      100 [0 1 2] RANGE-INDEX
      -1 [0 1 2] RANGE-INDEX
    ))
    stack = @interp.stack
    assert_equal 0, stack[0]
    assert_equal 1, stack[1]
    assert_equal 2, stack[2]
    assert_equal 2, stack[3]
    assert_equal 2, stack[4]
    assert_nil stack[5]
  end

  def test_math_converters
    @interp.run(%(
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
    ))
    stack = @interp.stack
    assert_equal false, stack[0]
    assert_equal false, stack[1]
    assert_equal true, stack[2]
    assert_equal false, stack[3]
    assert_equal true, stack[4]
    assert_equal 3, stack[5]
    assert_equal 4, stack[6]
    assert_equal 4, stack[7]
    assert_equal 1.2, stack[8]
    assert_equal 2.0, stack[9]
  end

  def test_profiling
    @interp.run(%(
      PROFILE-START
      [1 "1 +" 6 <REPEAT]
      PROFILE-END POP
      PROFILE-DATA
    ))
    stack = @interp.stack
    profile_data = stack.last
    assert_equal "1", profile_data[:word_counts][0][:word]
    assert_equal 7, profile_data[:word_counts][0][:count]
  end

  def test_parallel_map
    @interp.run(%(
      [ 1 2 3 4 5 ] "DUP *" 2 !INTERPS MAP
    ))
    result = @interp.stack_pop
    assert_equal [1, 4, 9, 16, 25], result
  end

  def test_parallel_map_over_record
    @interp.run(%(
      [
        ['a' 1]
        ['b' 2]
        ['c' 3]
        ['d' 4]
      ] REC "3 *" 2 !INTERPS MAP
    ))
    result = @interp.stack_pop
    assert_equal({ 'a' => 3, 'b' => 6, 'c' => 9, 'd' => 12 }, result)
  end


  def test_max_of_two_numbers
    @interp.stack_push(4)
    @interp.stack_push(18)
    @interp.run("MAX")
    assert_equal 18, @interp.stack_pop
  end

  def test_max_of_array_of_numbers
    @interp.stack_push([14, 8, 55, 4, 5])
    @interp.run("MAX")
    assert_equal 55, @interp.stack_pop
  end

  def test_min_of_two_numbers
    @interp.stack_push(4)
    @interp.stack_push(18)
    @interp.run("MIN")
    assert_equal 4, @interp.stack_pop
  end

  def test_min_of_array_of_numbers
    @interp.stack_push([14, 8, 55, 4, 5])
    @interp.run("MIN")
    assert_equal 4, @interp.stack_pop
  end

  def test_mean_of_array_of_numbers
    @interp.stack_push([1, 2, 3, 4, 5])
    @interp.run("MEAN")
    assert_equal 3, @interp.stack_pop
  end

  def test_mean_of_array_of_letters
    @interp.stack_push(["a", "a", "b", "c"])
    @interp.run("MEAN")
    assert_equal({ "a" => 0.5, "b" => 0.25, "c" => 0.25 }, @interp.stack_pop)
  end

  def test_mean_of_array_of_numbers_with_null_values
    @interp.stack_push([1, 2, 3, nil, 4, nil, 5])
    @interp.run("MEAN")
    assert_equal 3, @interp.stack_pop
  end

  def test_mean_of_array_of_letters_with_null_values
    @interp.stack_push(["a", "a", nil, "b", nil, "c"])
    @interp.run("MEAN")
    assert_equal({ "a" => 0.5, "b" => 0.25, "c" => 0.25 }, @interp.stack_pop)
  end

  def test_mean_of_array_of_objects
    @interp.stack_push([
      { a: 1, b: 0 },
      { a: 2, b: 0 },
      { a: 3, b: 0 },
    ])
    @interp.run("MEAN")
    assert_equal({ a: 2, b: 0 }, @interp.stack_pop)
  end

  def test_mean_of_array_of_objects_with_some_numbers_and_some_strings
    @interp.stack_push([
      { a: 0 },
      { a: 1, b: "To Do" },
      { a: 2, b: "To Do" },
      { a: 3, b: "In Progress" },
      { a: 4, b: "Done" },
    ])
    @interp.run("MEAN")
    assert_equal({
      a: 2,
      b: { "To Do" => 0.5, "In Progress" => 0.25, "Done" => 0.25 },
    }, @interp.stack_pop)
  end

  def test_range_index_on_boundary
    value1 = 5
    start_ranges = [0, 5, 10, 20]
    @interp.stack_push(value1)
    @interp.stack_push(start_ranges)
    @interp.run("RANGE-INDEX")
    res1 = @interp.stack_pop
    assert_equal 1, res1
  end

  def test_range_index_before_first_value
    value1 = -5
    start_ranges = [0, 5, 10, 20]
    @interp.stack_push(value1)
    @interp.stack_push(start_ranges)
    @interp.run("RANGE-INDEX")
    res1 = @interp.stack_pop
    assert_nil res1
  end

  def test_range_index_equals_first_value
    value1 = 0
    start_ranges = [0, 5, 10, 20]
    @interp.stack_push(value1)
    @interp.stack_push(start_ranges)
    @interp.run("RANGE-INDEX")
    res1 = @interp.stack_pop
    assert_equal 0, res1
  end

  def test_range_index_in_between_values
    value1 = 3
    start_ranges = [0, 5, 10, 20]
    @interp.stack_push(value1)
    @interp.stack_push(start_ranges)
    @interp.run("RANGE-INDEX")
    res1 = @interp.stack_pop
    assert_equal 0, res1
  end

  def test_range_index_after_last_value
    value1 = 25
    start_ranges = [0, 5, 10, 20]
    @interp.stack_push(value1)
    @interp.stack_push(start_ranges)
    @interp.run("RANGE-INDEX")
    res1 = @interp.stack_pop
    assert_equal 3, res1
  end

  def test_range_index_with_negative_infinity_value
    value1 = -15
    start_ranges = [-Float::INFINITY, 5, 10, 20]
    @interp.stack_push(value1)
    @interp.stack_push(start_ranges)
    @interp.run("RANGE-INDEX")
    res1 = @interp.stack_pop
    assert_equal 0, res1
  end

  def test_str_to_datetime
    @interp.run(%(
      "2020-11-02 10:25 PM" STR>DATETIME
    ))
    datetime = @interp.stack_pop
    assert_equal 2020, datetime.year
    assert_equal 11, datetime.month
    assert_equal 2, datetime.day
    assert_equal 22, datetime.hour
    assert_equal 25, datetime.min
  end

  def test_str_to_timestamp
    @interp.run(%(
      "2020-11-02 10:25 PM" STR>TIMESTAMP
    ))
    timestamp = @interp.stack_pop
    assert_equal 1604384700, timestamp
  end

end