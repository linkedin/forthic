import unittest
from forthic.v2.interpreter import Interpreter
from forthic.v2.modules.org_module import OrgModule, OrgContext


def get_interp():
    result = Interpreter()
    result.register_module(OrgModule)
    result.run('["org"] USE-MODULES')
    result.stack_push(get_context())
    result.run("org.PUSH-CONTEXT!")
    return result


class TestOrgModule(unittest.TestCase):
    def setUp(self):
        self.interp = get_interp()

    def test_FULL_ORG(self):
        self.interp.run("""
        'mgr1' org.FULL-ORG
        'director1' org.FULL-ORG
        """)
        self.assertEqual(["user101", "user102", "user103"], sorted(self.interp.stack[0]))
        self.assertEqual(["mgr1", "mgr2", "user101", "user102", "user103", "user201", "user202", "user203"], sorted(self.interp.stack[1]))

    def test_DIRECT_MANAGERS(self):
        self.interp.run("""
        'mgr1' org.DIRECT-MANAGERS
        'director1' org.DIRECT-MANAGERS
        'vp1' org.DIRECT-MANAGERS
        """)
        self.assertEqual(["mgr1"], sorted(self.interp.stack[0]))
        self.assertEqual(["director1", "mgr1", "mgr2"], sorted(self.interp.stack[1]))
        self.assertEqual(["director1", "vp1"], sorted(self.interp.stack[2]))

    def test_DIRECTS(self):
        self.interp.run("""
        'mgr1' org.DIRECTS
        'director1' org.DIRECTS
        'vp1' org.DIRECTS
        """)
        self.assertEqual(["user101", "user102", "user103"], sorted(self.interp.stack[0]))
        self.assertEqual(["mgr1", "mgr2"], sorted(self.interp.stack[1]))
        self.assertEqual(["director1"], sorted(self.interp.stack[2]))

    def test_GROUP_BY_LEADS(self):
        items = [
            {"key": 101, "owner": "user101"},
            {"key": 102, "owner": "user102"},
            {"key": 202, "owner": "user202"},
            {"key": 203, "owner": "user203"},
            {"key": 302, "owner": "mgr2"},
            {"key": 401, "owner": "user401"},
        ]

        # Store items
        self.interp.run("['items'] VARIABLES")
        self.interp.stack_push(items)
        self.interp.run("items !")

        # Group by mgr1 and mgr2
        self.interp.run('items @ "owner" ["mgr1" "mgr2"] "?" org.GROUP-BY-LEADS')
        grouping = self.interp.stack[0]
        self.assertEqual(2, len(grouping['mgr1']))
        self.assertEqual(3, len(grouping['mgr2']))
        self.assertEqual(1, len(grouping['?']))

        # Group by director1
        self.interp.stack.pop()
        self.interp.run('items @ "owner" ["director1"] "?" org.GROUP-BY-LEADS')
        grouping = self.interp.stack[0]
        self.assertEqual(5, len(grouping['director1']))
        self.assertEqual(1, len(grouping['?']))

    def test_ITEM_to_LEAD(self):
        # ( item field leads default_lead -- lead )
        item = {"key": 101, "owner": "user101"}
        self.interp.stack_push(item)
        self.interp.run("'owner' ['mgr1' 'mgr2'] '?' org.ITEM>LEAD")
        self.assertEqual('mgr1', self.interp.stack[0])

    def test_MANAGER(self):
        self.interp.run("'mgr1' org.MANAGER")
        self.assertEqual('director1', self.interp.stack[0])

    def test_CHAIN(self):
        self.interp.run("'user201' 'vp1' org.CHAIN")
        self.assertEqual(['vp1', 'director1', 'mgr2', 'user201'], self.interp.stack[0])

        self.interp.run("'unknown' 'vp1' org.CHAIN")
        self.assertEqual(['unknown'], self.interp.stack[1])

    def test_CHAIN_KEY_FUNC(self):
        self.interp.run("['user101' 'mgr1' 'user203' 'director1'] 'vp1' org.CHAIN-KEY-FUNC SORT-w/KEY-FUNC")
        self.assertEqual(['director1', 'mgr1', 'user101', 'user203'], self.interp.stack[0])


def get_context():
    def get_users_managers():
        res = [
            ["user101", "mgr1"],
            ["user102", "mgr1"],
            ["user103", "mgr1"],
            ["user201", "mgr2"],
            ["user202", "mgr2"],
            ["user203", "mgr2"],
            ["mgr1", "director1"],
            ["mgr2", "director1"],
            ["director1", "vp1"]
        ]
        return res

    result = OrgContext(get_users_managers)
    return result


if __name__ == '__main__':
    unittest.main()