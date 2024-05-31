import collections
from ..module import Module
from ..interfaces import IInterpreter
from typing import List, Callable, Dict, Optional, Any


class OrgModule(Module):
    def __init__(self, interp: IInterpreter):
        super().__init__('org', interp)

        self.org_contexts: List['OrgContext'] = []

        self.flags = {
            "with_lead": None,
        }

        self.add_module_word('PUSH-CONTEXT!', self.word_PUSH_CONTEXT_bang)
        self.add_module_word('POP-CONTEXT!', self.word_POP_CONTEXT_bang)
        self.add_module_word('ROOT-MANAGERS', self.word_ROOT_MANAGERS)
        self.add_module_word('FULL-ORG', self.word_FULL_ORG)
        self.add_module_word('ORG-MANAGERS', self.word_ORG_MANAGERS)
        self.add_module_word('DIRECTS', self.word_DIRECTS)
        self.add_module_word('DIRECT-MANAGERS', self.word_DIRECT_MANAGERS)
        self.add_module_word('GROUP-BY-LEADS', self.word_GROUP_BY_LEADS)
        self.add_module_word('ITEM>LEAD', self.word_ITEM_to_LEAD)
        self.add_module_word('MANAGER', self.word_MANAGER)
        self.add_module_word('CHAIN', self.word_CHAIN)
        self.add_module_word('CHAIN-KEY-FUNC', self.word_CHAIN_KEY_FUNC)
        self.add_module_word('USERS-MANAGERS', self.word_USERS_MANAGERS)

        self.add_module_word('!WITH-LEAD', self.word_bang_WITH_LEAD)

    # ( org_context -- )
    def word_PUSH_CONTEXT_bang(self, interp: IInterpreter):
        """Sets context for org computations"""
        org_context = interp.stack_pop()
        self.org_contexts.append(org_context)

    # ( -- )
    def word_POP_CONTEXT_bang(self, interp: IInterpreter):
        """Restores previous context for org computations"""
        self.org_contexts.pop()

    # ( -- username)
    def word_ROOT_MANAGERS(self, interp: IInterpreter):
        """Returns root manager of org context"""
        org_context = self.current_context()
        result = org_context.root_managers()
        interp.stack_push(result)

    # (manager -- usernames)
    def word_FULL_ORG(self, interp: IInterpreter):
        """Returns all usernames reporting up to manager"""
        manager = interp.stack_pop()
        org_context = self.current_context()
        flags = self.get_flags()

        result = org_context.full_org(manager)
        if flags.get('with_lead'):
            result = [manager] + result
        interp.stack_push(result)

    # (manager -- usernames)
    def word_ORG_MANAGERS(self, interp: IInterpreter):
        """Returns all manager usernames reporting up to manager"""
        manager = interp.stack_pop()
        org_context = self.current_context()
        flags = self.get_flags()

        result = org_context.org_managers(manager)

        if not flags.get('with_lead'):
            result = result[1:]

        interp.stack_push(result)

    # (manager -- usernames)
    def word_DIRECTS(self, interp: IInterpreter):
        """Returns usernames of direct reports of a manager
        """
        manager = interp.stack_pop()
        org_context = self.current_context()
        flags = self.get_flags()

        result = org_context.get_directs(manager)
        if flags.get('with_lead'):
            result = [manager] + result
        interp.stack_push(result)

    # (manager -- usernames)
    def word_DIRECT_MANAGERS(self, interp: IInterpreter):
        """Returns usernames of direct reports of a manager who are also managers

        NOTE: This also returns the manager at the end of the list
        """
        manager = interp.stack_pop()
        org_context = self.current_context()
        flags = self.get_flags()

        result = org_context.get_direct_managers(manager)
        if flags.get('with_lead'):
            result = [manager] + result
        interp.stack_push(result)

    # ( items field leads default_lead -- record )
    def word_GROUP_BY_LEADS(self, interp: IInterpreter):
        default_lead = interp.stack_pop()
        leads = interp.stack_pop()
        field = interp.stack_pop()
        items = interp.stack_pop()

        org_context = self.current_context()
        result = org_context.group_by_leads(items, field, leads, default_lead)
        interp.stack_push(result)

    # ( item field leads default_lead -- lead )
    def word_ITEM_to_LEAD(self, interp: IInterpreter):
        default_lead = interp.stack_pop()
        leads = interp.stack_pop()
        field = interp.stack_pop()
        item = interp.stack_pop()

        org_context = self.current_context()
        result = org_context.item_to_lead(item, field, leads, default_lead)
        interp.stack_push(result)

    # ( username -- manager )
    def word_MANAGER(self, interp: IInterpreter):
        username = interp.stack_pop()

        org_context = self.current_context()
        result = org_context.get_manager(username)
        interp.stack_push(result)

    # ( username root_username -- usernames )
    def word_CHAIN(self, interp: IInterpreter):
        root_username = interp.stack_pop()
        username = interp.stack_pop()
        org_context = self.current_context()
        result = org_context.get_chain(username, root_username)
        interp.stack_push(result)

    # ( root_username -- key_func )
    def word_CHAIN_KEY_FUNC(self, interp: IInterpreter):
        """Returns a function that can be used as a key function in SORT

        The comparator returns an integer giving the distance from a user to the root.
        """
        root_username = interp.stack_pop()
        org_context = self.current_context()

        def result(username: str) -> int:
            chain = org_context.get_chain(username, root_username)
            if username == root_username:
                res = 0
            else:
                res = len(chain)
            return res

        interp.stack_push(result)

    # ( -- user_mgr_pairs )
    def word_USERS_MANAGERS(self, interp: IInterpreter):
        """Returns an array of user/manager pairs
        """
        org_context = self.current_context()
        result = org_context.get_users_managers()
        interp.stack_push(result)

    # ( -- )
    def word_bang_WITH_LEAD(self, interp: IInterpreter):
        self.flags["with_lead"] = True

    # =================================
    # Helpers
    def get_flags(self):
        flags = self.flags.copy()
        self.flags = {}
        return flags

    def current_context(self):
        if not self.org_contexts:
            raise RuntimeError(
                'Use org.PUSH-CONTEXT! to provide an Org context'
            )

        result = self.org_contexts[-1]
        return result


class OrgContext:
    def __init__(self, get_users_managers: Callable[[], List[List[str]]]):
        """The `get_users_managers` function returns a list of pairs [username, manager_username].
        This information is used to construct a hierarchy.
        """
        self.get_users_managers = get_users_managers
        self.user_managers = self.get_users_managers()

        def make_user_to_manager() -> Dict[str, str]:
            res: Dict[str, str] = {}
            for p in self.user_managers:
                res[p[0]] = p[1]
            return res

        def make_manager_to_users() -> Dict[str, List[str]]:
            res: Dict[str, List[str]] = collections.defaultdict(list)
            for p in self.user_managers:
                res[p[1]].append(p[0])
            return res

        self.user_to_manager = make_user_to_manager()
        self.managers = list(set(self.user_to_manager.values()))
        self.manager_to_users = make_manager_to_users()

        def gather_direct_managers() -> Dict[Optional[str], List[str]]:
            res = collections.defaultdict(list)
            for m in self.managers:
                res[self.user_to_manager.get(m)].append(m)
            return res

        self.direct_managers = gather_direct_managers()

    def root_managers(self):
        managers = list(set(self.user_to_manager.values()))
        result = [m for m in managers if self.user_to_manager.get(m) is None]
        return result

    def get_manager(self, username: str) -> Optional[str]:
        result = self.user_to_manager.get(username)
        return result

    def org_managers(self, root_manager: str) -> List[str]:
        """Returns all managers that are part of a root_manager's org, including the root_manager"""
        if root_manager not in self.direct_managers:
            return [root_manager]

        def add_directs(manager, res):
            if manager not in self.direct_managers:
                return
            directs = self.direct_managers[manager]
            for m in directs:
                if m != manager:
                    res.append(m)
                    add_directs(m, res)

        result = []
        result.append(root_manager)
        add_directs(root_manager, result)
        return result

    def full_org(self, manager: str) -> List[str]:
        """Returns a list of people rolling up to a manager"""
        org_managers = self.org_managers(manager)

        def get_lead(username):
            manager = self.user_to_manager[username]
            if manager in org_managers:
                return manager
            return None

        result = []
        for username in self.user_to_manager:
            lead = get_lead(username)
            if lead:
                result.append(username)
        return result

    def get_directs(self, username: str) -> List[str]:
        """Returns direct reports of a user"""
        result = self.manager_to_users.get(username)
        if result is None:
            result = []
        return result

    def get_direct_managers(self, username: str) -> List[str]:
        """Returns direct reports of a user who are managers"""
        result = self.direct_managers.get(username)
        if not result:
            result = []
        result = result[:]
        result.sort()
        return result

    def group_by_leads(self, items: List[Dict[str, Any]], field: str, leads: List[str], default_lead: str) -> Dict[str, List[Any]]:
        manager_to_lead: Dict[str, str] = {}
        lead: Optional[str] = None

        if not items:
            items = []

        if not leads:
            leads = []

        for lead in leads:
            managers = self.org_managers(lead)
            for m in managers:
                manager_to_lead[m] = lead

        # Group items by lead
        result: Dict[str, List[Any]] = collections.defaultdict(list)
        for lead in leads:
            result[lead] = []

        for item in items:
            if field is None:
                username = item
            else:
                username = item[field]

            lead = manager_to_lead.get(username)

            # If user is not a manger, get their manager and map to lead
            if not lead:
                manager = self.user_to_manager.get(username)
                if manager:
                    lead = manager_to_lead.get(manager)

            if not lead:
                lead = default_lead

            result[lead].append(item)
        return result

    def item_to_lead(self, item: Dict[str, Any], field: str, leads: List[str], default_lead: str):
        if field is None:
            username = item
        else:
            username = item[field]

        if not leads:
            leads = []

        # Recursively climb org tree until we find a lead in `leads`
        def get_lead(username: Optional[str]) -> str:
            if not username:
                return default_lead

            if username in leads:
                return username

            username = self.user_to_manager.get(username)
            if not username:
                return default_lead
            return get_lead(username)

        result = get_lead(username)
        return result

    def get_chain(self, username: str, root_username: str) -> List[str]:
        result: List[str] = []
        cur_user = username

        while True:
            result.append(cur_user)
            cur_user = self.user_to_manager.get(cur_user)   # type: ignore

            if cur_user == root_username:
                result.append(root_username)
                break

            if not cur_user:
                break

        result.reverse()
        return result
