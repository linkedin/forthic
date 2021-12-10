import { Module, Word, ModuleWord } from '../module.mjs';


class OrgModule extends Module {
    constructor(interp) {
        super("org", interp, FORTHIC);

        let self = this;
        this.org_contexts = [];
        this.add_exportable_word(new ModuleWord("SET-CONTEXT!", (interp) => this.word_SET_CONTEXT_bang(interp), self));
        this.add_exportable_word(new ModuleWord("PUSH-CONTEXT!", (interp) => this.word_PUSH_CONTEXT_bang(interp), self));
        this.add_exportable_word(new ModuleWord("POP-CONTEXT!", (interp) => this.word_POP_CONTEXT_bang(interp), self));
        this.add_exportable_word(new ModuleWord("FULL-ORG", (interp) => this.word_FULL_ORG(interp), self));
        this.add_exportable_word(new ModuleWord("DIRECT-MANAGERS", (interp) => this.word_DIRECT_MANAGERS(interp), self));
        this.add_exportable_word(new ModuleWord("GROUP-BY-LEADS", (interp) => this.word_GROUP_BY_LEADS(interp), self));
        this.add_exportable_word(new ModuleWord("ITEM>LEAD", (interp) => this.word_ITEM_to_LEAD(interp), self));
    }

    // ( user_managers -- )
    // Sets context for org computations
    //     `user_managers` is a list of pairs [username, manager_username]
    word_SET_CONTEXT_bang(interp) {
        let self = this;

        let user_managers = interp.stack_pop();
        let org_context = new OrgContext(user_managers);
        self.org_contexts = [org_context]
    }

    // ( user_managers -- )
    // Sets context for org computations
    //     `user_managers` is a list of pairs [username, manager_username]
    word_PUSH_CONTEXT_bang(interp) {
        let self = this;

        let user_managers = interp.stack_pop();
        let org_context = new OrgContext(user_managers);
        self.org_contexts.push(org_context);
    }


    // ( -- )
    // Restores previous context for org computations
    word_POP_CONTEXT_bang(interp) {
        let self = this;
        self.org_contexts.pop();
    }

    // (manager -- usernames)
    // Returns all usernames reporting up to manager
    word_FULL_ORG(interp) {
        let self = this;
        let manager = interp.stack_pop();
        let org_context = self.org_contexts[self.org_contexts.length-1];
        let result = org_context.full_org(manager);
        interp.stack_push(result)
    }

    // (manager -- usernames)
    // Returns usernames of direct reports of a manager who are also managers
    // NOTE: This also returns the manager at the end of the list
    word_DIRECT_MANAGERS(interp) {
        let self = this;
        let manager = interp.stack_pop();
        let org_context = self.org_contexts[self.org_contexts.length-1];
        let result = org_context.get_direct_managers(manager);
        interp.stack_push(result);
    }

    // ( items field leads default_lead -- record )
    word_GROUP_BY_LEADS(interp) {
        let self = this;
        let default_lead = interp.stack_pop();
        let leads = interp.stack_pop();
        let field = interp.stack_pop();
        let items = interp.stack_pop();

        let org_context = self.org_contexts[self.org_contexts.length-1];
        let result = org_context.group_by_leads(items, field, leads, default_lead);
        interp.stack_push(result);
    }

    // ( item field leads default_lead -- lead )
    word_ITEM_to_LEAD(interp) {
        let self = this;
        let default_lead = interp.stack_pop();
        let leads = interp.stack_pop();
        let field = interp.stack_pop();
        let item = interp.stack_pop();

        let org_context = self.org_contexts[self.org_contexts.length-1];
        let result = org_context.item_to_lead(item, field, leads, default_lead);
        interp.stack_push(result);
    }
}


const FORTHIC = `
`;


class OrgContext {
    constructor(user_managers) {
        let self = this;
        self.user_managers = user_managers;

        function make_user_to_manager() {
            let res = {};
            user_managers.forEach(p => {
                res[p[0]] = p[1]
            });
            return res
        }

        function get_managers() {
            let managers = {};
            user_managers.forEach(p => {
                managers[p[1]] = true;
            });
            let res = [];
            Object.keys(managers).forEach(m => res.push(m));
            return res;
        }

        self.user_to_manager = make_user_to_manager();
        self.managers = get_managers();

        function gather_direct_managers() {
            let res = {};
            self.managers.forEach(m => {
                let m_manager = self.user_to_manager[m];
                if (res[m_manager])   res[m_manager].push(m);
                else                  res[m_manager] = [m];
            });
            return res;
        }

        self.direct_managers = gather_direct_managers()
    }

    // Returns all managers that are part of a root_manager's org
    org_managers(root_manager) {
        let self = this;

        if (!self.direct_managers[root_manager])   return [root_manager];

        function add_directs(manager, res) {
            if (!self.direct_managers[manager])   return;

            let directs = self.direct_managers[manager];
            directs.forEach(m => {
                if (m != manager) {
                    res.push(m);
                    add_directs(m, res);
                }
            });
        }

        let result = [];
        result.push(root_manager);
        add_directs(root_manager, result);
        return result;
    }

    // Returns a list of people rolling up to a manager
    full_org(manager) {
        let self = this;
        let org_managers = self.org_managers(manager)

        function get_lead(username) {
            let manager = self.user_to_manager[username];
            if (org_managers.indexOf(manager) >= 0)   return manager;
            else                                      return null;
        }

        let result = [];
        Object.keys(self.user_to_manager).forEach(username => {
            let lead = get_lead(username);
            if (lead)   result.push(username);
        });

        return result
    }

    // Returns direct reports of a user who are managers
    get_direct_managers(username) {
        let self = this;
        let result = self.direct_managers[username];
        if (!result)   result = [];
        result.push(username)
        return result;
    }

    group_by_leads(items, field, leads, default_lead) {
        if (!items)   return {};

        let self = this;
        let manager_to_lead = {};
        leads.forEach(lead => {
            let managers = self.org_managers(lead);
            managers.forEach(m => {
                manager_to_lead[m] = lead;
            });
        });

        // Group items by lead
        let result = {};
        result[default_lead] = []
        leads.forEach(lead => {
            result[lead] = [];
        });

        items.forEach(item => {
            let username;
            if (field == null)   username = item;
            else                 username = item[field];

            let lead = manager_to_lead[username]

            // If user is not a manger, get their manager and map to lead
            if (!lead) {
                let manager = self.user_to_manager[username];
                lead = manager_to_lead[manager]
            }

            if (!lead)   lead = default_lead;

            result[lead].push(item);
        });
        return result;
    }

    item_to_lead(item, field, leads, default_lead) {
        let self = this;
        let username;
        if (field == null)   username = item;
        else                 username = item[field];

        function get_lead(username) {
            if (leads.indexOf(username) >= 0)   return username;

            username = self.user_to_manager[username]
            if (!username)    return default_lead
            return get_lead(username);
        }

        let result = get_lead(username);
        return result
    }
}



function new_module(interp) {
    return new OrgModule(interp);
}

export { OrgModule, new_module };
