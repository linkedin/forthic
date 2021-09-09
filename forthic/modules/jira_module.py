import re
import requests
from dateutil import parser
from ..module import Module
from ..global_module import drill_for_value
from collections import defaultdict
from ..utils.errors import UnauthorizedError
from ..interfaces import IInterpreter
from typing import List, Any, Dict, Optional


class JiraError(RuntimeError):
    pass


class JiraModule(Module):
    """This implements support for common use cases when interacting with Jira.

    See `docs/modules/jira_module.md` for detailed descriptions of each word.
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('jira', interp, JIRA_FORTHIC)
        self.context_stack: List['JiraContext'] = []

        self.add_module_word('PUSH-CONTEXT!', self.word_PUSH_CONTEXT_bang)
        self.add_module_word('POP-CONTEXT!', self.word_POP_CONTEXT_bang)

        self.add_module_word('HOST', self.word_HOST)
        self.add_module_word('SEARCH', self.word_SEARCH)
        self.add_module_word('DEFAULT-SEARCH', self.word_DEFAULT_SEARCH)
        self.add_module_word('RENDERED-SEARCH', self.word_RENDERED_SEARCH)

        self.add_module_word('CREATE', self.word_CREATE)
        self.add_module_word('UPDATE', self.word_UPDATE)
        self.add_module_word('ADD-WATCHER', self.word_ADD_WATCHER)
        self.add_module_word('LINK-ISSUES', self.word_LINK_ISSUES)
        self.add_module_word('VOTES', self.word_VOTES)

        self.add_module_word('CHANGELOG', self.word_CHANGELOG)
        self.add_module_word('FIELD-AS-OF', self.word_FIELD_AS_OF)

        self.add_module_word('FIELD-TAG', self.word_FIELD_TAG)
        self.add_module_word('REMOVE-FIELD-TAGS', self.word_REMOVE_FIELD_TAGS)
        self.add_module_word('<FIELD-TAG!', self.word_l_FIELD_TAG_bang)

    # ( context -- )
    def word_PUSH_CONTEXT_bang(self, interp: IInterpreter):
        context = interp.stack_pop()
        self.context_stack.append(context)

    # ( -- )
    def word_POP_CONTEXT_bang(self, interp: IInterpreter):
        self.context_stack.pop()

    # ( -- host )
    def word_HOST(self, interp: IInterpreter):
        context = self.current_context()
        interp.stack_push(context.get_host())

    # ( jql fields -- tickets )
    def word_SEARCH(self, interp: IInterpreter):
        """Returns the minimal requested info. E.g., keys instead of records"""
        fields = interp.stack_pop()
        jql = interp.stack_pop()

        result = self.search(jql, fields)

        # Extract key, if possible
        for r in result:
            for key, value in r.items():
                r[key] = self.simplify_value(key, value)

        interp.stack_push(result)

    # ( jql fields -- tickets )
    def word_DEFAULT_SEARCH(self, interp: IInterpreter):
        """Returns the default data returned from the Jira APIs"""
        fields = interp.stack_pop()
        jql = interp.stack_pop()
        result = self.search(jql, fields)
        interp.stack_push(result)

    # ( jql fields -- tickets )
    def word_RENDERED_SEARCH(self, interp: IInterpreter):
        """Returns data expanded to include "rendered" data"""
        fields = interp.stack_pop()
        jql = interp.stack_pop()
        result = self.search(jql, fields, expand=['renderedFields'])
        interp.stack_push(result)

    # ( record -- ticket_key )
    def word_CREATE(self, interp: IInterpreter):
        """Creates a new Jira ticket"""
        record = interp.stack_pop()
        normalized_rec = self.normalize_ticket_record(record)

        context = self.current_context()

        request_data = {
            'fields': normalized_rec,
        }

        api_url = '/rest/api/2/issue'
        response = context.requests_post(api_url, json=request_data)

        if response.status_code != 201:
            raise JiraError(
                f'Unable to create issue {record}: {response.text}'
            )
        res_data = response.json()
        result = res_data['key']
        interp.stack_push(result)

    # ( ticket_key record -- )
    def word_UPDATE(self, interp: IInterpreter):
        record = interp.stack_pop()
        ticket_key = interp.stack_pop()

        context = self.current_context()
        normalized_rec = self.normalize_ticket_record(record)
        request_data = {
            'fields': normalized_rec,
        }
        api_url = f'/rest/api/2/issue/{ticket_key}'
        response = context.requests_put(api_url, json=request_data)

        if response.status_code != 204:
            raise JiraError(
                f'Unable to update issue {record}: {response.text}'
            )

    # ( ticket_key username -- )
    def word_ADD_WATCHER(self, interp: IInterpreter):
        username = interp.stack_pop()
        ticket_key = interp.stack_pop()
        context = self.current_context()

        request_data = username
        api_url = f'/rest/api/2/issue/{ticket_key}/watchers'
        response = context.requests_post(api_url, json=request_data)

        if response.status_code != 204:
            raise JiraError(
                f'Unable to add watcher {username} to issue {ticket_key}: {response.text}'
            )

    # ( in_key out_key link_type -- )
    def word_LINK_ISSUES(self, interp: IInterpreter):
        link_type = interp.stack_pop()
        out_key = interp.stack_pop()
        in_key = interp.stack_pop()

        context = self.current_context()

        request_data = {
            'type': {'name': link_type},
            'inwardIssue': {'key': in_key},
            'outwardIssue': {'key': out_key},
        }
        api_url = '/rest/api/2/issueLink'
        response = context.requests_post(api_url, json=request_data)
        if response.status_code != 201:
            raise JiraError(
                f"Unable to link issues {in_key} {out_key} with link type '{link_type}'"
            )

    # ( ticket_key -- votes)
    def word_VOTES(self, interp: IInterpreter):
        ticket_key = interp.stack_pop()
        context = self.current_context()

        api_url = f'/rest/api/2/issue/{ticket_key}/votes'
        res = context.requests_get(api_url)

        data = res.json()
        result = [voter.get('name') for voter in data['voters']]
        interp.stack_push(result)

    # ( ticket_key fields -- changes )
    def word_CHANGELOG(self, interp: IInterpreter):
        fields = interp.stack_pop()
        key = interp.stack_pop()

        if not key:
            result = []
        else:
            result = self.get_changelog(key, fields)

        interp.stack_push(result)

    # ( date changes field -- value )
    def word_FIELD_AS_OF(self, interp: IInterpreter):
        field = interp.stack_pop()
        changes = interp.stack_pop()
        date = interp.stack_pop()

        def select_field_changes(changes: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
            if not changes:
                changes = []
            result = []
            for c in changes:
                if c['field'] == field:
                    result.append(c)
            return result

        field_changes = select_field_changes(changes)

        def change_containing_date() -> Optional[Dict[str, Any]]:
            res = None
            for c in field_changes:
                change_date = c['date'].date()
                if change_date > date:
                    break
                else:
                    res = c
            return res

        change = change_containing_date()
        result = None
        if change:
            result = change['to']

        interp.stack_push(result)

    # ( ticket field key -- value )
    def word_FIELD_TAG(self, interp: IInterpreter):
        key = interp.stack_pop()
        field = interp.stack_pop()
        ticket = interp.stack_pop()

        field_text = ticket.get(field)
        result = self.get_field_tag_value(field_text, key)
        interp.stack_push(result)

    # ( string -- stringvalue )
    def word_REMOVE_FIELD_TAGS(self, interp: IInterpreter):
        string = interp.stack_pop()

        # Escape "[http...]
        RS = chr(30)  # Record Separator
        http_escaped = re.sub(r'\[(http.+?:.+?)\]', f'{RS}' + r'\g<1>' + f'{RS}', string)

        # Remove field tags
        no_field_tags = re.sub(r'\[[^\]]+?:.+?\]', '', http_escaped)

        # Unescape http
        result = re.sub(f'{RS}(.+?){RS}', r'[\g<1>]', no_field_tags)
        interp.stack_push(result)

    # ( ticket_rec field key value -- ticket )
    def word_l_FIELD_TAG_bang(self, interp: IInterpreter):
        value = interp.stack_pop()
        key = interp.stack_pop()
        field = interp.stack_pop()
        ticket = interp.stack_pop()

        # Get field value from ticket
        field_value = ticket[field]
        if not field_value or field_value == 'None':
            field_value = ''

        # Append/replace field tag with new value
        pattern = re.compile(r'\[%s:.*\]' % key, re.DOTALL)
        new_field_tag = f'[{key}: {value}]'
        (field_value, num) = pattern.subn(new_field_tag, field_value)
        if num == 0:
            field_value += '\n\n' + new_field_tag

        # Return result
        ticket[field] = field_value
        interp.stack_push(ticket)

    # =================================
    # Helpers
    def current_context(self):
        if not self.context_stack:
            raise JiraError('Use jira.PUSH-CONTEXT! to provide a Jira context')

        result = self.context_stack[-1]
        return result

    def get_field_tag_value(self, field_text, key, ignore_case=True):
        if not field_text:
            field_text = ''

        def get_field_tags(string: str) -> Dict[str, str]:
            """Extracts all field tags from string returns a hash of them.

            Field tags look like this: "[rank: 1]"
            """
            m_iter = re.finditer(
                r'\[([^\]]+):\s*([^\]]*)\]', string, re.MULTILINE | re.DOTALL
            )
            res = {}
            for match in m_iter:
                tag = match.group(1)
                value = match.group(2)
                res[tag] = value
            return res

        def to_lowercase(d: Dict[str, Any]) -> Dict[str, Any]:
            res = {}
            for k, v in d.items():
                res[k.lower()] = v
            return res

        field_tags = get_field_tags(field_text)
        if ignore_case:
            field_tags = to_lowercase(field_tags)
            value = field_tags.get(key.lower())
        else:
            value = field_tags.get(key)

        result = value
        if not result:
            result = ' '
        return result

    def get_changelog(self, ticket_key: str, fields: List[str]) -> List[Dict[str, Any]]:
        context = self.current_context()

        normalized_fields = [self.normalize_field(f) for f in fields]

        api_url = f"/rest/api/2/issue/{ticket_key}?expand=changelog&fields={','.join(normalized_fields + ['created'])}"
        res = context.requests_get(api_url)
        if not res.ok:
            raise JiraError(
                f"Can't get changelog for {ticket_key}: {res.text}"
            )

        ticket = res.json()

        # NOTE: Changelog response uses field names instead of IDs. This may lead to a bug if there are duplicate
        #       custom field names
        def get_ticket_changes(ticket: Dict[str, Any]) -> List[Dict[str, Any]]:
            result = []
            for history in ticket['changelog']['histories']:
                for item in history['items']:
                    item_field = item['field']
                    if item_field in fields:
                        result.append(
                            {
                                'date': parser.parse(history['created']),
                                'field': item_field,
                                'from': item['fromString'],
                                'to': item['toString'],
                                'from_': item['from'],
                                'to_': item['to'],
                            }
                        )
            return result

        changes = get_ticket_changes(ticket)

        def get_first_change(field: str) -> Optional[Any]:
            res = None
            for c in changes:
                if c['field'] == field:
                    res = c
                    break
            return res

        def create_initial_change(field: str, value: Any):
            res = {
                'date': parser.parse(ticket['fields']['created']),
                'field': field,
                'from': '',
                'to': value,
            }
            return res

        def get_initial_change(field: str) -> Any:
            """This handles two cases: when a field has been set at least once, and when a field has never been set"""
            normalized_field = normalized_fields[fields.index(field)]
            first_change = get_first_change(field)
            res = None
            if first_change:
                res = create_initial_change(field, first_change['from'])
            else:
                value = ticket['fields'][normalized_field]
                string_value = str(self.simplify_value(field, value))
                res = create_initial_change(field, string_value)
            return res

        # Pull changes and construct result
        result = [get_initial_change(f) for f in fields] + changes
        return result

    def simplify_value(self, user_field: str, val: Any):
        """
        This extracts simple values from Jira value records. See normalize_value for more info.
        """
        context = self.current_context()

        field = self.normalize_field(user_field)
        schema = context.field_to_schema.get(field)
        if not schema:
            schema = {'type': '?'}

        def simplify_schema_value(schema_type: str, value: Any) -> Any:
            if not value:
                res = None
            elif schema_type == 'array':
                if value:
                    res = [
                        simplify_schema_value(schema['items'], v)
                        for v in value
                    ]
                else:
                    res = []
            else:
                if schema_type in ('date', 'datetime', 'string', 'number'):
                    res = value
                elif schema_type in ('timetracking'):
                    res = value
                elif schema_type in ('option'):
                    res = value['value']
                elif schema_type in ('option-with-child'):
                    res = value
                elif schema_type in ('project'):
                    res = value['key']
                elif isinstance(value, dict) and 'name' in value:
                    res = value['name']
                elif isinstance(value, dict) and 'displayName' in value:
                    res = value['displayName']
                else:
                    res = value
            return res

        result = simplify_schema_value(schema['type'], val)
        return result

    def normalize_value(self, field: str, val: Any) -> Any:
        """
        Most field values will be single items. Some will be arrays of items:

        * option-with-child:   ["parent-value" "child-value"]
        * timestracking:       ["original-estimate" "remaining-estimate"]

        We assume that any value that can take an ID or name will take a name.

        The value of the record fields must normalized according schema of the Jira field. Here are
        examples of normalized values for given types:

        * array:             An array of values like [{"name": "jsmith"}, {"name": "bjones"}]
        * date:              "2011-10-03"
        * datetime:          "2011-10-19T10:29:29.908+1100"
        * group:             {"name": "jira-devs"}
        * issuetype:         {"name": "bug"}
        * number:            42.07
        * option:            {"value": "green"}
        * option-with-child: {"value": "green", "child": {"value":"blue"} }
        * priority:          {"name": "Critical"}
        * parent:            {"key": "PROJ-1234"}
        * project:           {"key": "JIRA"}
        * resolution:        {"name": "Fixed"}
        * securitylevel:     {"name": "?"}
        * string:            "Howdy"
        * timetracking:      {"originalEstimate": "1d 2h", "remainingEstimate": "3h 25m"}
        * user:              {"name": "jsmith"}
        * version:           {"name": "5.0"}
        """
        context = self.current_context()
        schema = context.field_to_schema.get(field)

        # Handle "parent" field as a special case
        if field == 'parent':
            schema = {'type': 'parent'}

        if not schema:
            raise JiraError(f'Could not find schema for field {field}')

        def schematize_value(schema_type: str, value: Any) -> Any:
            if schema_type == 'array':
                res: Any = [schematize_value(schema['items'], v) for v in value]
            else:
                if schema_type in ('date', 'datetime', 'string', 'number'):
                    res = value
                elif schema_type in ('timetracking'):
                    res = {
                        'originalEstimate': value[0],
                        'remainingEstimate': value[1],
                    }
                elif schema_type in ('option'):
                    res = {'value': value}
                elif schema_type in ('option-with-child'):
                    res = {'value': value[0], 'child': {'value': value[1]}}
                elif schema_type in ('project', 'parent'):
                    res = {'key': value}
                else:
                    res = {'name': value}
            return res

        result = schematize_value(schema['type'], val)
        return result

    def normalize_ticket_record(self, record: Dict[str, Any]) -> Dict[str, Any]:
        """This normalizes fields and values of the specified ticket record."""
        result = {}
        for field, value in record.items():
            normalized_field = self.normalize_field(field)
            result[normalized_field] = self.normalize_value(
                normalized_field, value
            )
        return result

    def search(self, jql: str, fields: List[str], expand: List[str] = []):
        """Uses jql to search Jira, returning records with the specified fields"""
        if jql.strip() == '':
            raise JiraError('JQL must not be blank')

        context = self.current_context()

        # key always comes back in the results. Specifying it will cause the value to be nulled out
        if 'key' in fields:
            fields.remove('key')

        normalized_fields = [self.normalize_field(f) for f in fields]
        batch_size = 200

        def run_batch(start_at, session):
            req_data = {
                'jql': jql,
                'startAt': start_at,
                'maxResults': batch_size,
                'fields': normalized_fields,
            }

            if len(expand) > 0:
                req_data['expand'] = expand

            api_url = '/rest/api/2/search'
            response = context.requests_post(
                api_url, json=req_data, session=session
            )

            if not response.ok:
                raise JiraError(
                    f"Problem doing Jira search '{jql}': {response.text}"
                )
            res_data = response.json()
            res = res_data['issues']
            return res

        def run(session: requests.Session):
            res = []
            start_at = 0
            while True:
                batch = run_batch(start_at, session)
                res += batch
                if len(batch) < batch_size:
                    break
                start_at += batch_size
            return res

        with requests.Session() as session:
            issues = run(session)

        def issue_data_to_record(issue_data: Dict[str, Any]) -> Dict[str, Any]:
            res = {'key': issue_data['key']}
            # Map field back to what the user provided
            for field in fields:
                normalized_field = self.normalize_field(field)
                value = drill_for_value(
                    issue_data, ['fields', normalized_field]
                )
                res[field] = value
            return res

        result = [issue_data_to_record(d) for d in issues]
        return result

    def normalize_field(self, field: str) -> str:
        """If field doesn't correspond to a field ID, search for name of field in field map"""
        context = self.current_context()
        if field in context.field_map:
            return field
        ids = context.field_name_to_id.get(field)

        if ids is None:
            return field

        if len(ids) > 1:
            raise JiraError(
                f"Jira field '{field}' corresponds to multiple field ids: {ids}"
            )

        result = ids[0]
        return result


class JiraContext:
    """Override this and pass to PUSH-CONTEXT! in order to make Jira calls"""

    def __init__(self):
        self.field_map = self.get_field_map()
        self.field_name_to_id = self.make_field_name_to_id()
        self.field_to_schema = self.make_field_schema()

    def get_field_map(self):
        api_url = '/rest/api/2/field'
        res = self.requests_get(api_url)
        if res.status_code == 401:
            raise UnauthorizedError(self.get_field())

        if not res.ok:
            raise JiraError(f'Unable to load Jira field map: {res.text}')

        field_records = res.json()

        result = {}
        for f in field_records:
            result[f['id']] = f
        return result

    def make_field_name_to_id(self):
        """Constructs a mapping from field names to IDs

        NOTE: There may be more than one field corresponding to the same name
        """
        result = defaultdict(list)
        for field_id, record in self.field_map.items():
            name = record['name']
            result[name].append(field_id)
        return result

    def make_field_schema(self):
        result = {}
        for field, rec in self.field_map.items():
            if 'schema' in rec:
                result[field] = rec['schema']
        return result

    def requests_get(self, api_url, session=None):
        """Makes HTTP GET call to pull data"""
        api_url_w_host = self.get_host() + api_url
        if session:
            result = session.get(
                api_url_w_host,
                auth=(self.get_username(), self.get_password()),
                verify=self.get_cert_verify(),
            )
        else:
            result = requests.get(
                api_url_w_host,
                auth=(self.get_username(), self.get_password()),
                verify=self.get_cert_verify(),
            )
        return result

    def requests_post(self, api_url, json=None, session=None):
        api_url_w_host = self.get_host() + api_url
        if session:
            result = session.post(
                api_url_w_host,
                auth=(self.get_username(), self.get_password()),
                json=json,
                verify=self.get_cert_verify(),
            )
        else:
            result = requests.post(
                api_url_w_host,
                auth=(self.get_username(), self.get_password()),
                json=json,
                verify=self.get_cert_verify(),
            )
        return result

    def requests_put(self, api_url, json=None, session=None):
        api_url_w_host = self.get_host() + api_url
        if session:
            result = session.put(
                api_url_w_host,
                auth=(self.get_username(), self.get_password()),
                json=json,
                verify=self.get_cert_verify(),
            )
        else:
            result = requests.put(
                api_url_w_host,
                auth=(self.get_username(), self.get_password()),
                json=json,
                verify=self.get_cert_verify(),
            )
        return result

    def get_field(self):
        return None

    def get_host(self):
        return None

    # Override this to supply the path to the cert file to use. Use False to skip verification
    def get_cert_verify(self):
        return False

    def get_username(self):
        return None

    def get_password(self):
        return None


JIRA_FORTHIC = '''
# Link types
: DEPENDENCY            "Dependency";   # in_key "depends on" out_key
: ACTION-ITEM           "Action Item";  # in_key "has action item for" out_key
: CLONERS               "Cloners";      # in_key "cloned from" out_key
: DUPLICATE             "Duplicate";    # in_key "duplicates" out_key
: ISSUE-SPLIT           "Issue Split";  # in_key "split to" out_key
: RELATED               "Related";      # in_key "related to" out_key
: REQUIRE               "Require";      # in_key "requires" out_key

: ISSUE-URL              HOST "/browse/" CONCAT SWAP CONCAT ;   # ( key -- url )
: JQL-URL                HOST "/issues/?jql=" CONCAT SWAP URL-ENCODE CONCAT ;   # ( jql -- url )

["_key"] VARIABLES
: PARENT-KEY             (_key !) ["key=" _key @] CONCAT ["parent"] SEARCH 0 NTH "parent" REC@;

[
    "DEPENDENCY" "ACTION-ITEM" "CLONERS" "DUPLICATE" "ISSUE-SPLIT" "RELATED" "REQUIRE"
    "ISSUE-URL" "JQL-URL"
    "PARENT-KEY"
] EXPORT
'''
