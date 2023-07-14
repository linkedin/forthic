import re
import urllib
import requests
from ..module import Module
from ..interfaces import IInterpreter
from ...utils.errors import ConfluenceError
from typing import List, Optional

# Unit separator
US = chr(31)


class ConfluenceModule(Module):
    """This implements basic support to upsert wiki pages to Confluence

    See `docs/modules/confluence_module.md` for detailed descriptions of each word.
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('confluence', interp, CONFLUENCE_FORTHIC)
        self.context_stack: List['ConfluenceContext'] = []

        self.add_module_word('PUSH-CONTEXT!', self.word_PUSH_CONTEXT_bang)
        self.add_module_word('POP-CONTEXT!', self.word_POP_CONTEXT_bang)
        self.add_module_word('HOST', self.word_HOST)

        self.add_module_word('PAGE-INFO', self.word_PAGE_INFO)

        self.add_module_word('NBSP', self.word_NBSP)
        self.add_module_word('SPACES-WIDE', self.word_SPACES_WIDE)

        self.add_module_word('|ESCAPE-TABLE-CONTENT', self.word_pipe_ESCAPE_TABLE_CONTENT)
        self.add_module_word('|ESCAPE-NEWLINES', self.word_pipe_ESCAPE_NEWLINES)
        self.add_module_word('COLOR-BOX', self.word_COLOR_BOX)
        self.add_module_word('TABLE', self.word_TABLE)
        self.add_module_word('RENDER', self.word_RENDER)

        self.add_module_word('UPSERT-PAGE', self.word_UPSERT_PAGE)
        self.add_module_word('ADD-BLOG-POST', self.word_ADD_BLOG_POST)

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

    # ( space title -- page_info )
    def word_PAGE_INFO(self, interp: IInterpreter):
        context = self.current_context()
        title = interp.stack_pop()
        space = interp.stack_pop()

        encoded_title = urllib.parse.quote_plus(title)
        api_url = f'/wiki/cf/rest/api/content?title={encoded_title}&spaceKey={space}&expand=version'
        response = context.requests_get(api_url)

        if response.status_code != 200:
            raise ConfluenceError(
                f"Can't find '{title}' in space '{space}: {response.text}'"
            )
        data = response.json()

        if not data['results']:
            raise ConfluenceError(f"Can't find '{title}' in space '{space}'")

        result = data['results'][0]
        interp.stack_push(result)

    # ( -- nbsp_char )
    def word_NBSP(self, interp: IInterpreter):
        interp.stack_push('&nbsp;')

    # ( str num_spaces -- str )
    def word_SPACES_WIDE(self, interp: IInterpreter):
        """This forces a string to be num_spaces wide using &nbsp;"""
        num_spaces = interp.stack_pop()
        string = interp.stack_pop()

        # Count &nbsp; as one space
        num_nbsps = len(re.findall('&nbsp;', string))
        chars_to_subtract = 5 * num_nbsps
        string_len = len(string) - chars_to_subtract

        if string_len >= num_spaces:
            result = string
        else:
            spaces_to_add = num_spaces - string_len
            result = string + spaces_to_add * '&nbsp;'

        interp.stack_push(result)

    # ( str -- str )
    def word_pipe_ESCAPE_TABLE_CONTENT(self, interp: IInterpreter):
        """This escapes content that should be rendered into a wiki table cell.

        In particular, we convert newlines into "\\", *except* for  bulleted lists and numbered lists.
        We also remove the '|' character except in the case where it's used to specify a link
        """
        content = interp.stack_pop()
        result = escape_table_content(content)
        interp.stack_push(result)

    # ( str -- str )
    def word_pipe_ESCAPE_NEWLINES(self, interp: IInterpreter):
        content = interp.stack_pop()
        if not content:
            interp.stack_push(content)
            return
        content = content.strip()
        content = content.replace('\r', '')
        pieces = content.split('\n')
        result = r" \\ ".join(pieces)
        interp.stack_push(result)
        pass

    # ( color -- ColorBox )
    def word_COLOR_BOX(self, interp: IInterpreter):
        color = interp.stack_pop()
        result = ColorBox(color)
        interp.stack_push(result)

    # ( headers recs -- wiki_markup )
    def word_TABLE(self, interp: IInterpreter):
        recs = interp.stack_pop()
        headers = interp.stack_pop()

        def table_heading():
            interp.run("[ ''")
            for h in headers:
                interp.stack_push(h)
            interp.run("'' ] '||' JOIN")

        def table_row(rec):
            interp.run("[ ''")
            for h in headers:
                value = rec.get(h)
                if not value:
                    value = ''
                interp.stack_push(value)
            interp.run("'' ] '|' JOIN")

        # Assemble table
        interp.run('[')
        table_heading()
        for r in recs:
            table_row(r)
        interp.run(']')
        interp.run('/N JOIN')

    # ( object -- html/wiki )
    def word_RENDER(self, interp: IInterpreter):
        obj = interp.stack_pop()
        if isinstance(obj, str):
            result = obj
        else:
            result = obj.render()
        interp.stack_push(result)

    # ( space parent_title title content -- )
    def word_UPSERT_PAGE(self, interp: IInterpreter):
        context = self.current_context()

        content = interp.stack_pop()
        title = interp.stack_pop()
        parent_title = interp.stack_pop()
        space = interp.stack_pop()
        encoded_title = urllib.parse.quote_plus(title)

        def does_page_exist():
            api_url = f'/wiki/cf/rest/api/content?title={encoded_title}&spaceKey={space}&expand=ancestors'
            response = context.requests_get(api_url)
            data = response.json()
            if data['size'] == 0:
                return False

            page_info = data['results'][0]
            current_parent = page_info['ancestors'][-1]['title']
            if current_parent != parent_title:
                raise ConfluenceError(
                    f"'{title}' exists, but its current parent '{current_parent}' does not match the specified parent '{parent_title}'"
                )
            return True

        def get_page_info(page_title):
            interp.stack_push(space)
            interp.stack_push(page_title)
            interp.run('PAGE-INFO')
            res = interp.stack_pop()
            return res

        def create_page():
            parent_info = get_page_info(parent_title)
            parent_id = parent_info['id']
            request_data = {
                'type': 'page',
                'title': title,
                'ancestors': [{'id': parent_id}],
                'space': {'key': space},
                'body': {
                    'storage': {'value': content, 'representation': 'wiki'}
                },
            }
            api_url = '/wiki/cf/rest/api/content'
            response = context.requests_post(api_url, json=request_data)
            if response.status_code != 200:
                raise ConfluenceError(
                    f"Could not create page '{title}': {response.text}"
                )

        def get_version(page_info):
            version_info = page_info.get('version')
            if version_info:
                res = int(version_info['number'])
            else:
                res = 1
            return res

        def update_page():
            page_info = get_page_info(title)
            page_id = page_info['id']
            version = get_version(page_info)

            request_data = {
                'id': page_id,
                'type': 'page',
                'title': title,
                'space': {'key': space},
                'body': {
                    'storage': {'value': content, 'representation': 'wiki'}
                },
                'version': {'number': version + 1},
            }

            api_url = f'/wiki/cf/rest/api/content/{page_id}'
            response = context.requests_put(api_url, json=request_data)

            if response.status_code != 200:
                raise ConfluenceError(
                    f"Could not update page '{title}': {response.text}"
                )

        # Do the upsert
        if does_page_exist():
            update_page()
        else:
            create_page()

    # NOTE: This has not been officially released yet and is subject to change
    # ( space title content labels -- )
    def word_ADD_BLOG_POST(self, interp: IInterpreter):
        context = self.current_context()

        labels = interp.stack_pop()
        content = interp.stack_pop()
        title = interp.stack_pop()
        space = interp.stack_pop()

        def make_record_label(label):
            return {
                "prefix": "global",
                "name": label
            }

        if labels:
            label_records = [make_record_label(label) for label in labels]
        else:
            label_records = None

        def create_post():
            request_data = {
                'type': 'blogpost',
                'title': title,
                'space': {'key': space},
                'body': {
                    'storage': {'value': content, 'representation': 'wiki'}
                }
            }
            api_url = '/wiki/cf/rest/api/content'
            response = context.requests_post(api_url, json=request_data)
            if response.status_code != 200:
                raise ConfluenceError(
                    f"Could not create post '{title}': {response.text}"
                )

            # Add labels
            if label_records:
                page_id = response.json()["id"]
                label_api_url = f'/wiki/cf/rest/api/content/{page_id}/label'
                response = context.requests_post(label_api_url, json=label_records)
                if response.status_code != 200:
                    raise ConfluenceError(
                        f"Could not add labels to blog post '{title}': {response.text}"
                    )
            return

        create_post()
        return

    def current_context(self):
        if not self.context_stack:
            raise ConfluenceError(
                'Use confluence.PUSH-CONTEXT! to provide a Confluence context'
            )

        result = self.context_stack[-1]
        return result


def escape_table_content(content):
    """This escapes content that should be rendered into a wiki table cell.

    In particular, we remove blank lines and we also remove the '|' character except in the case where it's
    used to specify a link
    """
    if not content:
        return ''

    def remove_blank_lines(s):
        s = s.strip()
        s = s.replace('\r', '')
        pieces = s.split('\n')
        non_blank_pieces = [p for p in pieces if p]
        res = "\n".join(non_blank_pieces)

        # If content is empty, return a space so the table cell doesn't collapse
        if not res:
            res = ' '
        return res

    def remove_pipes_if_needed(s):
        res = re.sub(
            r'\[(.*?)\|(.*?)\]', r'[\1%s\2]' % US, s
        )   # Replace pipes in links with US character
        res = re.sub(
            r'\|', '', res
        )                             # Remove all other pipes
        res = re.sub(
            US, '|', res
        )                               # Replace US chars with pipes again
        return res

    result = remove_blank_lines(content)
    result = remove_pipes_if_needed(result)
    return result


def raise_status_error_if_needed(response):
    if response.status_code < 400:
        return

    if response.status_code == 401:
        raise ConfluenceError("Unauthorized request. Please check your Confluence credentials.")
    else:
        raise ConfluenceError(response.text)


class ConfluenceContext:
    """Override this and pass to PUSH-CONTEXT! in order to make Confluence calls"""

    def requests_get(self, api_url: str):
        """Makes HTTP GET call to pull data"""
        api_url_w_host = self.get_host() + api_url
        result = requests.get(
            api_url_w_host,
            auth=(self.get_username(), self.get_password()),
            verify=self.get_cert_verify(),
        )
        raise_status_error_if_needed(result)
        return result

    def requests_post(self, api_url: str, json: Optional[str] = None):
        api_url_w_host = self.get_host() + api_url
        result = requests.post(
            api_url_w_host,
            auth=(self.get_username(), self.get_password()),
            json=json,
            verify=self.get_cert_verify(),
        )
        raise_status_error_if_needed(result)
        return result

    def requests_put(self, api_url: str, json: Optional[str] = None):
        api_url_w_host = self.get_host() + api_url
        result = requests.put(
            api_url_w_host,
            auth=(self.get_username(), self.get_password()),
            json=json,
            verify=self.get_cert_verify(),
        )
        raise_status_error_if_needed(result)
        return result

    def get_host(self):
        return None

    # Override this to supply the path to the cert file to use. Use False to skip verification
    def get_cert_verify(self):
        return False

    def get_username(self):
        return None

    def get_password(self):
        return None


CONFLUENCE_FORTHIC = '''
'''


class ColorBox():
    def __init__(self, color):
        self.color = color
        self.options = {
            "hover_text": ''
        }
        return

    def __getitem__(self, key: str) -> Optional[bool]:
        result = self.options.get(key)
        return result

    def __setitem__(self, key: str, value: Optional[bool]):
        if key not in self.options:
            raise RuntimeError(f"Unknown ColorBox option: '{key}'. Must be one of {self.options.keys()}")
        self.options[key] = value

    def render(self):
        result = '{html}<table style="margin:0px auto; display:inline-block; margin-left:-6px;">'
        result += '   <tbody style="">'
        result += '      <tr style="">'
        result += f'''      <td title="{self.options['hover_text']}" style="border:1px solid black; color:rgb(255,255,255);
                               vertical-align:middle; width:8px; overflow:hidden; height:25px;
                               background-color:{self.color}">'''
        result += '         </td> </tr> </tbody> </table>{html}'

        return result
