import json
import html
import markdown
from ..module import Module
import random
from ..interfaces import IInterpreter
from ...utils.errors import (
    HtmlModuleError,
    InvalidForthicWordError
)
from typing import List, Dict, Optional


ASYNC_BUTTON_KEY = '_async_forthic_button_state'


class HtmlModule(Module):
    """This implements basic rendering of HTML via Forthic

    NOTE: For more sophisticated template-based rendering, see the `jinja_module`.

    See `docs/modules/html_module.md` for detailed descriptions of each word.
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('html', interp, HTML_FORTHIC)
        self.add_module_word('ELEMENT', self.word_ELEMENT)
        self.add_module_word('RAW-HTML', self.word_RAW_HTML)
        self.add_module_word('<APPEND', self.word_l_APPEND)
        self.add_module_word('CHILD-NODES', self.word_CHILD_NODES)
        self.add_module_word('<INNER-HTML!', self.word_l_INNER_HTML_bang)
        self.add_module_word('<INNER-TEXT!', self.word_l_INNER_TEXT_bang)
        self.add_module_word('INNER-HTML', self.word_INNER_HTML)
        self.add_module_word('<INSERT-ADJ-HTML', self.word_l_INSERT_ADJ_HTML)
        self.add_module_word('<ATTR!', self.word_l_ATTR_bang)
        self.add_module_word('ATTR', self.word_ATTR)
        self.add_module_word('VALUE', self.word_VALUE)

        self.add_module_word('<ADD-CLASS', self.word_l_ADD_CLASS)
        self.add_module_word('<REMOVE-CLASS', self.word_l_REMOVE_CLASS)
        self.add_module_word('CLASSES', self.word_CLASSES)

        # Python only
        self.add_module_word('MARKDOWN>HTML', self.word_MARKDOWN_to_HTML)
        self.add_module_word('RENDER', self.word_RENDER)
        self.add_module_word('JS-PATH!', self.word_JS_PATH_bang)
        self.add_module_word('RUN-FORTHIC.JS', self.word_RUN_FORTHIC_JS)
        self.add_module_word('FORTHIC-BUTTON', self.word_FORTHIC_BUTTON)

        self.add_module_word('ASYNC-FORTHIC-BUTTON', self.word_ASYNC_FORTHIC_BUTTON)
        self.add_module_word('RUN-ASYNC-BUTTON', self.word_RUN_ASYNC_BUTTON)

        self.js_path = '/static/js/forthic/v2/'

    # ( type -- element )
    def word_ELEMENT(self, interp: IInterpreter):
        elem_type = interp.stack_pop()
        result = Element(elem_type)
        interp.stack_push(result)

    # ( string -- raw_html )
    def word_RAW_HTML(self, interp: IInterpreter):
        string = interp.stack_pop()
        result = RawHtml(string)
        interp.stack_push(result)

    # ( parent child -- parent )
    # ( parent child_items -- parent )
    def word_l_APPEND(self, interp: IInterpreter):
        child = interp.stack_pop()
        parent = interp.stack_pop()

        if isinstance(child, list):
            child_items = child
        else:
            child_items = [child]

        for item in child_items:
            parent.appendChild(item)
        interp.stack_push(parent)

    # ( element -- children )
    def word_CHILD_NODES(self, interp: IInterpreter):
        element = interp.stack_pop()
        result = element.getChildNodes()
        interp.stack_push(result)

    # ( element string -- element )
    def word_l_INNER_HTML_bang(self, interp: IInterpreter):
        string = interp.stack_pop()
        element = interp.stack_pop()
        element.setInnerHTML(string)
        interp.stack_push(element)

    # ( element string -- element )
    def word_l_INNER_TEXT_bang(self, interp: IInterpreter):
        string = interp.stack_pop()
        element = interp.stack_pop()
        element.setInnerText(string)
        interp.stack_push(element)

    # ( element -- string )
    def word_INNER_HTML(self, interp: IInterpreter):
        element = interp.stack_pop()
        result = element.getInnerHTML()
        interp.stack_push(result)

    # ( element string position -- element )
    # Position is one of: 'beforebegin', 'afterbegin', 'beforeend', 'afterend'
    def word_l_INSERT_ADJ_HTML(self, interp: IInterpreter):
        position = interp.stack_pop()
        string = interp.stack_pop()
        element = interp.stack_pop()
        element.insertAdjacentHTML(position, string)
        interp.stack_push(element)

    # ( element key val -- element )
    # ( element pairs -- element )
    def word_l_ATTR_bang(self, interp: IInterpreter):
        val = interp.stack_pop()
        if isinstance(val, list):
            pairs = val
        else:
            key = interp.stack_pop()
            pairs = [[key, val]]

        element = interp.stack_pop()

        for pair in pairs:
            element.setAttribute(pair[0], pair[1])
        interp.stack_push(element)

    # ( element attr -- val )
    def word_ATTR(self, interp: IInterpreter):
        key = interp.stack_pop()
        element = interp.stack_pop()
        result = element.getAttribute(key)
        interp.stack_push(result)

    # ( element -- val )
    def word_VALUE(self, interp: IInterpreter):
        element = interp.stack_pop()
        result = element.value
        interp.stack_push(result)

    # ( element class -- element )
    # ( element classes -- element )
    def word_l_ADD_CLASS(self, interp: IInterpreter):
        css_class = interp.stack_pop()
        element = interp.stack_pop()

        if isinstance(css_class, list):
            classes = css_class
        else:
            classes = [css_class]

        element.addClasses(classes)
        interp.stack_push(element)

    # ( element -- classes )
    def word_CLASSES(self, interp: IInterpreter):
        element = interp.stack_pop()
        result = element.getClasses()
        interp.stack_push(result)

    # ( element class -- element )
    # ( element classes -- element )
    def word_l_REMOVE_CLASS(self, interp: IInterpreter):
        css_class = interp.stack_pop()
        element = interp.stack_pop()

        if isinstance(css_class, list):
            classes = css_class
        else:
            classes = [css_class]
        element.removeClasses(classes)

        interp.stack_push(element)

    # ( markdown -- html)
    def word_MARKDOWN_to_HTML(self, interp: IInterpreter):
        markdown_content = interp.stack_pop()
        result = markdown.markdown(markdown_content)
        interp.stack_push(result)

    # ( element -- html )
    # ( elements -- html )
    def word_RENDER(self, interp: IInterpreter):
        element = interp.stack_pop()
        if isinstance(element, list):
            elements = element
        else:
            elements = [element]

        result = ''
        for e in elements:
            result += e.render()
        interp.stack_push(result)

    # ( path -- )
    def word_JS_PATH_bang(self, interp: IInterpreter):
        """Sets the URL path where the Forthic JS interpreter is"""
        path = interp.stack_pop()
        self.js_path = path

    # ( forthic -- script_element )
    def word_RUN_FORTHIC_JS(self, interp: IInterpreter):
        """Creates a script element that sets up a Forthic interpreter on the browser
        and runs a forthic string
        """
        forthic = interp.stack_pop()
        result = Element('script')
        result.setAttribute('type', 'module')
        random_str = random.uniform(0, 1)
        result.setInnerHTML(
            f'''
            import {{ Interpreter }} from "{self.js_path}/interpreter.mjs?version={random_str}";
            let interp = new Interpreter();
            interp.run(`{forthic}`)
            .then(() => {{
                window.FORTHIC_INTERP = interp
            }})'''
        )
        interp.stack_push(result)

    # ( id label forthic -- ForthicButton )
    def word_FORTHIC_BUTTON(self, interp: IInterpreter):
        forthic = interp.stack_pop()
        label = interp.stack_pop()
        html_id = interp.stack_pop()

        result = ForthicButton(interp, html_id, label, forthic)
        interp.stack_push(result)

    # ( id label forthic_word -- ForthicButton )
    def word_ASYNC_FORTHIC_BUTTON(self, interp: IInterpreter):
        forthic_word = interp.stack_pop()
        label = interp.stack_pop()
        html_id = interp.stack_pop()
        result = AsyncForthicButton(interp, html_id, label, forthic_word)
        interp.stack_push(result)

    # ( forthic button_id -- )
    def word_RUN_ASYNC_BUTTON(self, interp: IInterpreter):
        button_id = interp.stack_pop()
        forthic = interp.stack_pop()

        def get_button_states():
            interp.run(f"'{ASYNC_BUTTON_KEY}' cache.CACHE@ [] REC DEFAULT")
            res = interp.stack_pop()
            return res

        def store_button_states(button_id, state_info):
            """state_info is a dict with the following fields: state, Optional[message]"""
            button_states = get_button_states()
            button_states[button_id] = state_info
            interp.stack_push(button_states)
            interp.run(f"'{ASYNC_BUTTON_KEY}' cache.CACHE!")

        def is_running():
            button_states = get_button_states()
            state_info = button_states.get(button_id)
            if not state_info:
                state_info = {}
            state = state_info.get('state')
            res = state == 'RUNNING'
            return res

        if is_running():
            return

        try:
            store_button_states(button_id, {'state': 'RUNNING'})
            interp.run(forthic)
            store_button_states(button_id, {'state': ''})
        except Exception as e:
            store_button_states(button_id, {'state': 'ERROR', 'message': str(e)})


HTML_FORTHIC = '''
: COMMON-TYPES   ["H1" "H2" "H3" "H4" "H5" "H6"
                  "P" "UL" "OL" "LI"
                  "A" "SPAN"
                  "TABLE" "TR" "TH" "TD"
                  "DIV" "SECTION"
                  "STYLE" "IMG" "CANVAS"
                  "SCRIPT"
                 ] ;

[ "type" ] VARIABLES
: FDEFINE-ELEMENT (type !) [": " type @  " '" type @ "' ELEMENT ;"] CONCAT ;
COMMON-TYPES "FDEFINE-ELEMENT INTERPRET" FOREACH

: SVG   "svg" ELEMENT [["xmlns" "http://www.w3.org/2000/svg"] ["version" "1.1"]] <ATTR! ;

COMMON-TYPES EXPORT
["SVG"] EXPORT
'''


VOID_ELEMENTS = [
    'area',
    'base',
    'br',
    'col',
    'embed',
    'hr',
    'img',
    'input',
    'link',
    'meta',
    'param',
    'source',
    'track',
    'wbr',
]

VALID_POSITIONS = ['beforebegin', 'afterbegin', 'beforeend', 'afterend']


class Element:
    def __init__(self, elem_type: str):
        self.tagName = elem_type.upper()
        self.childNodes: List[Element] = []
        self.attributes: Dict[str, str] = {}
        self.beforeBegin: str = ''
        self.afterEnd: str = ''
        self.innerHTML: Optional[str] = None

    def appendChild(self, item: 'Element'):
        self.childNodes.append(item)

    def getChildNodes(self) -> List['Element']:
        return self.childNodes

    def setInnerHTML(self, string: str):
        self.childNodes = []
        self.innerHTML = string

    def setInnerText(self, string: str):
        self.setInnerHTML(html.escape(string))

    def getInnerHTML(self) -> str:
        if self.innerHTML is not None:
            return self.innerHTML

        result = ''
        for child in self.childNodes:
            result += child.render()
        return result

    def insertAdjacentHTML(self, position: str, string: str):
        if position == 'beforebegin':
            self.beforeBegin += string
        elif position == 'afterbegin':
            raw_items: List[Element] = [RawHtml(string)]
            self.childNodes = raw_items + self.childNodes
        elif position == 'beforeend':
            self.childNodes.append(RawHtml(string))
        elif position == 'afterend':
            self.afterEnd += string
        else:
            raise HtmlModuleError(f'Unhandled position: {position}')

    def getAttribute(self, key: str) -> str:
        result = self.attributes.get(key)
        if result is None:
            result = ''
        return result

    def setAttribute(self, key, val: Optional[str] = None):
        if val is None:
            del self.attributes[key]
            return
        self.attributes[key] = val

    def addClasses(self, classes: List[str]):
        element_classes = self.getClasses()
        for item in classes:
            if item not in element_classes:
                element_classes.append(item)
        self.setClasses(element_classes)

    def getClasses(self) -> List[str]:
        class_string = self.attributes.get('class')
        if not class_string:
            return []
        result = class_string.strip().split(' ')
        return result

    def setClasses(self, classes: List[str]):
        class_string = ' '.join(classes)
        self.attributes['class'] = class_string

    def removeClasses(self, classes: List[str]):
        element_classes = self.getClasses()
        remaining_classes = []
        for item in element_classes:
            if item not in classes:
                remaining_classes.append(item)
        self.setClasses(remaining_classes)

    def render(self):
        def get_attr_string() -> str:
            keys = sorted(self.attributes.keys())
            fragments = []
            for key in keys:
                fragment = f'{key}="{self.attributes[key]}"'
                if self.attributes[key] is None:
                    fragment = key
                fragments.append(fragment)
            res = ' '.join(fragments)
            if res != '':
                res = ' ' + res
            return res

        tag = self.tagName.lower()
        attributes = get_attr_string()

        if tag in VOID_ELEMENTS:
            result = f'<{tag}{attributes}>'
        else:
            result = self.beforeBegin
            result += f'<{tag}{attributes}>'
            result += self.getInnerHTML()
            result += f'</{tag}>'
            result += self.afterEnd
        return result


class RawHtml(Element):
    def __init__(self, string: str):
        self.html = string

    def render(self) -> str:
        return self.html


class ForthicButton:
    def __init__(self, interp: IInterpreter, html_id: str, label: str, forthic: str):
        self.html_id = html_id
        self.label = label
        self.forthic = forthic

        self.options = {
            'reload_page': False,
            'post_data_ids': None,
            'confirmable': False,
        }

    def __getitem__(self, key: str) -> Optional[bool]:
        result = self.options.get(key)
        return result

    def __setitem__(self, key: str, value: Optional[bool]):
        if key not in self.options:
            raise RuntimeError(f"Unknown ForthicButton option: '{key}'")
        self.options[key] = value

    def render(self) -> str:
        def get_done_code() -> str:
            if self.options['reload_page']:
                res = '''
                window.location.reload(true);
                '''
            else:
                res = '''
                $('#{html_id}').prop("disabled", false);
                alert("Done!");
                '''.format(
                    html_id=self.html_id
                )
            return res

        def get_confirm_code() -> str:
            res = 'true'
            if self.options['confirmable']:
                res = 'confirm("Are you sure?")'
            return res

        def make_func_gather_data() -> str:
            res = 'function gather_data() {\n'
            res += '    var fields = %s\n;' % json.dumps(
                self.options['post_data_ids']
            )
            res += '    var res = {};\n'
            res += "    fields.forEach(f => res[f] = $('#' + f).val());\n"
            res += '    return res;\n'
            res += '}\n'
            return res

        def make_func_prepend_data() -> str:
            res = 'function prepend_data(forthic) {\n'
            if self.options['post_data_ids']:
                res += make_func_gather_data()
                res += 'var data = gather_data();\n'
                res += "var res = `'${JSON.stringify(data)}' ${forthic}`;\n"
            else:
                res += 'var res = forthic;\n'
            res += '    return res;\n'
            res += '}\n'
            return res

        result = '''
        <button id='{html_id}' type='button'>{label}</button>
        <script>
            $('#{html_id}').click(function() {{
                function get_forthic_route() {{
                    var base = window.location.origin + window.location.pathname;
                    var end = base[base.length-1] == "/" ? "forthic" : "/forthic";
                    return base + end;
                }}

                {func_prepend_data}

                $('#{html_id}').prop("disabled", true);
                var proceed = {confirm_code};
                if (proceed) {{
                    $.ajax({{
                        type: "POST",
                        url: get_forthic_route(),
                        data: {{
                            'forthic': prepend_data('{forthic}')
                        }}
                    }}).done( function(data) {{
                        {done_code}
                    }}).fail( function(xhr, status, error) {{
                        console.error(xhr.responseText);
                        let error_text = JSON.parse(xhr.responseText).split("\\n")[0];
                        alert("Problem executing action. " + error_text);
                        $('#{html_id}').prop("disabled", false);
                    }});
                }}
            }});
        </script>
        '''.format(
            html_id=self.html_id,
            label=self.label,
            forthic=self.forthic,
            done_code=get_done_code(),
            confirm_code=get_confirm_code(),
            func_prepend_data=make_func_prepend_data(),
        )
        return result


class AsyncForthicButton:
    def __init__(self, interp: IInterpreter, html_id: str, label: str, forthic: str):
        self.html_id = html.escape(html_id)
        self.label = label
        self.forthic = forthic
        self.interp = interp

        # Ensure that `forthic` is just a Forthic word
        if ' ' in forthic or "'" in forthic or '"' in forthic:
            raise InvalidForthicWordError(forthic)

        self.options = {
            'reload_page': False,
            'post_data_ids': None,
            'confirmable': False,
        }

    def __getitem__(self, key: str) -> Optional[bool]:
        result = self.options.get(key)
        return result

    def __setitem__(self, key: str, value: Optional[bool]):
        if key not in self.options:
            raise RuntimeError(f"Unknown AsyncForthicButton option: '{key}'")
        self.options[key] = value

    def get_async_state(self) -> Dict[str, str]:
        self.interp.run(f"'{ASYNC_BUTTON_KEY}' cache.CACHE@")
        button_states = self.interp.stack_pop()
        if button_states is None:
            button_states = {}
        result = button_states.get(self.html_id)
        if not result:
            result = {}
        return result

    def render(self) -> str:
        def get_done_code() -> str:
            if self.options['reload_page']:
                res = '''
                window.location.reload(true);
                '''
            else:
                res = '''
                $('#{html_id}').prop("disabled", false);
                alert("Done!");
                '''.format(
                    html_id=self.html_id
                )
            return res

        def get_confirm_code() -> str:
            res = 'true'
            if self.options['confirmable']:
                res = 'confirm("Are you sure?")'
            return res

        def make_func_gather_data() -> str:
            res = 'function gather_data() {\n'
            res += '    var fields = %s\n;' % json.dumps(
                self.options['post_data_ids']
            )
            res += '    var res = {};\n'
            res += "    fields.forEach(f => res[f] = $('#' + f).val());\n"
            res += '    return res;\n'
            res += '}\n'
            return res

        def make_func_prepend_data() -> str:
            res = 'function prepend_data(forthic) {\n'
            if self.options['post_data_ids']:
                res += make_func_gather_data()
                res += 'var data = gather_data();\n'
                res += "var res = `'${JSON.stringify(data)}' ${forthic}`;\n"
            else:
                res += 'var res = forthic;\n'
            res += '    return res;\n'
            res += '}\n'
            return res

        async_state = self.get_async_state()

        result = f'''
        <style>
        .busy-indicator {{
            display: inline-block;
            margin-right: 11px;
            animation: rotation 1.5s infinite 0.5s;
            font-size: 17px;
            margin-left: 10px;
        }}
        @keyframes rotation {{
            from {{
                transform: rotate(0deg);
            }}
            to {{
                transform: rotate(359deg);
            }}
        }}
        </style>
        <button id='{self.html_id}' type='button'>{self.label}</button>
        <p id='{self.html_id}-busy' style='display: none;'><span class='busy-indicator'>&mid;</span>RUNNING...</p>
        <p id='{self.html_id}-error' style='display: none;'>{async_state.get("message")}</p>
        <script>
            (function () {{
                function get_forthic_route() {{
                    var base = window.location.origin + window.location.pathname;
                    var end = base[base.length-1] == "/" ? "forthic" : "/forthic";
                    return base + end;
                }}

                $('#{self.html_id}').click(function() {{
                    {make_func_prepend_data()}

                    $('#{self.html_id}').prop("disabled", true);
                    var proceed = {get_confirm_code()};
                    if (proceed) {{
                        $.ajax({{
                            type: "POST",
                            url: get_forthic_route(),
                            data: {{
                                'forthic': JSON.stringify(prepend_data('{self.forthic}')) + " '{self.html_id}' {{html RUN-ASYNC-BUTTON}}"
                            }}
                        }}).fail( function(xhr, status, error) {{
                            console.error(xhr.responseText);
                            let error_text = JSON.parse(xhr.responseText).split("\\n")[0];
                            alert("Problem executing action. " + error_text);
                            $('#{self.html_id}').prop("disabled", false);
                        }});
                        show_running_state();
                        setTimeout(check_state_periodically, 3000);
                    }}
                }});

                function show_normal_state() {{
                    $('#{self.html_id}').prop("disabled", false);
                    $('#{self.html_id}').show();
                    $('#{self.html_id}-busy').hide();
                    $('#{self.html_id}-error').hide();
                }}

                function show_running_state() {{
                    $('#{self.html_id}').hide();
                    $('#{self.html_id}-busy').show();
                    $('#{self.html_id}-error').hide();
                }}

                function show_error_state() {{
                    $('#{self.html_id}').show();
                    $('#{self.html_id}-busy').hide();
                    $('#{self.html_id}-error').show();
                }}

                function update_button_ui(state) {{
                    switch(state) {{
                        case '': case null:
                            show_normal_state();
                            break;

                        case 'RUNNING':
                            show_running_state();
                            break;

                        case 'ERROR':
                            show_error_state();
                            break;
                    }}
                }}

                function check_state_periodically() {{
                    $.ajax({{
                        type: "POST",
                        url: get_forthic_route(),
                        data: {{
                            'forthic': "'{ASYNC_BUTTON_KEY}' cache.CACHE@ '{self.html_id}' REC@ '' DEFAULT"
                        }}
                    }}).done( function(data) {{
                        let state_info = data.result;
                        let state = state_info.state;
                        update_button_ui(state);
                        switch (state) {{
                            case 'RUNNING':
                                setTimeout(check_state_periodically, 10000);
                                break;

                            case 'ERROR':
                                $('{self.html_id}-error').text(state_info.message);
                                break;

                            default:
                                {get_done_code()}
                                break;
                        }}
                    }}).fail( function(xhr, status, error) {{
                        console.error(xhr.responseText);
                        let error_text = JSON.parse(xhr.responseText).split("\\n")[0];
                        alert("Problem checking state. " + error_text);
                    }});
                }}

                // Update UI based on the current state
                let initial_state = '{async_state.get("state")}';
                update_button_ui(initial_state)

                if (initial_state == 'RUNNING') {{
                    check_state_periodically()
                }}
            }})()
        </script>
        '''
        return result
