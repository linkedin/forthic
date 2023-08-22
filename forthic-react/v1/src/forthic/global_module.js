import React from 'react';
import {createBrowserRouter, Navigate} from "react-router-dom";
import axios from 'axios';
import ForthicPage from './elements/ForthicPage';
import { Module, Word, PushValueWord } from './module';
import { ensure_array, render_content_array } from './utils';
import { Typeahead, AsyncTypeahead } from 'react-bootstrap-typeahead';
import { CSVLink } from "react-csv"
import { UserNav, UserBreadcrumbNav, UserTypeahead } from './elements/UserNav'
import RecordsTable from './elements/RecordsTable'
import TicketsModal from './elements/TicketsModal'
import {TicketSelector} from './elements/TicketsModal'


// React Bootstrap
import Accordion from 'react-bootstrap/Accordion'
import AccordionButton from 'react-bootstrap/AccordionButton'
import Alert from 'react-bootstrap/Alert'
import Badge from 'react-bootstrap/Badge'
import Breadcrumb from 'react-bootstrap/Breadcrumb'
import Button from 'react-bootstrap/Button';
import ButtonGroup from 'react-bootstrap/ButtonGroup'
import ButtonToolbar from 'react-bootstrap/ButtonToolbar'
import Card from 'react-bootstrap/Card'
import Carousel from 'react-bootstrap/Carousel'
import CloseButton from 'react-bootstrap/CloseButton'
import Col from 'react-bootstrap/Col'
import Collapse from 'react-bootstrap/Collapse'
import Container from 'react-bootstrap/Container'
import Dropdown from 'react-bootstrap/Dropdown'
import DropdownButton from 'react-bootstrap/DropdownButton'
import Fade from 'react-bootstrap/Fade'
import Figure from 'react-bootstrap/Figure'
import FloatingLabel from 'react-bootstrap/FloatingLabel'
import Form from 'react-bootstrap/Form'
import Image from 'react-bootstrap/Image'
import InputGroup from 'react-bootstrap/InputGroup'
import ListGroup from 'react-bootstrap/ListGroup'
import Modal from 'react-bootstrap/Modal'
import Nav from 'react-bootstrap/Nav'
import Navbar from 'react-bootstrap/Navbar'
import NavDropdown from 'react-bootstrap/NavDropdown'
import NavItem from 'react-bootstrap/NavItem'
import NavLink from 'react-bootstrap/NavLink'
import Offcanvas from 'react-bootstrap/Offcanvas'
import Overlay from 'react-bootstrap/Overlay'
import OverlayTrigger from 'react-bootstrap/OverlayTrigger'
import Pagination from 'react-bootstrap/Pagination'
import Placeholder from 'react-bootstrap/Placeholder'
import Popover from 'react-bootstrap/Popover'
import ProgressBar from 'react-bootstrap/ProgressBar'
import Ratio from 'react-bootstrap/Ratio'
import Row from 'react-bootstrap/Row'
import Spinner from 'react-bootstrap/Spinner'
import SplitButton from 'react-bootstrap/SplitButton'
import Stack from 'react-bootstrap/Stack'
import Tab from 'react-bootstrap/Tab'
import Table from 'react-bootstrap/Table'
import Tabs from 'react-bootstrap/Tabs'
import Toast from 'react-bootstrap/Toast'
import Tooltip from 'react-bootstrap/Tooltip'

const REACT_BOOTSTRAP_NAME_TO_ELEMENT = {
    Accordion,
    AccordionButton,
    Alert,
    Badge,
    Breadcrumb,
    Button,
    ButtonGroup,
    ButtonToolbar,
    Card,
    Carousel,
    CloseButton,
    Col,
    Collapse,
    Container,
    Dropdown,
    "Dropdown.Toggle": Dropdown.Toggle,
    "Dropdown.Item": Dropdown.Item,
    "Dropdown.Divider": Dropdown.Divider,
    DropdownButton,
    Fade,
    Figure,
    FloatingLabel,
    Form,
    "Form.Group": Form.Group,
    "Form.Label": Form.Label,
    "Form.Control": Form.Control,
    "Form.Text": Form.Text,
    "Form.Check": Form.Check,
    "Form.Select": Form.Select,
    Image,
    InputGroup,
    ListGroup,
    Modal,
    Nav,
    Navbar,
    "Navbar.Brand": Navbar.Brand,
    "Navbar.Collapse": Navbar.Collapse,
    NavDropdown,
    "NavDropdown.Item": NavDropdown.Item,
    "NavDropdown.Divider": NavDropdown.Divider,
    NavItem,
    NavLink,
    Offcanvas,
    Overlay,
    OverlayTrigger,
    Pagination,
    Placeholder,
    Popover,
    ProgressBar,
    Ratio,
    Row,
    Spinner,
    SplitButton,
    Stack,
    Tab,
    Table,
    Tabs,
    Toast,
    Tooltip
}


const NAME_TO_ELEMENT = {
    ...REACT_BOOTSTRAP_NAME_TO_ELEMENT,
    Typeahead,
    AsyncTypeahead,
    CSVLink,
    Navigate,
    RecordsTable,
    TicketSelector,
    TicketsModal,
    UserBreadcrumbNav,
    UserTypeahead,
    UserNav
};


let DLE = String.fromCharCode(16);  // ASCII char for "Data Link Escape" used as an untypeable quote

class MessageBroker {
  constructor() {
    this.subscribers = {}
  }

  // Subscribes for a message, returning a subscription token
  //
  // `func` expects a message argument
  subscribe(func) {
    let subscription = Object.keys(this.subscribers).length + 1
    this.subscribers[subscription] = func
    return subscription
  }

  publish(message) {
    for (const subscription of Object.keys(this.subscribers)) {
      this.subscribers[subscription](message)
    }
  }

  unsubscribe(subscription) {
    delete this.subscribers[subscription]
  }
}

// NOTE: To add CSRF, redefine this function to look up your CSRF token from the cookies
function get_csrf_token() {
    return "LOOK-UP-FROM-CSRF-COOKIE"
}

const csrf_axios = axios.create({
    headers: {'X-CSRFToken': get_csrf_token()}
});

// ----- MODULE FLAGS --------------------------------------------------------------------------------------------------
// Module Flags: These are all None but are settable for one-time use to change the behavior
// of module words
let FLAGS = {
    with_key: null,
    push_error: null,
    comparator: null,
    push_rest: null,
    depth: null,
    overwrite: null,
    delay: null
}

// ----- Set Timeout IDs -------------------------------------------------------------------------------------
// These are used to cancel setTimeout calls
let QPARAM_BANG_ID = null

// Retrieves current value of FLAGS and then clears flags
function get_flags() {
    let result = {...FLAGS}
    FLAGS = {}
    return result
}

class GlobalModule extends Module {
  constructor(interp) {
    super("<GLOBAL>", interp);
    this.literal_handlers = [
      this.to_bool,
      this.to_float,
      this.to_int,
      this.to_date,
      this.to_time,
    ];

    // --------------------
    // Base words
    this.add_module_word("VARIABLES", this.word_VARIABLES);
    this.add_module_word("!", this.word_bang);
    this.add_module_word("@", this.word_at);
    this.add_module_word("!@", this.word_bang_at);
    this.add_module_word("INTERPRET", this.word_INTERPRET);

    this.add_module_word("EXPORT", this.word_EXPORT);
    this.add_module_word("USE-MODULES", this.word_USE_MODULES);
    this.add_module_word("REC", this.word_REC);
    this.add_module_word("REC@", this.word_REC_at);
    this.add_module_word("<REC!", this.word_l_REC_bang);

    // ----------------
    // Array/Record words
    this.add_module_word("APPEND", this.word_APPEND);
    this.add_module_word("REVERSE", this.word_REVERSE);
    this.add_module_word("UNIQUE", this.word_UNIQUE);
    this.add_module_word("<DEL", this.word_L_DEL);
    this.add_module_word("RELABEL", this.word_RELABEL);
    this.add_module_word("BY-FIELD", this.word_BY_FIELD);
    this.add_module_word("GROUP-BY-FIELD", this.word_GROUP_BY_FIELD);
    this.add_module_word("GROUP-BY", this.word_GROUP_BY);
    this.add_module_word("GROUPS-OF", this.word_GROUPS_OF);
    this.add_module_word("INDEX", this.word_INDEX);
    this.add_module_word("MAP", this.word_MAP);
    this.add_module_word("FOREACH", this.word_FOREACH);
    this.add_module_word("INVERT-KEYS", this.word_INVERT_KEYS);
    this.add_module_word("ZIP", this.word_ZIP);
    this.add_module_word("ZIP-WITH", this.word_ZIP_WITH);
    this.add_module_word("KEYS", this.word_KEYS);
    this.add_module_word("VALUES", this.word_VALUES);
    this.add_module_word("LENGTH", this.word_LENGTH);
    this.add_module_word("RANGE", this.word_RANGE);
    this.add_module_word("SLICE", this.word_SLICE);
    this.add_module_word("DIFFERENCE", this.word_DIFFERENCE);
    this.add_module_word("INTERSECTION", this.word_INTERSECTION);
    this.add_module_word("UNION", this.word_UNION);
    this.add_module_word("SELECT", this.word_SELECT);
    this.add_module_word("TAKE", this.word_TAKE);
    this.add_module_word("DROP", this.word_DROP);
    this.add_module_word("ROTATE", this.word_ROTATE);
    this.add_module_word("ROTATE-ELEMENT", this.word_ROTATE_ELEMENT);
    this.add_module_word("ARRAY?", this.word_ARRAY_q);

    this.add_module_word("SHUFFLE", this.word_SHUFFLE);
    this.add_module_word("SORT", this.word_SORT);
    this.add_module_word("FIELD-KEY-FUNC", this.word_FIELD_KEY_FUNC);
    this.add_module_word("NTH", this.word_NTH);
    this.add_module_word("LAST", this.word_LAST);
    this.add_module_word("UNPACK", this.word_UNPACK);
    this.add_module_word("FLATTEN", this.word_FLATTEN);
    this.add_module_word("KEY-OF", this.word_KEY_OF);
    this.add_module_word("REDUCE", this.word_REDUCE);

    // --------------------
    // Stack words
    this.add_module_word("POP", this.word_POP);
    this.add_module_word("DUP", this.word_DUP);
    this.add_module_word("SWAP", this.word_SWAP);

    // --------------------
    // String words
    this.add_module_word(">STR", this.word_to_STR);
    this.add_module_word("CONCAT", this.word_CONCAT);
    this.add_module_word("SPLIT", this.word_SPLIT);
    this.add_module_word("JOIN", this.word_JOIN);
    this.add_module_word("/N", this.word_slash_N);
    this.add_module_word("/R", this.word_slash_R);
    this.add_module_word("/T", this.word_slash_T);
    this.add_module_word("LOWERCASE", this.word_LOWERCASE);
    this.add_module_word("UPPERCASE", this.word_UPPERCASE);
    this.add_module_word("ASCII", this.word_ASCII);
    this.add_module_word("STRIP", this.word_STRIP);
    this.add_module_word("REPLACE", this.word_REPLACE);
    this.add_module_word("RE-MATCH", this.word_RE_MATCH);
    this.add_module_word("RE-MATCH-GROUP", this.word_RE_MATCH_GROUP);
    this.add_module_word("RE-MATCH-ALL", this.word_RE_MATCH_ALL);

    // --------------------
    // Misc words
    this.add_module_word("NULL", this.word_NULL);
    this.add_module_word("DEFAULT", this.word_DEFAULT);
    this.add_module_word("*DEFAULT", this.word_star_DEFAULT);
    this.add_module_word("REC-DEFAULTS", this.word_REC_DEFAULTS);
    this.add_module_word("<REPEAT", this.word_l_REPEAT);
    this.add_module_word("IDENTITY", this.word_IDENTITY);
    this.add_module_word(">FIXED", this.word_to_FIXED);
    this.add_module_word(">JSON", this.word_to_JSON);
    this.add_module_word("JSON>", this.word_JSON_to);
    this.add_module_word(".s", this.word_dot_s);

    // --------------------
    // Date/time words
    this.add_module_word("AM", this.word_AM);
    this.add_module_word("PM", this.word_PM);
    this.add_module_word("NOW", this.word_NOW);
    this.add_module_word(">TIME", this.word_to_TIME);
    this.add_module_word("TIME>STR", this.word_TIME_to_STR);
    this.add_module_word(">DATE", this.word_to_DATE);
    this.add_module_word("TODAY", this.word_TODAY);
    this.add_module_word("MONDAY", this.word_MONDAY);
    this.add_module_word("TUESDAY", this.word_TUESDAY);
    this.add_module_word("WEDNESDAY", this.word_WEDNESDAY);
    this.add_module_word("THURSDAY", this.word_THURSDAY);
    this.add_module_word("FRIDAY", this.word_FRIDAY);
    this.add_module_word("SATURDAY", this.word_SATURDAY);
    this.add_module_word("SUNDAY", this.word_SUNDAY);
    this.add_module_word("ADD-DAYS", this.word_ADD_DAYS);
    this.add_module_word("SUBTRACT-DATES", this.word_SUBTRACT_DATES);
    this.add_module_word("DATE>STR", this.word_DATE_to_STR);
    this.add_module_word("DATE-TIME>DATETIME", this.word_DATE_TIME_to_DATETIME);
    this.add_module_word("DATETIME>TIMESTAMP", this.word_DATETIME_to_TIMESTAMP);
    this.add_module_word("TIMESTAMP>DATETIME", this.word_TIMESTAMP_to_DATETIME);
    this.add_module_word("STR>DATETIME", this.word_STR_to_DATETIME);
    this.add_module_word("STR>TIMESTAMP", this.word_STR_to_TIMESTAMP);

    // --------------------
    // Math words
    this.add_module_word("+", this.word_plus);
    this.add_module_word("-", this.word_minus);
    this.add_module_word("*", this.word_times);
    this.add_module_word("/", this.word_divide_by);
    this.add_module_word("MOD", this.word_MOD);
    this.add_module_word("MEAN", this.word_MEAN);
    this.add_module_word("ROUND", this.word_ROUND);
    this.add_module_word("==", this.word_equal_equal);
    this.add_module_word("!=", this.word_not_equal);
    this.add_module_word(">", this.word_greater_than);
    this.add_module_word(">=", this.word_greater_than_or_equal);
    this.add_module_word("<", this.word_less_than);
    this.add_module_word("<=", this.word_less_than_or_equal);
    this.add_module_word("OR", this.word_OR);
    this.add_module_word("AND", this.word_AND);
    this.add_module_word("NOT", this.word_NOT);
    this.add_module_word("IN", this.word_IN);
    this.add_module_word("ANY", this.word_ANY);
    this.add_module_word("ALL", this.word_ALL);
    this.add_module_word(">BOOL", this.word_to_BOOL);
    this.add_module_word(">INT", this.word_to_INT);
    this.add_module_word(">FLOAT", this.word_to_FLOAT);
    this.add_module_word("BUCKET", this.word_BUCKET);
    this.add_module_word("UNIFORM-RANDOM", this.word_UNIFORM_RANDOM);
    this.add_module_word("RANGE-INDEX", this.word_RANGE_INDEX);

    // ----------------
    // Flag words
    this.add_module_word("!PUSH-ERROR", this.word_bang_PUSH_ERROR);
    this.add_module_word("!WITH-KEY", this.word_bang_WITH_KEY);
    this.add_module_word("!COMPARATOR", this.word_bang_COMPARATOR);
    this.add_module_word("!PUSH-REST", this.word_bang_PUSH_REST);
    this.add_module_word("!DEPTH", this.word_bang_DEPTH);
    this.add_module_word("!OVERWRITE", this.word_bang_OVERWRITE);
    this.add_module_word("!DELAY", this.word_bang_DELAY);

    // --------------------
    // React words
    this.add_module_word("Element", this.word_Element);
    this.add_module_word("<CONTENT", this.word_l_CONTENT);
    this.add_module_word("<PROPS", this.word_l_PROPS);
    this.add_module_word("<<CLASSNAME", this.word_ll_CLASSNAME);
    this.add_module_word("FCALLBACK", this.word_FCALLBACK);
    this.add_module_word("Route", this.word_Route);
    this.add_module_word("Router", this.word_Router);
    this.add_module_word("ForthicPage", this.word_ForthicPage);
    this.add_module_word("QPARAM", this.word_QPARAM);
    this.add_module_word("QPARAM!", this.word_QPARAM_bang);
    this.add_module_word("QPARAMS!", this.word_QPARAMS_bang);
    this.add_module_word("DEL-QPARAM!", this.word_DEL_QPARAM_bang);
    this.add_module_word("<QPARAMS", this.word_l_QPARAMS);
    this.add_module_word("console.log", this.word_console_log);
    this.add_module_word("window.open", this.word_window_open);
    this.add_module_word("TITLE!", this.word_TITLE_bang);
    this.add_module_word("SERVER-INTERPRET", this.word_SERVER_INTERPRET);
    this.add_module_word("MESSAGE-BROKER", this.word_MESSAGE_BROKER);
    this.add_module_word("PUBLISH-MESSAGE", this.word_PUBLISH_MESSAGE);

    // --------------------
    // Misc words (js-specific)
    this.add_module_word("URL-ENCODE", this.word_URL_ENCODE);
    this.add_module_word("URL-DECODE", this.word_URL_DECODE);
    this.add_module_word("QUOTE-CHAR", this.word_QUOTE_CHAR);
    this.add_module_word("QUOTED", this.word_QUOTED);

    // --------------------
    // Profiling words
    this.add_module_word("PROFILE-START", this.word_PROFILE_START);
    this.add_module_word("PROFILE-TIMESTAMP", this.word_PROFILE_TIMESTAMP);
    this.add_module_word("PROFILE-END", this.word_PROFILE_END);
    this.add_module_word("PROFILE-DATA", this.word_PROFILE_DATA);
  }

  find_word(name) {
    let result = super.find_word(name);
    if (!result) result = this.find_literal_word(name);
    return result;
  }

  find_literal_word(string) {
    let self = this;
    let value = null;
    for (let i = 0; i < self.literal_handlers.length; i++) {
      value = self.literal_handlers[i](string);
      if (value !== null) return new PushValueWord(string, value);
    }
    return null;
  }

  // =======================
  // Literal handlers
  to_bool(str_val) {
    if (str_val == "TRUE") return true;
    else if (str_val == "FALSE") return false;
    else return null;
  }

  to_int(str_val) {
    let result = parseInt(str_val);
    if (isNaN(result)) return null;
    if (result != str_val) return null;
    return result;
  }

  to_float(str_val) {
    if (str_val.indexOf(".") == -1) return null;
    let result = parseFloat(str_val);
    if (isNaN(result)) return null;
    else return result;
  }

  to_date(str_val) {
    let match = str_val.match(/(\d{4})-(\d{2})-(\d{2})/);
    if (!match) return null;
    let year = match[1];
    let month = match[2];
    let day = match[3];
    let result = new Date();
    result.setFullYear(year);
    result.setMonth(month - 1);
    result.setDate(day);
    return result;
  }

  to_time(str_val) {
    let match = str_val.match(/(\d{1,2}):(\d{2})/);
    if (!match) return null;

    let hours = match[1];
    let minutes = match[2];

    if (hours > 23) return null;
    if (minutes >= 60) return null;

    let result = new Date();
    result.setHours(hours);
    result.setMinutes(minutes);
    return result;
  }

  // =======================
  // Words

  // ( varnames -- )
  word_VARIABLES(interp) {
    let varnames = interp.stack_pop();
    let module = interp.cur_module();
    varnames.forEach((v) => {
      module.add_variable(v);
    });
  }

  // ( value variable -- )
  word_bang(interp) {
    let variable = interp.stack_pop();
    let value = interp.stack_pop();
    variable.value = value;
  }

  // ( variable -- value )
  word_at(interp) {
    let variable = interp.stack_pop();
    interp.stack_push(variable.value);
  }

  // ( value variable -- value )
  word_bang_at(interp) {
    let variable = interp.stack_pop();
    let value = interp.stack_pop();
    variable.value = value;
    interp.stack_push(variable.value);
  }

  // ( string -- )
  async word_INTERPRET(interp) {
    let string = interp.stack_pop();
    if (string) await interp.run(string);
  }

  // ( names -- )
  word_EXPORT(interp) {
    let names = interp.stack_pop();
    interp.cur_module().add_exportable(names);
  }

  // ( names -- )
  async word_USE_MODULES(interp) {
    let names = interp.stack_pop();

    let cur_module = interp.cur_module();
    if (cur_module != interp.app_module)
      throw "USE-MODULES can only be called within the app module";

    for (let i = 0; i < names.length; i++) {
      let name = names[i];
      let module_name = name;
      let prefix = name;
      if (name instanceof Array) {
        module_name = name[0];
        prefix = name[1];
      }

      let module = interp.find_module(module_name);
      await interp.app_module.import_module(prefix, module, interp);
    }
  }

  // ( key_vals -- rec )
  word_REC(interp) {
    let key_vals = interp.stack_pop();
    if (!key_vals) key_vals = [];
    let result = {};
    key_vals.forEach((pair) => {
      let key = null;
      let val = null;
      if (pair) {
        if (pair.length >= 1) key = pair[0];
        if (pair.length >= 2) val = pair[1];
      }
      result[key] = val;
    });
    interp.stack_push(result);
  }

  // ( rec field -- value )
  // ( rec fields -- value )
  word_REC_at(interp) {
    let field = interp.stack_pop();
    let rec = interp.stack_pop();

    if (!rec) {
      interp.stack_push(null);
      return;
    }

    let fields = [field];
    if (field instanceof Array) fields = field;
    let result = drill_for_value(rec, fields);
    interp.stack_push(result);
  }

  // ( rec value field -- rec )
  word_l_REC_bang(interp) {
    let field = interp.stack_pop();
    let value = interp.stack_pop();
    let rec = interp.stack_pop();

    if (!rec) rec = {};

    let fields = null;
    if (field instanceof Array) fields = field;
    else fields = [field];

    function ensure_field(rec, field) {
      let res = rec[field];
      if (res === undefined) {
        res = {};
        rec[field] = res;
      }
      return res;
    }

    let cur_rec = rec;
    // Drill down up until the last value
    for (let i = 0; i < fields.length - 1; i++) {
      cur_rec = ensure_field(cur_rec, fields[i]);
    }

    // Set the value at the right depth within rec
    cur_rec[fields[fields.length - 1]] = value;

    interp.stack_push(rec);
  }

  // ( array item -- array )
  // ( record key/val -- record )
  word_APPEND(interp) {
    let item = interp.stack_pop();
    let result = interp.stack_pop();

    if (!result) result = [];

    if (result instanceof Array) result.push(item);
    // If not a list, treat as record
    else result[item[0]] = item[1];

    interp.stack_push(result);
  }

  // ( array -- array )
  // ( record -- record )
  word_REVERSE(interp) {
    let result = interp.stack_pop();

    if (!result) {
      interp.stack_push(result);
      return;
    }

    if (result instanceof Array) result = result.reverse();

    interp.stack_push(result);
  }

  // ( array -- array )
  // ( record -- record )
  word_UNIQUE(interp) {
    let container = interp.stack_pop();

    if (!container) {
      interp.stack_push(container);
      return;
    }

    function invert_rec(rec) {
      let res = {};
      Object.keys(rec).forEach((k) => {
        res[rec[k]] = k;
      });
      return res;
    }

    let result = null;
    if (container instanceof Array) {
      let set = {};
      container.forEach((item) => {
        set[item] = true;
      });
      result = Object.keys(set);
    } else {
      result = invert_rec(invert_rec(container));
    }

    interp.stack_push(result);
  }

  // ( array index -- array )
  // ( record key -- record )
  word_L_DEL(interp) {
    let key = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) {
      interp.stack_push(container);
      return;
    }

    if (container instanceof Array) container.splice(key, 1);
    else delete container[key];
    interp.stack_push(container);
  }

  // ( array old_keys new_keys -- array )
  // ( record old_keys new_keys -- record )
  word_RELABEL(interp) {
    let new_keys = interp.stack_pop();
    let old_keys = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) {
      interp.stack_push(container);
      return;
    }

    if (old_keys.length != new_keys.length)
      throw "RELABEL: old_keys and new_keys must be same length";

    let new_to_old = {};
    for (let i = 0; i < old_keys.length; i++) {
      new_to_old[new_keys[i]] = old_keys[i];
    }

    let result = [];
    if (container instanceof Array) {
      Object.keys(new_to_old)
        .sort()
        .forEach((k) => result.push(container[new_to_old[k]]));
    } else {
      result = {};
      Object.keys(new_to_old).forEach(
        (k) => (result[k] = container[new_to_old[k]])
      );
    }

    interp.stack_push(result);
  }

  // ( array field -- field_to_item )
  // ( record field -- field_to_item )
  word_BY_FIELD(interp) {
    let field = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let values = null;
    if (container instanceof Array) {
      values = container;
    } else {
      values = [];
      Object.keys(container).forEach((k) => {
        values.push(container[k]);
      });
    }

    let result = {};
    values.forEach((v) => {
      if (v)   result[v[field]] = v;
    });

    interp.stack_push(result);
  }

  // ( array field -- field_to_items )
  // ( record field -- field_to_items )
  word_GROUP_BY_FIELD(interp) {
    let field = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let values = [];
    if (container instanceof Array) values = container;
    else values = Object.keys(container).map((k) => container[k]);

    let result = {};
    values.forEach((v) => {
      let field_val = v[field];
      if (field_val instanceof Array) {
        for (const fv of field_val) {
          if (!result[fv]) result[fv] = [];
          result[fv].push(v);
        }
      } else {
        if (!result[field_val]) result[field_val] = [];
        result[field_val].push(v);
      }
    });

    interp.stack_push(result);
  }

  // ( array forthic -- group_to_items )
  // ( record forthic -- group_to_items )
  async word_GROUP_BY(interp) {
    let forthic = interp.stack_pop();
    let container = interp.stack_pop();

    const flags = get_flags();

    if (!container) container = [];

    let keys, values;

    if (container instanceof Array) {
      keys = [];
      for (let i = 0; i < container.length; i++) keys.push(i);
      values = container;
    } else {
      keys = Object.keys(container);
      values = keys.map((k) => container[k]);
    }

    let result = {};
    for (let i = 0; i < values.length; i++) {
      let key = keys[i];
      let value = values[i];
      if (flags.with_key) interp.stack_push(key);
      interp.stack_push(value);
      await interp.run(forthic);
      let group = interp.stack_pop();
      if (!result[group]) result[group] = [];
      result[group].push(value);
    }

    interp.stack_push(result);
  }

  // ( array n -- arrays )
  // ( record n -- records )
  word_GROUPS_OF(interp) {
    let size = interp.stack_pop();
    let container = interp.stack_pop();
    if (size <= 0) throw "GROUPS-OF requires group size > 0";

    if (!container) container = [];

    function group_items(items, group_size) {
      let num_groups = Math.ceil(items.length / group_size);
      let res = [];
      let remaining = items.slice();
      for (let i = 0; i < num_groups; i++) {
        res.push(remaining.slice(0, group_size));
        remaining = remaining.slice(group_size);
      }

      return res;
    }

    function extract_rec(record, keys) {
      let res = {};
      keys.forEach((k) => (res[k] = record[k]));
      return res;
    }

    let result;
    if (container instanceof Array) {
      result = group_items(container, size);
    } else {
      let keys = Object.keys(container);
      let key_groups = group_items(keys, size);
      result = key_groups.map((ks) => extract_rec(container, ks));
    }

    interp.stack_push(result);
    return;
  }

  // ( array forthic -- record )
  async word_INDEX(interp) {
    const forthic = interp.stack_pop();
    const items = interp.stack_pop();

    if (!items) {
      interp.stack_push(items);
      return;
    }

    let result = {};
    for (let i = 0; i < items.length; i++) {
      let item = items[i];
      interp.stack_push(item);
      await interp.run(forthic);
      let keys = interp.stack_pop();
      keys.forEach((k) => {
        let lowercased_key = k.toLowerCase();
        if (result[lowercased_key]) result[lowercased_key].push(item);
        else result[lowercased_key] = [item];
      });
    }
    interp.stack_push(result);
  }

  // ( items forthic -- [ ? ] )
  async word_MAP(interp) {
    let forthic = interp.stack_pop();
    let items = interp.stack_pop();

    const flags = get_flags();
    let depth = flags.depth;
    if (!depth) depth = 0;

    if (!items) {
      interp.stack_push(items);
      return;
    }

    // This maps the forthic over an item, storing errors if needed
    async function map_value(key, value, errors) {
      if (flags.with_key) interp.stack_push(key);
      interp.stack_push(value);

      if (flags.push_error) {
        let error = null;
        try {
          // If this runs successfully, it would have pushed the result onto the stack
          await interp.run(forthic);
        }
        catch (e) {
          // Since this didn't run successfully, push null onto the stack
          interp.stack_push(null);
          error = e;
        }
        errors.push(error);
      }
      else {
        await interp.run(forthic);
      }
      return interp.stack_pop();
    }

    // This recursively descends a record structure
    async function descend_record(record, depth, accum, errors) {
      let keys = Object.keys(record);
      for (let i = 0; i < keys.length; i++) {
        const k = keys[i];
        const item = record[k];
        if (depth > 0) {
          if (item instanceof Array) {
            accum[k] = [];
            await descend_list(item, depth - 1, accum[k], errors);
          } else {
            accum[k] = {};
            await descend_record(item, depth - 1, accum[k], errors);
          }
        } else {
          accum[k] = await map_value(k, item, errors);
        }
      }

      return accum;
    }

    // This recursively descends a list
    async function descend_list(items, depth, accum, errors) {
      for (let i = 0; i < items.length; i++) {
        const item = items[i];
        if (depth > 0) {
          if (item instanceof Array) {
            accum.push([]);
            await descend_list(
              item,
              depth - 1,
              accum[accum.length - 1],
              errors
            );
          } else {
            accum.push({});
            await descend_record(
              item,
              depth - 1,
              accum[accum.length - 1],
              errors
            );
          }
        } else {
          accum.push(await map_value(i, item, errors));
        }
      }
      return accum;
    }

    let errors = [];
    let result;
    if (items instanceof Array)
      result = await descend_list(items, depth, [], errors);
    else result = await descend_record(items, depth, {}, errors);

    // Return results
    interp.stack_push(result);
    if (flags.push_error) interp.stack_push(errors);
  }

  // ( items word -- ? )
  async word_FOREACH(interp) {
    let forthic = interp.stack_pop();
    let items = interp.stack_pop();
    const flags = get_flags();

    if (!items) items = [];

    let errors = [];
    if (items instanceof Array) {
      for (let i = 0; i < items.length; i++) {
        let item = items[i];
        if (flags.with_key) interp.stack_push(i);
        interp.stack_push(item);
        if (flags.push_error)
          errors.push(await execute_returning_error(interp, forthic));
        else await interp.run(forthic);
      }
    } else {
      let keys = Object.keys(items);
      for (let i = 0; i < keys.length; i++) {
        let k = keys[i];
        let item = items[k];
        if (flags.with_key) interp.stack_push(k);
        interp.stack_push(item);
        if (flags.push_error)
          errors.push(await execute_returning_error(interp, forthic));
        else await interp.run(forthic);
      }
    }

    if (flags.push_error) interp.stack_push(errors);
  }

  // ( record -- record )
  word_INVERT_KEYS(interp) {
    let record = interp.stack_pop();
    let result = {};
    Object.keys(record).forEach((first_key) => {
      let sub_record = record[first_key];
      Object.keys(sub_record).forEach((second_key) => {
        let value = sub_record[second_key];
        if (!result[second_key]) result[second_key] = {};
        result[second_key][first_key] = value;
      });
    });
    interp.stack_push(result);
  }

  // ( array1 array2 -- array )
  // ( record1 record2 -- record )
  word_ZIP(interp) {
    let container2 = interp.stack_pop();
    let container1 = interp.stack_pop();

    if (!container1) container1 = [];
    if (!container2) container2 = [];

    let result;
    if (container2 instanceof Array) {
      result = [];
      for (let i = 0; i < container1.length; i++) {
        let value2 = null;
        if (i < container2.length) value2 = container2[i];
        result.push([container1[i], value2]);
      }
    } else {
      result = {};
      Object.keys(container1).forEach((k) => {
        let v = container1[k];
        result[k] = [v, container2[k]];
      });
    }

    interp.stack_push(result);
  }

  // ( array1 array2 forthic -- array )
  // ( record1 record2 forthic -- record )
  async word_ZIP_WITH(interp) {
    let forthic = interp.stack_pop();
    let container2 = interp.stack_pop();
    let container1 = interp.stack_pop();

    if (!container1) container1 = [];
    if (!container2) container2 = [];

    let result;
    if (container2 instanceof Array) {
      result = [];
      for (let i = 0; i < container1.length; i++) {
        let value2 = null;
        if (i < container2.length) value2 = container2[i];
        interp.stack_push(container1[i]);
        interp.stack_push(value2);
        await interp.run(forthic);
        let res = interp.stack_pop();
        result.push(res);
      }
    } else {
      result = {};
      let keys = Object.keys(container1);
      for (let i = 0; i < keys.length; i++) {
        let k = keys[i];
        interp.stack_push(container1[k]);
        interp.stack_push(container2[k]);
        await interp.run(forthic);
        let res = interp.stack_pop();
        result[k] = res;
      }
    }

    interp.stack_push(result);
  }

  // ( array -- array )
  // ( record -- array )
  word_KEYS(interp) {
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      result = [];
      for (let i = 0; i < container.length; i++) result.push(i);
    } else {
      result = Object.keys(container);
    }

    interp.stack_push(result);
  }

  // ( array -- array )
  // ( record -- array )
  word_VALUES(interp) {
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      result = container;
    } else {
      result = [];
      Object.keys(container).forEach((k) => result.push(container[k]));
    }

    interp.stack_push(result);
  }

  // ( array -- length )
  // ( record -- length )
  word_LENGTH(interp) {
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array || typeof container == "string") {
      result = container.length;
    } else {
      result = Object.keys(container).length;
    }

    interp.stack_push(result);
  }

  // ( array fstart fend -- indices )
  async word_RANGE(interp) {
    const fend = interp.stack_pop()
    const fstart = interp.stack_pop()
    const array = interp.stack_pop()

    if (!array)   array = []

    let start_found = false
    let end_found = false

    let start_index = null
    let end_index = null

    for (let index = 0; index < array.length; index++) {
        const item = array[index]

        if (!start_found) {
            interp.stack_push(item)
            await interp.run(fstart)
            start_found = interp.stack_pop()
            if (start_found)   {
                start_index = index
            }
        }

        if (start_found && !end_found) {
            interp.stack_push(item)
            await interp.run(fend)
            end_found = interp.stack_pop()
            if (end_found)   {
                end_index = index
                break
            }
        }
    }
    interp.stack_push([start_index, end_index])
  }

  // ( array start end -- array )
  // ( record start end -- record )
  word_SLICE(interp) {
    let end = Math.trunc(interp.stack_pop());
    let start = Math.trunc(interp.stack_pop());
    let container = interp.stack_pop();

    if (!container) container = [];

    let length;
    if (container instanceof Array) {
      length = container.length;
    } else {
      length = Object.keys(container).length;
    }

    function normalize_index(index) {
      let res = index;
      if (index < 0) res = index + length;
      return res;
    }

    start = normalize_index(start);
    end = normalize_index(end);

    let step = 1;
    if (start > end) step = -1;

    let indexes = [start];
    if (start < 0 || start >= length) indexes = [];

    while (start != end) {
      start = start + step;
      if (start < 0 || start >= length) indexes.push(null);
      else indexes.push(start);
    }

    let result;
    if (container instanceof Array) {
      result = [];
      indexes.forEach((i) => {
        if (i === null) result.push(null);
        else result.push(container[i]);
      });
    } else {
      let keys = Object.keys(container).sort();
      result = {};
      indexes.forEach((i) => {
        if (i !== null) {
          let k = keys[i];
          result[k] = container[k];
        }
      });
    }

    interp.stack_push(result);
  }

  // ( larray rarray -- array )
  // ( lrecord rrecord -- record )
  word_DIFFERENCE(interp) {
    let rcontainer = interp.stack_pop();
    let lcontainer = interp.stack_pop();

    if (!lcontainer) lcontainer = [];
    if (!rcontainer) rcontainer = [];

    function difference(l, r) {
      let res = [];
      l.forEach((item) => {
        if (r.indexOf(item) < 0) res.push(item);
      });
      return res;
    }

    let result;
    if (rcontainer instanceof Array) {
      result = difference(lcontainer, rcontainer);
    } else {
      let lkeys = Object.keys(lcontainer);
      let rkeys = Object.keys(rcontainer);

      let diff = difference(lkeys, rkeys);
      result = {};
      diff.forEach((k) => (result[k] = lcontainer[k]));
    }

    interp.stack_push(result);
  }

  // ( larray rarray -- array )
  // ( lrecord rrecord -- record )
  word_INTERSECTION(interp) {
    let rcontainer = interp.stack_pop();
    let lcontainer = interp.stack_pop();

    if (!lcontainer) lcontainer = [];
    if (!rcontainer) rcontainer = [];

    function intersection(l, r) {
      let res = [];
      l.forEach((item) => {
        if (r.indexOf(item) >= 0) res.push(item);
      });
      return res;
    }

    let result;
    if (rcontainer instanceof Array) {
      result = intersection(lcontainer, rcontainer);
    } else {
      let lkeys = Object.keys(lcontainer);
      let rkeys = Object.keys(rcontainer);

      let intersect = intersection(lkeys, rkeys);
      result = {};
      intersect.forEach((k) => (result[k] = lcontainer[k]));
    }
    interp.stack_push(result);
  }

  // ( larray rarray -- array )
  // ( lrecord rrecord -- record )
  word_UNION(interp) {
    let rcontainer = interp.stack_pop();
    let lcontainer = interp.stack_pop();

    if (!lcontainer) lcontainer = [];
    if (!rcontainer) rcontainer = [];

    function union(l, r) {
      let keyset = {};
      l.forEach((item) => {
        keyset[item] = true;
      });
      r.forEach((item) => {
        keyset[item] = true;
      });
      let res = Object.keys(keyset);
      return res;
    }

    let result;
    if (rcontainer instanceof Array) {
      result = union(lcontainer, rcontainer);
    } else {
      let lkeys = Object.keys(lcontainer);
      let rkeys = Object.keys(rcontainer);

      let keys = union(lkeys, rkeys);
      result = {};
      keys.forEach((k) => {
        let val = lcontainer[k];
        if (val === undefined) val = rcontainer[k];
        result[k] = val;
      });
    }

    interp.stack_push(result);
  }

  // ( larray forthic -- array )
  // ( lrecord forthic -- record )
  async word_SELECT(interp) {
    let forthic = interp.stack_pop();
    let container = interp.stack_pop();
    const flags = get_flags();

    if (!container) {
      interp.stack_push(container);
      return;
    }

    let result;
    if (container instanceof Array) {
      result = [];
      for (let i = 0; i < container.length; i++) {
        let item = container[i];
        if (flags.with_key) interp.stack_push(i);
        interp.stack_push(item);
        await interp.run(forthic);
        let should_select = interp.stack_pop();
        if (should_select) result.push(item);
      }
    } else {
      result = {};
      let keys = Object.keys(container);
      for (let i = 0; i < keys.length; i++) {
        let k = keys[i];
        let v = container[k];
        if (flags.with_key) interp.stack_push(k);
        interp.stack_push(v);
        await interp.run(forthic);
        let should_select = interp.stack_pop();
        if (should_select) result[k] = v;
      }
    }

    interp.stack_push(result);
  }

  // ( array n -- rest taken )
  // ( record n -- rest taken )
  word_TAKE(interp) {
    let n = interp.stack_pop();
    let container = interp.stack_pop();
    const flags = get_flags();

    if (!container) container = [];

    let rest, taken;
    if (container instanceof Array) {
      taken = container.slice(0, n);
      rest = container.slice(n);
    } else {
      let keys = Object.keys(container).sort();
      let taken_keys = keys.slice(0, n);
      let rest_keys = keys.slice(n);
      taken = taken_keys.map((k) => container[k]);
      rest = rest_keys.map((k) => container[k]);
    }

    interp.stack_push(taken);
    if (flags.push_rest) interp.stack_push(rest);
  }

  // ( array n -- array )
  // ( record n -- record )
  word_DROP(interp) {
    let n = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      result = container.slice(n);
    } else {
      let keys = Object.keys(container).sort();
      let rest_keys = keys.slice(n);
      result = rest_keys.map((k) => container[k]);
    }

    interp.stack_push(result);
  }

  // ( array  -- array )
  // ( record  -- record )
  word_ROTATE(interp) {
    let container = interp.stack_pop();

    let result;
    if (!container) {
      result = container;
    } else if (container instanceof Array) {
      result = container;
      if (container.length > 0) {
        let val = result.pop();
        result.unshift(val);
      }
    } else {
      result = container;
    }

    interp.stack_push(result);
  }

  // ( array element -- array )
  // ( record element -- record )
  // Moves element to front of array
  word_ROTATE_ELEMENT(interp) {
    let element = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      let index = container.indexOf(element);
      result = container;
      if (index > 0) {
        result.splice(index, 1);
        result.unshift(element);
      }
    } else {
      result = container;
    }

    interp.stack_push(result);
  }

    // ( val -- bool )
    word_ARRAY_q(interp) {
        let val = interp.stack_pop();
        let result = val instanceof Array
        interp.stack_push(result);
    }

  // ( array -- array )
  // ( record -- record )
  word_SHUFFLE(interp) {
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      // See: https://medium.com/@nitinpatel_20236/
      //    how-to-shuffle-correctly-shuffle-an-array-in-javascript-15ea3f84bfb
      result = container;
      for (let i = result.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * i);
        const temp = result[i];
        result[i] = result[j];
        result[j] = temp;
      }
    } else {
      result = container;
    }

    interp.stack_push(result);
  }


  // ( field -- key_func )
  word_FIELD_KEY_FUNC(interp) {
    let field = interp.stack_pop();

    function result(record) {
      return record[field];
    }

    interp.stack_push(result);
  }


  // ( array -- array )
  // ( record -- record )
  async word_SORT(interp) {
    let container = interp.stack_pop();
    let flags = get_flags();
    let comparator = flags["comparator"];

    if (!container) container = [];
    if (!(container instanceof Array)) {
      interp.stack_push(container);
      return;
    }

    // -----
    // Default sort
    function sort_without_comparator() {
      return container.sort();
    }

    // -----
    // Sort using a forthic string
    async function sort_with_forthic(forthic) {
      async function make_aug_array(vals) {
        let res = [];
        for (let i = 0; i < vals.length; i++) {
          let val = vals[i];
          interp.stack_push(val);
          await interp.run(forthic);
          let aug_val = interp.stack_pop();
          res.push([val, aug_val]);
        }
        return res;
      }

      function cmp_items(l, r) {
        let l_val = l[1];
        let r_val = r[1];

        if (l_val < r_val) return -1;
        else if (l_val > r_val) return 1;
        else return 0;
      }

      function de_aug_array(aug_vals) {
        let res = aug_vals.map((aug_val) => aug_val[0]);
        return res;
      }

      // Create an augmented array, sort it and then return the underlying values
      // NOTE: We're doing it this way because sort is synchronous
      let aug_array = await make_aug_array(container);
      aug_array.sort(cmp_items);
      return de_aug_array(aug_array);
    }

    // -----
    // Sort with key func
    function sort_with_key_func(key_func) {
        function cmp_items(l, r) {
            let l_val = key_func(l);
            let r_val = key_func(r);
            if (l_val < r_val)       return -1
            else if (l_val > r_val)  return 1
            else                     return 0
        }

        return container.sort(cmp_items);
    }


    // Figure out what to do
    let result;
    if (typeof comparator == "string") {
      result = await sort_with_forthic(comparator);
    } else if (comparator === undefined) {
      result = sort_without_comparator();
    } else {
        result = sort_with_key_func(comparator)
    }

    interp.stack_push(result);
  }

  // ( array n -- item )
  // ( record n -- value )
  word_NTH(interp) {
    let n = interp.stack_pop();
    let container = interp.stack_pop();

    if (n === null || !container) {
      interp.stack_push(null);
      return;
    }

    let result;
    if (container instanceof Array) {
      if (n < 0 || n >= container.length) {
        interp.stack_push(null);
        return;
      }
      result = container[n];
    } else {
      if (n < 0 || n >= Object.keys(container).length) {
        interp.stack_push(null);
        return;
      }
      let keys = Object.keys(container).sort();
      let key = keys[n];
      result = container[key];
    }

    interp.stack_push(result);
  }

  // ( array -- item )
  // ( record -- value )
  word_LAST(interp) {
    let container = interp.stack_pop();

    if (!container) {
      interp.stack_push(null);
      return;
    }

    let result;
    if (container instanceof Array) {
      if (container.length == 0) result = null;
      else result = container[container.length - 1];
    } else {
      let keys = Object.keys(container).sort();
      if (keys.length == 0) result = null;
      else result = container[keys[keys.length - 1]];
    }

    interp.stack_push(result);
  }

  // ( array -- a1 a2 .. an )
  // ( record -- v1 v2 .. vn )
  word_UNPACK(interp) {
    let container = interp.stack_pop();

    if (!container) container = [];

    if (container instanceof Array) {
      container.forEach((item) => {
        interp.stack_push(item);
      });
    } else {
      let keys = Object.keys(container).sort();
      keys.forEach((k) => {
        interp.stack_push(container[k]);
      });
    }
  }

  // ( array -- array )
  // ( record -- record )
  word_FLATTEN(interp) {
    let nested = interp.stack_pop();
    const flags = get_flags();

    if (!nested) nested = [];
    let depth = flags.depth;

    function fully_flatten_array(items, accum) {
      for (let i = 0; i < items.length; i++) {
        const item = items[i];
        if (item instanceof Array) fully_flatten_array(item, accum);
        else accum.push(item);
      }
      return accum;
    }

    function flatten_array(items, depth, accum = []) {
      if (depth === undefined) return fully_flatten_array(items, accum);
      for (let i = 0; i < items.length; i++) {
        const item = items[i];
        if (depth > 0 && item instanceof Array)
          flatten_array(item, depth - 1, accum);
        else accum.push(item);
      }
      return accum;
    }

    function is_record(obj) {
      let keys = Object.keys(obj);
      return keys.length > 0;
    }

    function add_to_record_result(item, key, keys, result) {
      let new_key = keys.concat([key]).join(".");
      result[new_key] = item;
    }

    function fully_flatten_record(record, res, keys) {
      let record_keys = Object.keys(record);
      for (const k of record_keys) {
        let item = record[k];
        if (is_record(item)) fully_flatten_record(item, res, keys.concat([k]));
        else add_to_record_result(item, k, keys, res);
      }
      return res;
    }

    function flatten_record(record, depth, res, keys) {
      if (depth === undefined) return fully_flatten_record(record, res, keys);

      let record_keys = Object.keys(record);
      for (const k of record_keys) {
        let item = record[k];
        if (depth > 0 && is_record(item))
          flatten_record(item, depth - 1, res, keys.concat([k]));
        else add_to_record_result(item, k, keys, res);
      }
      return res;
    }

    let result;
    if (nested instanceof Array) {
      result = flatten_array(nested, depth);
    } else {
      result = flatten_record(nested, depth, {}, []);
    }

    interp.stack_push(result);
    return;
  }

  // ( array item -- index )
  // ( record item -- key )
  word_KEY_OF(interp) {
    let item = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      let index = container.indexOf(item);
      if (index < 0) result = null;
      else result = index;
    } else {
      result = null;
      let keys = Object.keys(container);
      for (let i = 0; i < keys.length; i++) {
        let k = keys[i];
        let v = container[k];
        if (v == item) {
          result = k;
          break;
        }
      }
    }

    interp.stack_push(result);
    return;
  }

  // ( array init forthic -- value )
  // ( record init forthic -- value )
  async word_REDUCE(interp) {
    let forthic = interp.stack_pop();
    let initial = interp.stack_pop();
    let container = interp.stack_pop();

    if (!container) container = [];

    let result;
    if (container instanceof Array) {
      interp.stack_push(initial);
      container.forEach(async (item) => {
        interp.stack_push(item);
        await interp.run(forthic);
      });
      result = interp.stack_pop();
    } else {
      interp.stack_push(initial);
      Object.keys(container).forEach(async (k) => {
        let v = container[k];
        interp.stack_push(v);
        await interp.run(forthic);
      });
      result = interp.stack_pop();
    }

    interp.stack_push(result);
  }

  // ( a -- )
  word_POP(interp) {
    interp.stack_pop();
  }

  // ( a -- a a )
  word_DUP(interp) {
    let a = interp.stack_pop();
    interp.stack_push(a);
    interp.stack_push(a);
  }

  // ( a b -- b a )
  word_SWAP(interp) {
    let b = interp.stack_pop();
    let a = interp.stack_pop();
    interp.stack_push(b);
    interp.stack_push(a);
  }

  // ( item -- str )
  word_to_STR(interp) {
    let item = interp.stack_pop();
    interp.stack_push(item.toString());
  }

  // ( str1 str2 -- str )
  // ( array_of_str -- str )
  word_CONCAT(interp) {
    let str2 = interp.stack_pop();
    let array;
    if (str2 instanceof Array) {
      array = str2;
    } else {
      let str1 = interp.stack_pop();
      array = [str1, str2];
    }

    let result = array.join("");
    interp.stack_push(result);
  }

  // ( string sep -- items )
  word_SPLIT(interp) {
    let sep = interp.stack_pop();
    let string = interp.stack_pop();

    if (!string) string = "";

    let result = string.split(sep);
    interp.stack_push(result);
  }

  // ( strings sep -- string )
  word_JOIN(interp) {
    let sep = interp.stack_pop();
    let strings = interp.stack_pop();

    if (!strings) strings = [];

    let result = strings.join(sep);
    interp.stack_push(result);
  }

  // ( -- char )
  word_slash_N(interp) {
    interp.stack_push("\n");
  }

  // ( -- char )
  word_slash_R(interp) {
    interp.stack_push("\r");
  }

  // ( -- char )
  word_slash_T(interp) {
    interp.stack_push("\t");
  }

  // ( A -- a )
  word_LOWERCASE(interp) {
    let string = interp.stack_pop();
    let result = string.toLowerCase();
    interp.stack_push(result);
  }

  // ( a -- A )
  word_UPPERCASE(interp) {
    let string = interp.stack_pop();
    let result = string.toUpperCase();
    interp.stack_push(result);
  }

  // ( string -- string )
  word_ASCII(interp) {
    let string = interp.stack_pop();
    let result = "";
    for (let i = 0; i < string.length; i++) {
      let ch = string[i];
      if (ch.charCodeAt(0) < 256) result += ch;
    }
    interp.stack_push(result);
  }

  // ( str -- str )
  word_STRIP(interp) {
    let string = interp.stack_pop();
    let result = string;
    if (result) result = result.trim();
    interp.stack_push(result);
  }

  // ( string text replace -- string )
  word_REPLACE(interp) {
    let replace = interp.stack_pop();
    let text = interp.stack_pop();
    let string = interp.stack_pop();

    let result = string;
    if (string) {
      let pattern = new RegExp(text, "g");
      result = string.replace(pattern, replace);
    }
    interp.stack_push(result);
  }

  // ( string pattern -- match )
  word_RE_MATCH(interp) {
    let pattern = interp.stack_pop();
    let string = interp.stack_pop();

    let re_pattern = new RegExp(pattern);
    let result = false;
    if (string !== null) result = string.match(re_pattern);
    interp.stack_push(result);
  }

  // ( string pattern -- matches )
  word_RE_MATCH_ALL(interp) {
    let pattern = interp.stack_pop();
    let string = interp.stack_pop();

    let re_pattern = new RegExp(pattern, "g");
    let matches = [];
    if (string !== null) matches = string.matchAll(re_pattern);
    let result = Array.from(matches).map((v) => v[1]);

    interp.stack_push(result);
  }

  // ( match num -- string )
  word_RE_MATCH_GROUP(interp) {
    let num = interp.stack_pop();
    let match = interp.stack_pop();
    let result = null;
    if (match) result = match[num];
    interp.stack_push(result);
  }

  // ( time -- time )
  word_AM(interp) {
    let time = interp.stack_pop();
    if (!time instanceof Date) throw "AM expecting a time";

    let result = time;
    if (time.getHours() >= 12) {
      result = new Date();
      result.setHours(time.getHours() - 12);
      result.setMinutes(time.getMinutes());
    }
    interp.stack_push(result);
  }

  // ( time -- time )
  word_PM(interp) {
    let time = interp.stack_pop();
    if (!time instanceof Date) throw "PM expecting a time";

    let result = time;
    if (time.getHours() < 12) {
      result = new Date();
      result.setHours(time.getHours() + 12);
      result.setMinutes(time.getMinutes());
    }
    interp.stack_push(result);
  }

  // ( -- time )
  word_NOW(interp) {
    let result = new Date();
    interp.stack_push(result);
  }

  // ( item -- time )
  word_to_TIME(interp) {
    let item = interp.stack_pop();
    let result;
    if (item instanceof Date) {
      result = item;
    } else {
      // NB: We need a date in order for Date.parse to succeed. Also assuming str is a time
      let date_string = "Jan 1, 2000 " + item;
      result = new Date(Date.parse(date_string));
    }

    interp.stack_push(result);
  }

  // ( time -- str )
  word_TIME_to_STR(interp) {
    let time = interp.stack_pop();
    let result = time.getHours() + ":" + time.getMinutes();
    interp.stack_push(result);
  }

  // ( str -- date )
  word_to_DATE(interp) {
    let s = interp.stack_pop();
    function isValidDate(date) {
      return s != null && date instanceof Date && !isNaN(date);
    }

    let result;
    if (isValidDate(s)) {
      result = s;
    } else {
      result = new Date(s);
      if (!isValidDate(result)) result = null;
    }
    interp.stack_push(result);
  }

  // ( -- date )
  word_TODAY(interp) {
    interp.stack_push(new Date());
  }

  // ( -- date )
  word_MONDAY(interp) {
    interp.stack_push(GlobalModule.get_day_this_week(0));
  }

  // ( -- date )
  word_TUESDAY(interp) {
    interp.stack_push(GlobalModule.get_day_this_week(1));
  }

  // ( -- date )
  word_WEDNESDAY(interp) {
    interp.stack_push(GlobalModule.get_day_this_week(2));
  }

  // ( -- date )
  word_THURSDAY(interp) {
    interp.stack_push(GlobalModule.get_day_this_week(3));
  }

  // ( -- date )
  word_FRIDAY(interp) {
    interp.stack_push(GlobalModule.get_day_this_week(4));
  }

  // ( -- date )
  word_SATURDAY(interp) {
    interp.stack_push(GlobalModule.get_day_this_week(5));
  }

  // ( -- date )
  word_SUNDAY(interp) {
    interp.stack_push(GlobalModule.get_day_this_week(6));
  }

  static get_day_this_week(day_of_week) {
    // NOTE: Monday is start of week
    function normalize_day(day) {
      if (day == 0) return 6; // Sunday maps to 6
      else return day - 1;
    }
    let today = new Date();
    let delta_days = (day_of_week - normalize_day(today.getDay())) % 7;
    if (day_of_week < today.getDay()) delta_days -= 7;

    let result = today;
    result.setDate(result.getDate() + delta_days);
    return result;
  }

  // ( date num-days -- date )
  word_ADD_DAYS(interp) {
    let num_days = interp.stack_pop();
    let date = interp.stack_pop();

    let result = new Date(date);
    result.setDate(result.getDate() + num_days);
    interp.stack_push(result);
  }

  // ( l_date r_date -- num_days )
  word_SUBTRACT_DATES(interp) {
    let r_date = interp.stack_pop();
    let l_date = interp.stack_pop();
    let ms_per_day = 1000 * 60 * 60 * 24;
    let result = Math.round((l_date.getTime() - r_date.getTime()) / ms_per_day);
    interp.stack_push(result);
  }

  // ( date -- str )
  word_DATE_to_STR(interp) {
    let date = interp.stack_pop();
    if (date instanceof Date) {
        let m = date.getMonth() + 1;
        let d = date.getDate();
        let y = date.getFullYear()
        if (m < 10)   m = `0${m}`
        if (d < 10)   d = `0${d}`
        interp.stack_push(`${y}-${m}-${d}`);
    }
    else interp.stack_push("");
  }

  // ( date time -- datetime )
  word_DATE_TIME_to_DATETIME(interp) {
    let time = interp.stack_pop();
    let date = interp.stack_pop();
    let dt_string = `${date.getFullYear()}-${
      date.getMonth() + 1
    }-${date.getDate()} ${time.getHours()}:${time.getMinutes()}`;
    let result = new Date(dt_string);
    interp.stack_push(result);
  }

  // ( datetime -- timestamp )
  word_DATETIME_to_TIMESTAMP(interp) {
    let datetime = interp.stack_pop();
    let result = Math.round(datetime.getTime() / 1000);
    interp.stack_push(result);
  }

  // ( timestamp -- datetime )
  word_TIMESTAMP_to_DATETIME(interp) {
    let timestamp = interp.stack_pop();
    let result = new Date(timestamp * 1000);
    interp.stack_push(result);
  }

  // ( str -- datetime )
  word_STR_to_DATETIME(interp) {
    let s = interp.stack_pop();
    let result = new Date(s);
    interp.stack_push(result);
  }

  // ( str -- timestamp )
  word_STR_to_TIMESTAMP(interp) {
    let s = interp.stack_pop();
    let datetime = new Date(s);
    let result = Math.round(datetime.getTime() / 1000);
    interp.stack_push(result);
  }

  // ( a b -- a+b )
  // ( items -- sum )
  word_plus(interp) {
    let items = interp.stack_pop();

    if (items instanceof Array) {
      let sum = 0;
      items.forEach((item) => {
        sum += item;
      });
      interp.stack_push(sum);
    } else {
      let b = items;
      let a = interp.stack_pop();
      interp.stack_push(a + b);
    }
  }

  // ( a b -- a*b )
  word_times(interp) {
    let b = interp.stack_pop();
    let result = 1
    let numbers = []
    if (b instanceof Array) {
        numbers = b
    }
    else {
        let a = interp.stack_pop()
        numbers = [a, b]
    }
    for (const num of numbers) {
        if (num === null || num === undefined)   {
            interp.stack_push(null)
            return
        }
        result = result * num
    }
    interp.stack_push(result)
  }

  // ( a b -- a/b )
  word_divide_by(interp) {
    let b = interp.stack_pop();
    let a = interp.stack_pop();
    interp.stack_push(a / b);
  }

  // ( a b -- res )
  word_MOD(interp) {
    let b = interp.stack_pop();
    let a = interp.stack_pop();
    interp.stack_push(a % b);
  }

  // ( numbers -- mean )
  word_MEAN(interp) {
    let numbers = interp.stack_pop();
    let sum = 0
    for (const num of numbers) {
        sum += num
    }
    let result = sum/numbers.length
    interp.stack_push(result)
  }

  // ( num -- int )
  word_ROUND(interp) {
    let num = interp.stack_pop();
    interp.stack_push(Math.round(num));
  }

  // ( l r -- bool )
  word_equal_equal(interp) {
    let r = interp.stack_pop();
    let l = interp.stack_pop();
    interp.stack_push(l == r);
  }

  // ( l r -- bool )
  word_not_equal(interp) {
    let r = interp.stack_pop();
    let l = interp.stack_pop();
    interp.stack_push(l != r);
  }

  // ( l r -- bool )
  word_greater_than(interp) {
    let r = interp.stack_pop();
    let l = interp.stack_pop();

    if (l === null || r === null) {
      interp.stack_push(null);
      return;
    }

    interp.stack_push(l > r);
  }

  // ( l r -- bool )
  word_greater_than_or_equal(interp) {
    let r = interp.stack_pop();
    let l = interp.stack_pop();

    if (l === null || r === null) {
      interp.stack_push(null);
      return;
    }

    interp.stack_push(l >= r);
  }

  // ( l r -- bool )
  word_less_than(interp) {
    let r = interp.stack_pop();
    let l = interp.stack_pop();

    if (l === null || r === null) {
      interp.stack_push(null);
      return;
    }

    interp.stack_push(l < r);
  }

  // ( l r -- bool )
  word_less_than_or_equal(interp) {
    let r = interp.stack_pop();
    let l = interp.stack_pop();

    if (l === null || r === null) {
      interp.stack_push(null);
      return;
    }

    interp.stack_push(l <= r);
  }

  // ( l r -- bool )
  // ( items -- bool )
  word_OR(interp) {
    let r = interp.stack_pop();

    let items;
    if (r instanceof Array) {
      items = r;
    } else {
      let l = interp.stack_pop();
      items = [l, r];
    }
    let result = items.some((item) => item);
    interp.stack_push(result);
  }

  // ( l r -- bool )
  // ( items -- bool )
  word_AND(interp) {
    let r = interp.stack_pop();

    let items;
    if (r instanceof Array) {
      items = r;
    } else {
      let l = interp.stack_pop();
      items = [l, r];
    }
    let result = items.every((item) => item);
    interp.stack_push(result);
  }

  // ( bool -- bool )
  word_NOT(interp) {
    let value = interp.stack_pop();
    interp.stack_push(!value);
  }

  // ( item items -- bool )
  word_IN(interp) {
    let items = interp.stack_pop();
    let item = interp.stack_pop();
    if (!items) items = [];
    let result = items.indexOf(item) >= 0;
    interp.stack_push(result);
  }

  // ( vals required_vals -- bool )
  word_ANY(interp) {
    let required_vals = interp.stack_pop();
    let vals = interp.stack_pop();

    let result = false;
    for (let i = 0; i < required_vals.length; i++) {
      let rv = required_vals[i];
      if (vals.indexOf(rv) >= 0) {
        result = true;
        break;
      }
    }

    // If nothing is required, then all values are true
    if (required_vals.length == 0) result = true;

    interp.stack_push(result);
  }

  // ( vals required_vals -- bool )
  word_ALL(interp) {
    let required_vals = interp.stack_pop();
    let vals = interp.stack_pop();

    if (!vals) vals = [];
    if (!required_vals) required_vals = [];

    let result = required_vals.every((val) => vals.indexOf(val) >= 0);
    interp.stack_push(result);
  }

  // ( item -- bool )
  word_to_BOOL(interp) {
    let item = interp.stack_pop();
    let result = !!item;
    interp.stack_push(result);
  }

  // ( item -- int )
  word_to_INT(interp) {
    let str = interp.stack_pop();
    let result = parseInt(str);
    interp.stack_push(result);
  }

  // ( item -- int )
  word_to_FLOAT(interp) {
    let str = interp.stack_pop();
    let result = parseFloat(str);
    interp.stack_push(result);
  }

  // ( num buckets -- bucket )
  // Each bucket has three elements: [low high value]. If num is >= low and < high, it's in the bucket
  // If a number isn't in any bucket, null is returned
  word_BUCKET(interp) {
    let buckets = interp.stack_pop();
    let num = interp.stack_pop();

    let result = null;
    for (let i = 0; i < buckets.length; i++) {
      let low = buckets[i][0];
      let high = buckets[i][1];
      let value = buckets[i][2];

      if (num >= low && num < high) {
        result = value;
        break;
      }
    }
    if (num == undefined) result = "";
    interp.stack_push(result);
  }

  // ( low high -- int )
  word_UNIFORM_RANDOM(interp) {
    let high = interp.stack_pop();
    let low = interp.stack_pop();

    // From: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
    function getRandomIntInclusive(min, max) {
      min = Math.ceil(min);
      max = Math.floor(max);
      return Math.floor(Math.random() * (max - min + 1) + min); //The maximum is inclusive and the minimum is inclusive
    }
    let result = getRandomIntInclusive(low, high);
    interp.stack_push(result);
  }

  // ( val start_ranges -- index )
  word_RANGE_INDEX(interp) {
    let start_ranges = interp.stack_pop();
    let val = interp.stack_pop();

    // Cap off the value ranges with infinity
    start_ranges.push(Infinity);

    if (val === null || !start_ranges) {
      interp.stack_push(null);
      return;
    }

    if (val < start_ranges[0]) {
      interp.stack_push(null);
      return;
    }

    let result = null;
    for (let i = 0; i < start_ranges.length - 1; i++) {
      if (val >= start_ranges[i] && val < start_ranges[i + 1]) {
        result = i;
        break;
      }
    }

    interp.stack_push(result);
  }

  // ( -- )
  word_bang_PUSH_ERROR(interp) {
    FLAGS.push_error = true;
  }

  // ( -- )
  word_bang_WITH_KEY(interp) {
    FLAGS.with_key = true;
  }

  // (comparator -- )
  //
  // `comparator` may be a Forthic string or a Python key function
  word_bang_COMPARATOR(interp) {
    let comparator = interp.stack_pop();
    FLAGS.comparator = comparator;
  }

  // ( -- )
  word_bang_PUSH_REST(interp) {
    FLAGS.push_rest = true;
  }

  // (depth -- )
  //
  // NOTE: `depth` of 0 is the same not having set depth
  word_bang_DEPTH(interp) {
    let depth = interp.stack_pop();
    FLAGS.depth = depth;
  }

  // ( bool -- )
  word_bang_OVERWRITE(interp) {
    let overwrite = interp.stack_pop();
    FLAGS.overwrite = overwrite;
  }

  // ( delay_ms -- )
  word_bang_DELAY(interp) {
    let delay_ms = interp.stack_pop();
    FLAGS.delay = delay_ms;
  }

  // (element_name -- element)
  word_Element(interp) {
    const element_name = interp.stack_pop();
    const clean_element_name = element_name.replace(
      /[()[\]<>~!@#$%^&+=\-*\s]/g,
      ""
    );

    function Result() {
      let content_array = ensure_array(Result.content);
      let rendered_content = render_content_array(content_array);

      let element_class = NAME_TO_ELEMENT[clean_element_name];
      if (!element_class) {
        element_class = eval(clean_element_name);
      }

      let res = React.createElement(
        element_class,
        {...Result.props, interp},
        ...rendered_content
      );
      return res;
    }
    interp.stack_push(Result);
  }

  // (path element_func -- route_record)
  word_Route(interp) {
    let element_func = interp.stack_pop();
    let path = interp.stack_pop();
    let result = {
      path: window.basename + path,
      element: element_func(),
    };
    interp.stack_push(result);
  }

  // (routes -- router)
  word_Router(interp) {
    let routes = interp.stack_pop();
    const router = createBrowserRouter(routes);
    interp.stack_push(router);
  }

  // (module_name -- ForthicPage)
  word_ForthicPage(interp) {
    const module_name = interp.stack_pop();
    function Result() {
      let res = React.createElement(ForthicPage, { module_name });
      return res;
    }
    interp.stack_push(Result);
  }

  // ( varname -- value )
  word_QPARAM(interp) {
    let varname = interp.stack_pop();
    let result = get_qparam(varname);
    interp.stack_push(result);
  }

  // ( value varname -- )
  word_QPARAM_bang(interp) {
    // TODO: We should escape the value
    let varname = interp.stack_pop();
    let value = interp.stack_pop();
    const flags = get_flags()

    let cur_value = get_qparam(varname);
    if (value === cur_value)   return

    // If !DELAY is set, then we'll use setTimeout to delay setting the query param. If another QPARAM! happens
    // in the meantime, we'll cancel this call and replace it with the new one
    let delay_ms = flags.delay ? flags.delay : 0

    if (QPARAM_BANG_ID)   clearTimeout(QPARAM_BANG_ID)
    QPARAM_BANG_ID = setTimeout(() => {
        set_qparam(varname, value);
        QPARAM_BANG_ID = null
    }, delay_ms)
  }

  // ( pairs -- )
  // `pairs` is an array of name/value
  word_QPARAMS_bang(interp) {
    let pairs = interp.stack_pop();

    // Merge the provided params with the current qparams, removing any that are explicitly undefined
    let search_params = new URLSearchParams(window.location.search)

    for (const pair of pairs) {
      let name = pair[0];
      let value = pair[1];
      if (value === null || value === undefined || value === '') {
        del_qparam(name);
        search_params.delete(name)
      } else {
        search_params.set(name, value)
      }
    }

    window.location.search = search_params.toString()
}

  // ( varname -- )
  word_DEL_QPARAM_bang(interp) {
    let varname = interp.stack_pop();
    del_qparam(varname);
  }

  // ( url -- url_w_qparams )
  // Adds current qparams to specified URL
  word_l_QPARAMS(interp) {
    let url = interp.stack_pop();
    let search_params = new URLSearchParams(window.location.search)

    // Handle case where the URL has qparams
    let url_parts = url.split("?")
    let url_w_out_qparams = url_parts[0]
    let url_qparams = url_parts[1]

    // Merge url params with current search params
    if (url_qparams) {
        let qparams = url_qparams.split("&")
        for (const qparam of qparams) {
            let pieces = qparam.split("=")
            search_params.set(pieces[0], pieces[1])
        }
    }

    // Append search params to URL
    let result = url_w_out_qparams + "?" + search_params.toString();
    interp.stack_push(result)
  }

  // ( string -- string )
  word_console_log(interp) {
    let string = interp.stack_pop();
    console.log(string);
    interp.stack_push(string)
  }

  // ( url -- )
  word_window_open(interp) {
    let url = interp.stack_pop();
    window.open(url)
  }

    // ( str -- )
    word_TITLE_bang(interp) {
        let str = interp.stack_pop();
        document.title = str;
    }

  // ( args word -- ? )
  // If the !PUSH-ERRORS flag is set, this pushes an error after the return values
  async word_SERVER_INTERPRET(interp) {
    const word = interp.stack_pop();
    const args = interp.stack_pop();
    const flags = get_flags()

    function get_forthic_route() {
      return [window.location.origin, window.basename, "forthic"].join("/");
    }

    async function get_forthic() {
      interp.stack_push(args);
      await interp.run(`">JSON QUOTED  ' JSON>' CONCAT" MAP " " JOIN`);
      const arg_string = interp.stack_pop();
      const result = `${arg_string} ${word}`;
      return result;
    }

    await csrf_axios
      .post(get_forthic_route(), {
        forthic: await get_forthic(),
        fullstack_response: true,
      })
      .then(function (response) {
        let result_stack = response.data.result;
        for (let val of result_stack) {
          interp.stack_push(val);
        }
      })
      .catch(function (error) {
        if (flags.push_error) {
          interp.stack_push({error: error.response})
        }
        else {
          console.log(error);
          alert(`${error}: ${error.response.data}`);
        }
      });
  }

  // (element content -- element)
  word_l_CONTENT(interp) {
    let content = interp.stack_pop();
    let element = interp.stack_pop();
    element.content = content;
    interp.stack_push(element);
  }

  // (element props -- element)
  word_l_PROPS(interp) {
    let props = interp.stack_pop();
    let element = interp.stack_pop();
    element.props = props;
    interp.stack_push(element);
  }

  // (element string -- element)
  word_ll_CLASSNAME(interp) {
    let string = interp.stack_pop();
    let element = interp.stack_pop();

    const flags = get_flags();

    let props = element.props;
    if (!props) props = {};
    let className = props.className;
    if (!className) className = "";

    if (flags.overwrite)   props.className = string;
    else                   props.className = className + " " + string;

    element.props = props;
    interp.stack_push(element);
  }

  // (forthic -- callback_function)
  word_FCALLBACK(interp) {
    let forthic = interp.stack_pop();
    const modularized_forthic = modularize_forthic(interp, forthic)

    async function result(data) {
      interp.stack_push(data);
      await interp.run(modularized_forthic);
    }
    interp.stack_push(result);
  }

  // (str -- encoded)
  word_URL_ENCODE(interp) {
    let str = interp.stack_pop();
    let result = "";
    if (str) result = encodeURIComponent(str);
    interp.stack_push(result);
  }

  // (urlencoded -- decoded)
  word_URL_DECODE(interp) {
    let urlencoded = interp.stack_pop();
    let result = "";
    if (urlencoded) result = decodeURIComponent(urlencoded);
    interp.stack_push(result);
  }

  // ( -- char)
  word_QUOTE_CHAR(interp) {
    interp.stack_push(DLE);
  }

  // ( string -- quoted_string)
  word_QUOTED(interp) {
    let string = interp.stack_pop();
    let clean_string = "";
    for (let i = 0; i < string.length; i++) {
      let c = string[i];
      if (c == DLE) c = " ";
      clean_string += c;
    }
    let result = `${DLE}${clean_string}${DLE}`;
    interp.stack_push(result);
  }

  // ( -- MessageBroker )
  word_MESSAGE_BROKER(interp) {
    interp.stack_push(new MessageBroker())
  }

  // ( MessageBroker message -- )
  word_PUBLISH_MESSAGE(interp) {
    const message = interp.stack_pop()
    const broker = interp.stack_pop()
    broker.publish(message)
  }

  // ( -- )
  word_PROFILE_START(interp) {
    interp.start_profiling();
  }

  // ( -- )
  word_PROFILE_END(interp) {
    interp.stop_profiling();
  }

  // ( label -- )
  word_PROFILE_TIMESTAMP(interp) {
    let label = interp.stack_pop();
    interp.add_timestamp(label);
  }

  // ( -- )
  word_PROFILE_DATA(interp) {
    let histogram = interp.word_histogram();
    let timestamps = interp.profile_timestamps();

    let result = {
      word_counts: [],
      timestamps: [],
    };
    histogram.forEach((val) => {
      let rec = { word: val["word"], count: val["count"] };
      result["word_counts"].push(rec);
    });

    let prev_time = 0.0;
    timestamps.forEach((t) => {
      let rec = {
        label: t["label"],
        time_ms: t["time_ms"],
        delta: t["time_ms"] - prev_time,
      };
      prev_time = t["time_ms"];
      result["timestamps"].push(rec);
    });

    interp.stack_push(result);
  }

  // ( -- null )
  word_NULL(interp) {
    interp.stack_push(null);
  }

  // ( value default_value -- value )
  word_DEFAULT(interp) {
    let default_value = interp.stack_pop();
    let value = interp.stack_pop();
    if (value === undefined || value === null || value === "")
      value = default_value;
    interp.stack_push(value);
  }

  // ( value default_forthic -- value )
  async word_star_DEFAULT(interp) {
    let default_forthic = interp.stack_pop();
    let value = interp.stack_pop();

    if (value === undefined || value === null || value === "") {
      await interp.run(default_forthic);
      value = interp.stack_pop();
    }
    interp.stack_push(value);
  }

  // ( Record default_key/vals -- Record )
  word_REC_DEFAULTS(interp) {
    let key_vals = interp.stack_pop();
    let record = interp.stack_pop();
    key_vals.forEach((key_val) => {
      let key = key_val[0];
      let value = record[key];
      if (value === undefined || value === null || value == "") {
        record[key] = key_val[1];
      }
    });

    interp.stack_push(record);
  }

  // ( item string num-times -- ? )
  async word_l_REPEAT(interp) {
    let num_times = interp.stack_pop();
    let string = interp.stack_pop();
    for (let i = 0; i < num_times; i++) {
      // Store item so we can push it back later
      let item = interp.stack_pop();
      interp.stack_push(item);

      await interp.run(string);
      let res = interp.stack_pop();

      // Push original item and result
      interp.stack_push(item);
      interp.stack_push(res);
    }
  }

  // ( a -- a )
  async word_IDENTITY(interp) {}

  // ( value num_places -- str )
  word_to_FIXED(interp) {
    let num_places = interp.stack_pop();
    let value = interp.stack_pop();
    let result = value;
    if (value === undefined || value === null) result = "";
    else if (!isNaN(value)) result = value.toFixed(num_places);
    interp.stack_push(result);
  }

  // ( object -- json )
  word_to_JSON(interp) {
    let object = interp.stack_pop();
    let result = JSON.stringify(object);
    interp.stack_push(result);
  }

  // ( json -- object )
  word_JSON_to(interp) {
    let str = interp.stack_pop();
    let result = null;
    if (str) result = JSON.parse(str);
    interp.stack_push(result);
  }

  // ( -- )
  word_dot_s(interp) {
    console.log(interp.stack);
    debugger;
  }

  // ( a b -- a - b )
  word_minus(interp) {
    let b = interp.stack_pop();
    let a = interp.stack_pop();
    interp.stack_push(a - b);
  }
}


function modularize_forthic(interp, forthic) {
    // Condition forthic if the callback is defined within a module:
    //
    //     We need to do this because the module stack when the callback is called may be different from
    //     the one it was defined in.
    //
    //     The top of the module stack is the module of this definition
    //     The app module is always the first module on the stack and has "" as its name
    const callback_module = interp.module_stack[interp.module_stack.length - 1];
    let modularized_forthic = forthic;
    if (callback_module.name !== "") {
      modularized_forthic = `{${callback_module.name} ${forthic}}`;
    }
    return modularized_forthic
}


// Descends into record using an array of fields, returning final value or null
function drill_for_value(record, fields) {
    let result = record;
    for (let i=0; i < fields.length; i++) {
        let f = fields[i];
        if (result == null)   return null;
        result = result[f];
    }
    return result
}



function get_qparam(name) {
    const search_params = new URLSearchParams(window.location.search)
    return search_params.get(name)
}

function set_qparam(name, value) {
    let search_params = new URLSearchParams(window.location.search)
    search_params.set(name, value)
    window.location.search = search_params.toString()
}

function del_qparam(name) {
    let search_params = new URLSearchParams(window.location.search)
    search_params.delete(name)
    window.location.search = search_params.toString()
}

async function execute_returning_error(interp, forthic) {
    let result = null
    try {
        await interp.run(forthic)
    }
    catch(e) {
        result = e
    }
    return result
}


export { GlobalModule };
