import { Interpreter } from '../../interpreter.mjs';
import { assert, arrays_equal } from '../utils.mjs';

async function test_element() {
    let interp = new Interpreter();
    await interp.run(`
    [ "html" ] "./modules" LOAD-MODULES
    "h1" html.ELEMENT
    `);
    let stack = interp.stack;
    assert(stack[0].tagName == "H1")
    return true;
}

async function test_common_elements() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    H1 H2 H3 H4 H5 H6
    P UL OL LI A
    TABLE TR TH TD
    SPAN DIV SECTION
    IMG
    STYLE
    CANVAS
    `);
    return true;
}

async function test_attributes() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    A "src" "https://www.linkedin.com" <ATTR!  "src" ATTR
    P [["class" "error"] ["display" "block"]] <ATTR! DUP "class" ATTR SWAP "display" ATTR
    `);
    let stack = interp.stack;
    assert(stack[0] == "https://www.linkedin.com")
    assert(stack[1] == "error")
    assert(stack[2] == "block")
    return true;
}

async function test_child_nodes() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    ["table" "row"] VARIABLES
    TABLE table !
    TR row !
    table @ row @ [TH TH TH] <APPEND <APPEND POP

    table @ CHILD-NODES LENGTH
    table @ CHILD-NODES 0 NTH  row @ ==
    row @ CHILD-NODES LENGTH
    `);
    let stack = interp.stack;
    assert(stack[0] == 1)
    assert(stack[1] == true)
    assert(stack[2] == 3)
    return true;
}

async function test_node_content() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    P "Now is the <b>time</b>" <INNER-HTML! INNER-HTML
    P "Now is the <b>time</b>" <INNER-HTML! INNER-TEXT

    P "Now is the <b>time</b>" <INNER-TEXT! INNER-HTML
    P "Now is the <b>time</b>" <INNER-TEXT! INNER-TEXT

    TABLE TR <APPEND INNER-HTML

    DIV TABLE <APPEND "<span></span" "afterbegin" <INSERT-ADJ-HTML INNER-HTML
    `);
    let stack = interp.stack;
    assert(stack[0] == "Now is the <b>time</b>")
    assert(stack[1] == "Now is the time")
    assert(stack[2] == "Now is the &lt;b&gt;time&lt;/b&gt;")
    assert(stack[3] == "Now is the <b>time</b>")
    assert(stack[4] == "<tr></tr>")
    assert(stack[5] == "<span></span><table></table>")
    return true;
}

async function test_svg() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    "rect" SVG-ELEMENT
    `);
    let stack = interp.stack;
    assert(stack[0].tagName == "rect")
    assert(stack[0].namespaceURI == "http://www.w3.org/2000/svg")
    return true;
}

async function test_canvas() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    CANVAS "2d" CANVAS-CONTEXT
    `);
    let stack = interp.stack;
    assert(stack[0].canvas !== null)
    return true;
}

async function test_classes() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    H1 "important" <ADD-CLASS [ "note" "highlight" ] <ADD-CLASS CLASSES
    H1 "important" <ADD-CLASS [ "note" "highlight" ] <ADD-CLASS "note" <REMOVE-CLASS CLASSES
    `);
    let stack = interp.stack;
    assert(arrays_equal(stack[0], ["important", "note", "highlight"]))
    assert(arrays_equal(stack[1], ["important", "highlight"]))

    return true;
}

async function test_hide_show() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    H1 <HIDE
    `);
    let stack = interp.stack;
    assert(stack[0].style.display == "none");

    await interp.run("<SHOW");
    assert(stack[0].style.display == "block");

    return true;
}


async function test_disable_enable() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    H1 <DISABLE
    `);
    let stack = interp.stack;
    assert(stack[0].disabled == true);

    await interp.run("<ENABLE");
    assert(stack[0].disabled == false);

    return true;
}

async function test_value() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    "input" ELEMENT "type" "text" <ATTR! "alpha" <VALUE!
    `);
    let stack = interp.stack;
    assert(stack[0].value == "alpha");

    await interp.run("VALUE");
    assert(stack[0] == "alpha");
    return true;
}

async function test_id_to_element() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    "container" ID>ELEMENT
    `);
    let stack = interp.stack;
    assert(stack[0].tagName == "DIV");

    return true;
}

async function test_class_to_elements() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    "heading" CLASS>ELEMENTS
    `);
    let stack = interp.stack;
    assert(stack[0].length == 3);

    return true;
}

async function test_title_bang() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    "CONTAINER"   "'container' ID>ELEMENT"   MEMO
    : HANDLER   [ SWAP "Got a mouseover" ] .p ;
    CONTAINER "mouseover" "HANDLER" <ADD-LISTENER "forthic_mouseover" REC@
    CONTAINER "mouseover" <REMOVE-LISTENER "forthic_mouseover" REC@
    `);
    let stack = interp.stack;
    assert(stack[0] !== undefined);
    assert(stack[1] === undefined);
    return true;
}

async function test_event_listeners() {
    let interp = new Interpreter();
    await interp.run(`
    [["html" ""]] "./modules" LOAD-MODULES
    "html module test" TITLE!
    `);
    return true;
}


let tests = {
    // DOM elements
    "test_element": test_element,
    "test_common_elements": test_common_elements,
    "test_child_nodes": test_child_nodes,
    "test_node_content": test_node_content,
    "test_svg": test_svg,
    "test_canvas": test_canvas,

    // Attribute manipulation
    "test_attributes": test_attributes,
    "test_classes": test_classes,

    // Misc
    "test_hide_show": test_hide_show,
    "test_disable_enable": test_disable_enable,
    "test_value": test_value,
    "test_id_to_element": test_id_to_element,
    "test_class_to_elements": test_class_to_elements,
    "test_title_bang": test_title_bang,
    "test_event_listeners": test_event_listeners,
}

export { tests };
