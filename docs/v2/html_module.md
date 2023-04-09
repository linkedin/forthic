# html_module

The html module allows you to construct HTML strings using a DOM-based model.
This allows the Python and Javascript versions of this module to be (mostly)
identical.

The [jinja module](./jinja_module.md) can also be used to construct template
HTML at a higher level.

## Example
```
[["html" ""]] USE-MODULES
DIV  "main-content" <ADD-CLASS
    H1 "My Heading" <INNER-HTML! <APPEND
    UL
        [
            LI "First item" <INNER-HTML!
            LI "Second item" <INNER-HTML!
        ] <APPEND
    <APPEND
RENDER
```

## Reference

### ELEMENT
`( type -- element )`

Given a type like "H1", "SPAN", "DIV", etc. this returns a corresponding `Element` object.

### RAW-HTML
`( html_string -- RawHtml )`

Given an html string, returns a RawHtml object that will be rendered as-is.

### <APPEND
* `( parent child -- parent )`
* `( parent child_items -- parent )`

Adds a child or an array of child items to a parent `Element`.


### CHILD-NODES
`( element -- children )`

Returns the child nodes of an `Element`

### <INNER-HTML!
`( element string -- element )`

Sets the inner HTML property of an `Element`.

### <INNER-TEXT!
`( element string -- element )`

Sets the inner text property of an `Element`. The effect is to html-escape the
string so it does not have any active HTML in it.

### INNER-HTML
`( element -- string )`

Returns the inner HTML property of an element.

### <INSERT-ADJ-HTML
`( element string position -- element )`

Position is one of `beforebegin`, `afterbegin`, `beforeend`, or `afterend`.

This word inserts html content next to the specified element at the specified
position.

### <ATTR!
* `( element key val -- element )`
* `( element pairs -- element )`

This sets the attribute of an element. For example:
```
A "src" "https://www.linkedin.com" <ATTR!    # <a src="https://www.linkedin.com"></a>
```

### ATTR
`( element attr -- value )`

Returns the specified attribute for a given element.

### VALUE
`( element -- value )`

Returns the `value` property of an element (typically a control)

### <ADD-CLASS
* `( element class -- element )`
* `( element classes -- element )`

Adds the specified classes to an element's `class` attribute.

### CLASSES
`( element -- classes )`

Returns the classes of an element

### <REMOVE-CLASS
* `( element class -- element )`
* `( element classes -- element )`

Removes the specified classes from an element.


## HTML Elements
These are convenience words defined in terms of the `ELEMENT` word.
* `H1`, `H2`, `H3`, `H4`, `H5`, `H6`
* `P`, `UL`, `OL`, `LI`
* `A`, `SPAN`
* `TABLE`, `TR`, `TH`, `TD`
* `DIV`, `SECTION`
* `STYLE`, `IMG`, `CANVAS`
* `SCRIPT`
* `SVG`

## Python-specific Words

### MARKDOWN>HTML
`( markdown -- html )`

Given a markdown string, convert it to HTML.

### RENDER
* `( element -- html )`
* `( elements -- html )`

Renders one or more `Element`s as html. For multiple `Element`s, the html is
concatenated.

### JS-PATH!
`( path -- )`

When running on a web server, this sets the relative URL path to the Forthic
Javascript interpreter. This allows the `RUN-FORTHIC.JS` word to execute
Javascript-based Forthic in the browser.

### RUN-FORTHIC.JS
`( forthic -- script_element )`

Creates a script element that runs the specified `forthic` string in the Forthic
interpreter on the browser.

### FORTHIC-BUTTON
`( id label forthic -- ForthicButton )`

Creates a `ForthicButton` object that can be rendered to HTML to create a button with the specified `id` and `label` and executes the specified `Forthic` on click.

### ASYNC-FORTHIC-BUTTON
`( id label forthic_word -- AsyncForthicButton )`

Creates an `AsyncForthicButton` object that can be rendered to HTML to create a button with the specified `id` and `label` and executes the specified `forthic_word` on click. A busy indicator will be displayed until `forthic_word`
is done executing. If the page is reloaded while `forthic_word` is running, the busy indicator will still be shown.

The state of the execution is checked periodically. When done, the busy indicator is removed.

If an error occurs, the message is displayed below the button.

NOTE: This stores button state information in the cache, so the `cache` module must be used first