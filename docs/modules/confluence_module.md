# confluence_module

The confluence module allows you to publish wiki pages to Confluence in wiki
markup format.

The confluence module is configured with user credentials via a "context" using
`PUSH-CONTEXT!`. See reference section below for more info.

## Example
```
["confluence"] USE-MODULES

# NOTE: You must create a confluence context and apply it using PUSH-CONTEXT!

'SPACE' 'Parent title' 'Page title' 'h2. My Content' confluence.UPSERT-PAGE
```

## Reference

### PUSH-CONTEXT!
`(context --)`

The `context` is an object that provides a confluence host, a username, and a
password used to authorize requests to the Confluence API. It must be set up in
the Python code.

This word pushes a context onto the confluence module's context stack. The top
of this stack is used as the context for Confluence API calls.

Here's a simple example:
```
from forthic.interpreter import Interpreter
import forthic.modules.confluence_module as confluence_module

def get_interp():
    interp = Interpreter()
    interp.register_module(confluence_module.ConfluenceModule)

    class MyContext(confluence_module.ConfluenceContext):
        def get_host(self):
            return "http://someconfluencehost"
        
        def get_username(self):
            return "myusername"
        
        def get_password(self):
            # NOTE: This is for illustration purposes
            #       You should really read the password from disk/password manager
            return "mypassword"

    # Set up confluence context
    interp.run("['confluence'] USE-MODULES")
    interp.stack_push(MyContext())
    interp.run("confluence.PUSH-CONTEXT!")
    return interp
```

### POP-CONTEXT!
`( -- )`

This pops a context from the context stack and throws it away.


### HOST
`( -- host )`

Returns the Confluence host for the current context


### PAGE-INFO
`( space page_title -- page_info )`

This returns page info metadata for the specified `page_title` in the specified wiki `space`.

This word is used by other words in the confluence module and is typically not something
an end user would find useful.


### NBSP
`( -- nbsp_char )`

This returns a "non-breaking whitespace" character that can be inserted into the wiki markup.

### SPACES-WIDE
`( str num_spaces -- str )`

This is a convenience word that pads a string `str` with non-breaking whitespace
characters until it is `num_spaces` long. If the string is already more than
`num_spaces` long, this does nothing.

This is useful for forcing columns in a table to be a certain width.


### |ESCAPE-TABLE-CONTENT
`( str -- str )`

This escapes content that is rendered into a wiki table cell. 

In particular, this converts newlines into `\\`, *except* for  bulleted lists and
numbered lists. It also removes the '|' character except in the case where it's
used to specify a link

### TABLE
`( header records -- wiki_markup )`

Given a `header` and `records` with fields corresponding to the `header` values, constructs a table in wiki markup with a row for each record.


### UPSERT-PAGE
`( space parent_title page_title content -- )`

Creates/updates a page in the specified `space` under the specified `parent_title` page. The content
should be in [wiki markup](https://confluence.atlassian.com/doc/confluence-wiki-markup-251003035.html).
