# ForthicPage

`ForthicPage` is used to define a page that gets data from the server based on its query parameters.
It also maintains a local page state that can be updated by the page’s Forthic code.

To use a `ForthicPage`, you must define the following words within a Forthic module (note that all of them begin with `PAGE-`):

* `PAGE-DATA`: This is an empty memo that `ForthicPage` will use to store data when it needs to
refresh the page data when query params change
* `PAGE-STATE`: This is an empty memo that `ForthicPage` will use to store page state
* `PAGE-DATA-UPDATE ( -- data )`: This word is called whenever the query params change to retrieve new page data.
* `PAGE-BROKER`: This is a memo returning a message broker. The `ForthicPage` will subscribe to this, extending the contents of `PAGE-STATE` with new messages.
* `PAGE-DEFAULT-STATE`: This is the default value of `PAGE-STATE`.
* `PAGE-DEFAULT-QPARAMS`: This is a record whose fields are query param names and whose values are the default query param values.
* `PAGE-CONTENT`: This returns the content of the page to render.


## A Simple Example
```

# This is the module that contains the required "PAGE" words
{MyModule
    @: PAGE-DATA    ;
    @: PAGE-STATE   ;
    @: PAGE-BROKER       MESSAGE-BROKER;
    : PAGE-DATA-UPDATE   ["username" QPARAM "ticket_type" QPARAM] "PAGE-DATA" SERVER-INTERPRET;

    # An example of how to publish a state change via the PAGE-BROKER
    : UPDATE-PAGE-STATE   PAGE-BROKER SWAP PUBLISH-MESSAGE;  # (state --)

    : PAGE-DEFAULT-STATE   [] REC;
    : PAGE-DEFAULT-QPARAMS   [
        ["ticket_type"   "MYTEAM"]
        ["username"      "myusername"]
    ] REC;

    : PAGE-CONTENT   H1 "My Content Header" <CONTENT;
}

# This is how we use the module to create a ForthicPage
: HOME-ELEMENT   "MyModule" ForthicPage;
```