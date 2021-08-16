# Examples
Because Forthic apps often pull data from 3rd party sources like gsheets and Jira, it's natural for them to run as web apps -- this makes it possible to implement the OAuth flows. However, once credentials have been stored, it's easy to run Forthic apps from the commandline as well. We'll describe both types of examples here.

:::{note}
These examples show how to incorporate a Forthic interpreter inside of a web app, but this is only for illustration and not meant for production environments.
:::


## Web App: Cache Example
The `ex_cache` example stores an array of strings in the cache and then reads them out again.
* `make`
* Go to [http://localhost:8000/examples/ex_cache](http://localhost:8000/examples/ex_cache)


## Web App: gsheet Example
Because this example pulls live gsheet data, it requires a bit of setup.

### Setup
If you already have a Google OAuth client, all you'll need is your **client ID** and **secret**. Otherwise, you can generate new ones by registering an OAuth client with Google:
1. Create a new Project at the [Google Developer's Console](https://console.developers.google.com/)
2. Create a new "OAuth 2.0 Client ID" noting the client ID and secret

### Run the example
The first time you run this example, you'll be asked to fill out your **client ID** and **secret**. After you submit the form, these will be encrypted and stored in `./app/examples/.secrets`. Then, you'll automatically go through an OAuth flow to authorize your client to read gsheet data from an account you specify. After this flow is finished, your OAuth token will be encrypted and stored in `./app/examples/.secrets`.

* `make`
* Go to [http://localhost:8000/examples/ex_gsheet](http://localhost:8000/examples/ex_gsheet)

Once you make it through OAuth, you'll see the output of the gsheet example. But in order to pull real data, you'll need to edit the `./apps/examples/ex_gsheet/main.forthic` file to replace `MY-URL` with a url to a gsheet and then to uncomment the second `MAIN-PAGE` line. Once you've done this, reload to do the pull.
```
["gsheet" "example_contexts"] USE-MODULES

# The gsheet module words require a context (OAuth token) to run.
example_contexts.GOOGLE gsheet.PUSH-CONTEXT!

: MY-URL           "https://add-your-gsheet-url-here";

: |TO-LINES        ">STR" MAP "<br>" JOIN;
: MY-ROWS          MY-URL gsheet.SHEET-ROWS;

: MAIN-PAGE        "Uncomment the line below once you have MY-URL defined";
# : MAIN-PAGE        ["<code>" MY-ROWS |TO-LINES "</code>"] CONCAT;
```

## Web App: Jira Example
The first time you run this example, you'll be asked to fill out a Jira **host**, **username** and **password/apitoken**. After you submit the form, these will be encrypted and stored in `./app/examples/.secrets`.

* `make`
* Go to [http://localhost:8000/examples/ex_jira](http://localhost:8000/examples/ex_jira)

:::{note}
You may need to revise the `USER` word if your Jira username doesn't match the one the example tried to use.
:::

The example will render a list of your incomplete Jira tickets.

## CLI: jinja example
This example sets up a Forthic interpreter for commandline use and runs some Forthic code to render Jinja templates. To run it:
* Go to the `forthic` root dir
* `source myenv/bin/activate`
* `python -m apps.examples.ex_jinja`

Note the use of the `.s` word to print the stack and drop into the Python debugger. You can inspect the interpreter via the `interp` variable. Some things to try:
* `interp.stack[-1]` shows the top of the stack
* `interp.run("SOME FORTHIC CODE")` lets you run Forthic code live
* `c` is just plain PDB and lets you continue to the next `.s` or `set_trace()`

## Other CLI Examples
The remaining CLI examples (`ex_jira.py` and `ex_spreadsheets.py`) are more for reference since they involve more customization to make any sense. The `ex_jira.py` example, for instance, requires actual Jira projects in order to do anything interesting.

Also, the `ex_spreadsheets.py` example shows how to make Excel calls via MS Graph, but you'll need to set up your OAuth tokens first: [https://docs.microsoft.com/en-us/graph/auth-v2-user](https://docs.microsoft.com/en-us/graph/auth-v2-user
)
