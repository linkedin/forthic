# Idioms and Techniques
This document describes conventions for using Forthic in consistent ways. This reflects years of experience building 100s of internal tools at LinkedIn.

## Naming conventions
Characters that would be operators in other languages (e.g., `|`, `<`, `>`, `!`) are routinely part of Forthic word names. E.g., `|INCOMPLETE`, `>JSON`, `TICKETS!`.

### Words that start with `|`
Words that begin with `|` are meant to filter or transform arrays and records.
For example `|NON-NULL` might filter all of the `NULL` values from an array, and a word like `|CONDITION-NAMES` might take an array of records and strip whitespace from the `name` fields.


### Words with `>`
When a word has `>` character in it, it means that the top item on the stack will be transformed into something else. For example, `KEY>TICKET` will take a key and return an associated ticket. If you have an array of keys, then `"KEY>TICKET" MAP` will convert it into an array of tickets. In fact, you might define this word as `|KEY>TICKETS`:
```
: |KEY>TICKETS   "KEY>TICKET" MAP;
```

Some transforms are more involved than a data transformation or lookup. For instance, if you were rendering a table of data, each row might correspond to a record of values. There may be a whole set of words to compute each value, but all of them could be triggered by a word like `USERNAME>ROW-DATA`

### Words with `->`
When a word has `->` in it, that means the word returns a mapping from one thing to another. For example, a mapping from user to manager might be named `USER->MANAGER`. To get the manager for a user, you might do this: `USER->MANAGER "someuser" REC@`. This could be used in a word like
```
: USER>MANAGER   USER->MANAGER SWAP REC@;
```

### Words with `<`
When a word starts with `<`, it usually means that the word is going to do something to the top element of the stack and leave it on the stack. For example, the word `<ADD-MANAGER` might add a manager to an item. Since the item remains on the stack after these kinds of words are executed, you can have multiple such words in a row:
```
ticket @ <ADD-MANAGER <ADD-SUBTICKETS <ADD-PARENT-TICKET
```

## One-line definitions
Forthic definitions should be one line long. One-line definitions are easier to visually inspect and understand. But they shouldn't be inscrutable. They should be concise because they're written at a high level of abstraction. For example, the following line is real code that pulls Jira data, constructs a report, and publishes it as a wiki page:
```
: UPDATE    SPACE PARENT-TITLE TITLE CONTENT confluence.UPSERT-PAGE;
```
Of course, each word is made up of other definitions, but each of those should be one line long if possible.

### Creating a record from an array
Often, you'll have an array of keys and a word that can map those keys into objects of some kind and you'll want a record that maps keys into objects. Here's how to do it idiomatically:
```
ITEM-KEYS DUP "KEY>RECORD" MAP ZIP REC
```
The main idea is that you want to `DUP` the keys so you can `ZIP` them into an array of key/value pairs.

### Dealing with APIs and max URL length
Some APIs allow you to pass a comma separated string of keys via the URL and get a corresponding array of records back. When the number of keys is small, using the URL is fine. However, there's a limit to how long a URL can be (usually around 2000 characters). You can deal with this case
in Forthic by splitting the keys up, batching the calls, and combining the results:
```
ITEM-KEYS 200 GROUPS-OF "HTTP-GET-RECORDS" MAP FLATTEN
```
The idea is that you want to split your keys up into an array of smaller arrays via `GROUPS-OF` and then make your web call on these smaller arrays. When you have all of the responses, you `FLATTEN` the array of arrays to get back a single array of records.

## Triple Double Quotes vs Triple Single Quotes
Technically, there's no difference between triple double quoted strings and triple single quoted strings. However, the convention is that triple double quoted strings (especially multi-line strings) contain Forthic code that will be executed later or in a different context and triple
single quoted strings contain data/content (e.g., html).

## Declarative Style
You can use records to act as lookup tables/case statements to generalize logic. Using `DEFAULT` ensures that all cases are handled.

```
: STATUS>COLOR [
    ["In Progress"   "green"]
    ["Complete"      "blue"]
    ["At risk"       "yellow"]
    ["At high risk"  "red"]
] REC SWAP REC@ "gray" DEFAULT;  # ( status -- color )
```

## Functional Style
Many of Forthic's words already have a functional flavor. For instance `MAP`, `ZIP`, `ZIP-WITH` are inspired by Haskell. Much of Forthic's straight-line code are essentially functional transformations of data in a pipeline.

## Object Style
When writing code that manipulates object-oriented structures, the Forthic you write will take on object-oriented styles. For example, when dealing with the DOM of a web page, you might write code like this:
```
REPORT-DIV SUMMARY-TABLE <APPEND <SHOW <HIGHLIGHT
```
Here the `REPORT-DIV` is an HTML element with several of its methods called. While one might still view this as a pipeline, it's more appropriate to view it as an object sitting on the stack with repeated calls made on it.

## Async Style
When the host language has async features (like JavaScript), it's appropriate to write Forthic in an async style. Here's an example of browser Forthic making an async call to the server using a Promise:
```
"MAIN-CONTENT" SERVER-PROMISE  "ALERT" <CATCH AWAIT
```
In this example, we execute the `MAIN-CONTENT` word on the server via `SERVER-PROMISE` which returns a Promise repreenting an async web request. We can manipulate this promise in the same way that we would in JavaScript. We can chain `THEN` handlers and `CATCH` exceptions. If we need to synchronize on this call, then we use `AWAIT`. The results of the promise are left on the stack once `AWAIT` executes.

## Deferring execution using strings
Because a Forthic program is interpreted from beginning to end (except when defining new words), you can't have code like this:
```
# XXX: INVALID Forthic
CONDITION? IF DO-THIS ELSE DO-THAT THEN
```
In Forthic, both branches of the condition would execute as this was interpreted, which is certainly *not* what you want. Forthic doesn't have an `IF` statement per se (a philosophical design choice), but there are times when you need it. Here's the idiomatic way of coding a condition in Forthic:
```
[[TRUE   "DO-THIS"]  [FALSE  "DO-THAT"]] REC CONDITION? REC@ INTERPRET
```
There are two things of note here. The first is that we use a record in order to implement a case statement containing the outcomes of interest. The second is that we defer the execution of `DO-THIS` and `DO-THAT` by putting them into strings and then calling `INTERPRET` on them when we know which one to execute. Using strings to defer Forthic execution is an important idiom.

### Execution in the context of the browser
Forthic web apps typically have code that runs on the server and code that runs on the browser. Because the code that runs on the browser actually comes from the server, it's natural to write this code in the same file as the server code. To do this, we use multi-line, triple double quoted strings like this:
```
: BROWSER-FORTHIC
"""
: USERNAME   QUERY-PARAMS "username" REC@ CURRENT-USERNAME DEFAULT;
: MAIN-DIV   "main" ID>ELEMENT;
: UPDATE-BUTTON   "update-button" ID>ELEMENT;
: ADD-UPDATE-HANDLER   UPDATE-BUTTON "click" "'Howdy' CONSOLE.LOG" ADD-LISTENER;
"""
;
```
This allows you to have a server code context and a browser code context.

### Execution in the context of the server (from the browser)
The nice thing about having the server and browser code in one file is that you can refer to words in one context from the other and see them nearby in the source code. For example:
```
# ----- Server code -------------------------------------
: GET-PROFILE   CUR-USERNAME USERNAME>PROFILE;

# ----- Browser code ------------------------------------
: BROWSER-FORTHIC
"""
: PROFILE-BUTTON      "profile-button" ID>ELEMENT;
: REFRESH-PROFILE     'GET-PROFILE' SERVER-PROMISE AWAIT RENDER-PROFILE;
: ADD-UPDATE-HANDLER   PROFILE-BUTTON "click" "REFRESH-PROFILE" ADD-LISTENER;
"""
;
```
Here, we execute the `REFRESH-PROFILE` word on the browser when the `PROFILE-BUTTON` is clicked. `REFRESH-PROFILE` will call `GET-PROFILE` on the server via `SERVER-PROMISE`. This ability to execute Forthic in one context from another is a common pattern and is conceptually powerful.

## Use Context to Simplify Code
A context can be as small as knowing that a variable is set to a current, valid value or as big as the environment that a Forthic interpreter is running in. Effective use of contexts is an important concept in Forthic because it leads to simpler, more conceptual code. It allows you to reuse the words you write in delightful ways.

### Create informal contexts using variables
Whenever you use a variable, you are creating an informal context. If you structure your words well, then you can reuse the words that you've written in other parts of the code. For example, if you have a `ticket` variable, then all words that use that are open for use elsewhere:
```
["ticket"] VARIABLES
: TICKET-KEY          ticket @ 'key' REC@;
: TICKET-SUMMARY      ticket @ 'Summary' REC@;
: |TICKET-SUMMARIES   "(ticket !) TICKET-SUMMARY" MAP;

# Later, we can reuse the informal context around the ticket variable:
: TICKET>ROW   (ticket !)
    ["<tr><td>" TICKET-KEY "</td>" "<td>" TICKET-SUMMARY "</td></tr>"] CONCAT;

PULL-TICKETS "TICKET>ROW" MAP
```

### Define formal contexts to re-use sets of words
One example of this is when dealing with organizational hierarchy data. Management and reporting structures change over time. Data collected one quarter (e.g., a survey) may have a different org structure than the same data collected in the next quarter. To generate valid reports requires executing that code in the context that the org was in at the time of collection. By allowing code to run in a context, it is trivial to change the contexts without changing any of the underlying code. This type of re-use is common in Forthic code.

### Blur the lines between contexts
Sometimes the context that Forthic runs in is on different interpreters -- perhaps with different host languages running on different machines (e.g., a server and a browser). Because you can write Forthic in both contexts, conceptually, both sides start to look like a single context. Designing code this way leads to simpler expression because you can be of one mind on the design. Since it's easy to move across contexts, it's easy to treat both sides as one.

### Recreate contexts when needed
In some cases a context that you need to execute Forthic in may no longer exist. For instance, in a web application, the server creates the Forthic context, executes code to render a page, and then serves it to the browser. After the request, the server context is destroyed. If there's Forthic code on the browser that wants to execute a word on the server, we can recreate that context -- exactly -- via a web request back to the server. This allows us to run Forthic words in contexts that don't exist until we need them. This is a simpler, more direct, more conceptual way of viewing API calls.