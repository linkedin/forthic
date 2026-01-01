> **⚠️ ARCHIVED - This documentation is from an archived repository.**
> **For current documentation, visit:** https://github.com/forthix/forthic

---

# Overview

Forthic is a stack-based language for building tweakable apps. A Forthic program consists of words executed sequentially. Data is passed between words via a parameter stack. Some words push data, some consume data, some consume and push new data. Here's an example of a Forthic program:
```
ENGINEER EAT SLEEP CODE
```
New words can be defined in terms of existing words and used at higher levels of abstraction:
```
: WORK         EAT SLEEP CODE;
: STAFF-CODE   STAFF "WORK" MAP;
: STAFF-LOC    STAFF-CODE CONCAT  NEWLINE SPLIT LENGTH;
: STAFF-KLOC   STAFF-LOC 1000 /;

STAFF-KLOC
```
This captures the feeling of a Forthic program:

* Words defined in Forthic should be **one line long**
* Since word definitions grow horizontally rather than vertically, each line of code is expressive
* It should be easy to figure out what a word does
* It should be easy to see that a word is correct
* Since arguments are implicitly passed via the stack, definitions look like unix pipelines (but without the pipes)
* A succession of definitions can rapidly raise the level of abstraction
* Forthic programs flow in straight lines

## Why Forthic?
Forthic allows applications to be written at a level of abstraction high enough that end users are able to tweak them. This enables Forthic applications to be treated more like spreadsheets than dedicated tools, allowing users to quickly create and customize their own solutions.

## Isn't this just FORTH?
No. Forthic is inspired by FORTH, but it has very different goals. FORTH wants to be the top, middle, and bottom of the application, all the way down to the metal. Forthic just wants to be the top of the app, the part that's closest to the user. Forthic does this by requiring the **host language** that it's implemented in to take care of the middle and bottom of the app. This enables Forthic to dispense with low-level code and focus entirely on higher-level concepts.

FORTH is geared towards numerical computation and is most comfortable with integers--in fact its parameter stack only holds integers. Its syntax for strings is...awkward...as is manipulating them. Forthic was designed with Web APIs in mind and is very comfortable dealing with strings. And, naturally, it is very comfortable with whatever its host language is comfortable with. In Python and JavaScript, the Forthic stack can hold anything a Python/JavaScript array can hold (which is basically anything).

FORTH is focused on efficiency. Forthic is focused on tweakability.

## If Forthic's host language is Python/JavaScript, why not just use Python/JavaScript?
There are two main benefits to using Forthic to build apps rather than its host language. The first is related to code complexity. By splitting an app into a high-level piece and a lower-level piece, you literally divide the complexity of the code. The high-level Forthic code stays more conceptual and "straight line" while the host language deals with all of the jags and special cases. Since the host language is focused on implementing focused, specific words, the complexity is constrained. Because there's a clear line between high-level and low-level code, it's easy to keep both sides simpler.

The second is being able to blur the lines between *where* an app is executing. Consider a web application where some of the app executes on the server (when the initial page is generated and when API calls are handled), and some executes in the browser's JavaScript engine. Although the code is running in different places at different times, it's conceptually a single app. When Forthic is the language that coordinates both server and browser code, it's easy to maintain this view of a single app, especially when you call server-defined Forthic words from the browser (and vice versa). A single level of abstraction covers both execution contexts, leading to a simpler, conceptual view of the app.

## What is Forthic bad at?
Forthic is not meant for building libraries and infrastructure. It's focused on building apps and making apps flexible and tweakable. It's best at coordinating code conceptually, not at building underlying functionality. The underlying functionality (as well as any library/infrastructure work) should be done in the host language (or lower).

Forthic is not focused on performance *per se*. Trying to write numerical algorithms directly in Forthic is a bad idea--it's not meant for that. The Forthic way of doing this is to move the numerical algorithms into the host language and manipulate them at a higher level in Forthic.

## What is Forthic good at?
If you were building a video game, Forthic would be the level designer and its host language would be the game engine. Forthic is great at moving chunks of data and functionality around and combining them at a high level.

Forthic is excellent at pulling data from gsheets and Jira and then rolling up data by org hierarchy from LDAP to build small, focused tools like wiki-based status reports, dynamic dashboards, automated emails, and Jira intake forms. Forthic plays very well in the gaps between information systems and excels at flexibly bridging them.

## Features
* Forthic interpreter with Python as a host language
* Forthic interpreter with JavaScript as a host language (meant to run in browser)
* Standard Forthic modules
    * [`global`](./modules/global_module.md): Words common across all apps: using modules, manipulating arrays and records, string functions, date/time functions, math, profiling
    * [`cache`](./modules/cache_module.md): Store app data in between runs
    * [`confluence`](./modules/confluence_module.md): Create and update wiki content
    * [`datasets`](./modules/datasets_module.md): Store, retrieve, and update records organized in datasets
    * [`excel`](./modules/excel_module.md): Manipulate Excel data via MS Graph API
    * [`gsheet`](./modules/gsheet_module.md): Manipulate Google Sheets data via Google API
    * [`html`](./modules/html_module.md): Construct html based on DOM model
    * [`jinja`](./modules/jinja_module.md): Render content using jinja templating
    * [`jira`](./modules/jira_module.md): Search Jira, pull changelogs, create and update tickets
    * [`org`](./modules/org_module.md): Roll up data by org hierarchies