# jinja_module

The jinja module uses the Python [jinja templating
package](https://jinja.palletsprojects.com/en/2.11.x/) to render arbitrary
strings using record data.

Typically, this is the best choice for rendering html, emails, or anything that
involves looping along with interpolation.

## Example
```
["jinja"] USE-MODULES

: MY-DATA   [
    ["letters" ["alpha" "beta" "gamma"]]
] REC;

: MY-TEMPLATE "
    <ul>
        {% for letter in letters %}
            <li>{{ letter }}</li>
        {% endfor %}
    </ul>
";

MY-TEMPLATE  MY-DATA jira.RENDER
```

## Reference

### RENDER
`(template record -- string)`
Given a jinja template and a Forthic record, renders a string using the jinja engine.

NOTE: The record must have fields that are valid Python variable names since
these are used within the jinja template.