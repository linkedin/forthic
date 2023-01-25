# UserNav

The `UserNav` implements a breadcrumb-oriented user selector. For the selected user, it shows breadcrumbs for each manager in their chain up to the root manager. It also has a user search typeahead control.

## Props

* `breadcrumbLabelKey`:  The user field to display in the breadcrumb
* `typeaheadLabelKey`:  The field of the search result records that's used to match typeahead input
* `fsearch`: Forthic that takes a string and returns an array of records and is executed when typeahead is active
* `fselected`: Forthic that is executed when a user is selected
* `mgr_chain`: List of records representing the current management chain. Each record must have `username` and `fullname` fields
* `interp`: Forthic interpreter. This is automatically set by the global module.

## Examples

### Simple Example
```
: USER-NAV   "UserNav" Element [
    ["fsearch"             "PERSON-SEARCH"]
    ["typeaheadLabelKey"   "label"]
    ["breadcrumbLabelKey"  "fullname"]
    ["fselected"           "'username' QPARAM!"]
    ["mgr_chain"           PAGE-DATA "manager_chain" REC@]
] REC <PROPS;
```