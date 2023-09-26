# intake-example (DRAFT)
This shows how to create an intake form that can be dynamically configured using
a Google sheet.

## Initial run
- On startup, the app should check for a configured gsheet URL. If none exists, it should prompt them to add one in the config section of the Forthic code. It should also ask them to add their username/email to the admin list.
- If the config URL points to a blank sheet, we should show a button that lets them
set up the config gsheet with some starter content. This will include all of the
control types and some basic things to start.
- Within the config section, we should have an example that shows how to configure
a single sheet.
- If there are multiple forms set up, we'll assume the first one is the default
- If there is only one form, we'll hide the selector control

## Types of controls
There should be the typical controls:
- TextInput
- Date
- Checkbox
- Dropdown
- MultiCheckbox
- Textarea
- Attachment

But we should have ones that are more read-only as well:
- MarkdownContent

## Refreshing configs
- If there's a URL, and the user is an admin, we should have an admin control that
lets them refresh the config.

## Thoughts on customizing the forms



## Questions
### Q: How do I get the current user?

A: We need to create a new CURRENT-USER word that returns an object with a username or email


### Q: How will we handle the case where you have some common questions and you want to show different sets for different options?
A: I think we need to handle this from the Forthic side. We need some way to refer to the fields in the sheet. I think we should have an ID for each field.

### Q: How do we handle multipage forms?
A: We need to handle this in Forthic.