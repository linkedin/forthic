# Forthic React v1

This is a React bundle with a Forthic interpreter inside of it. This allows us to construct
React-based apps dynamically using Forthic to coordinate the creation and configuration of
React elements. React elements can also be defined on the fly in JSX and manipulated by the
Forthic interpreter.

## Using React Elements

## Available Modules

* The [global module](docs/modules/global_module.md) defines words that are always available.
Some of these words are React-specific.
* The [recharts](docs/modules/recharts_module.md) module defines words for constructing charts using [Recharts](https://recharts.org/en-US/)

## Available React Elements
* [ForthicPage](docs/elements/ForthicPage.md) provides a structure for updating
based on query param changes. It can specify default query params. It also maintains page state.
* [RecordsTable](docs/elements/RecordsTable.md) can render an array of records as a table. The full
information from the records is available within the table and can be manipulated with Forthic
* [TicketsModal](docs/elements/TicketsModal.md) is a higher level component that can be used to
present ticket records on the fly. It also has the ability to email assignees/managers when configured.
* [UserNav](docs/elements/UserNav.md) provides a way to navigate up the org given a username as well as search for people within the org to select.