# Threat Model

## Application Architecture
See [ARCHITECTURE.md](./ARCHITECTURE.md)

## What does the Product/Application do?
See [OVERVIEW.md](./OVERVIEW.md)

## Assumptions
To run Forthic securely, please adhere to the following assumptions.

### Using the Example App Server
**Assumption**: Do not share your `.secrets` and `.key` files

Forthic is a programming language with modules for building single-purpose tools. Any data that users access must be done via the standard modules (e.g., `jira`, `gsheet`, `excel`). These modules typically use OAuth to authorize access to data from those platforms. Some services use username/password. In the vanilla Forthic install, secrets are encrypted and stored in a `.secrets` file on disk. A `.key` file is automatically generated to perform the encryption.

If this assumption is violated, then a user's credentials are compromised.

**Assumption**: Only run and access the vanilla web apps locally

The vanilla Forthic install is meant to be used for app development only. It is **not** meant to be used in a production environment. Forthic apps typically run as web apps. When access to 3rd party data is required, there is an on-demand credential flow that adds records to the `.secrets` file. These credentials are provided via a web browser running on localhost. A web browser is used because OAuth flows require it. 

If this assumption is violated, by running the vanilla setup on a server, this would expose secrets over http.

**Assumption**: Do not run the vanilla web server with `--host=0.0.0.0`

During app development, sensitive data may be retrieved, stored, and made accessible via the vanilla web server. This is a flask server running with the default host settings. In this mode, it is not accessible outside of the user's local computer.

If this assumption is violated, this would allow anyone with network access to the user's computer to access any data stored by the web apps or that the web apps could access from 3rd parties.

### Using Forthic

**Assumption**: Ensure that **no user content** is executed directly by `INTERPRET`

`INTERPRET` executes arbitrary Forthic strings. It is an integral part of Forthic apps because it allows deferred execution of Forthic code. A Forthic app should be designed so that only developer content is executed directly by `INTERPRET`. For cases where the app must execute user content (e.g., using the Forthic interpreter to parse user-specified report filters), the Interpreter that executes it must be sandboxed so that it is powerless to do anything but perform a parse.

**Assumption**: User content that is an argument in a Forthic string must be `QUOTED`

The `QUOTED` words uses an untypeable ASCII character (`DLE`) as a quote character to safely quote an arbitrary string (after first stripping out `DLE` characters from it). Any data that comes from the user must by quoted in this way in order to prevent Forthic injection attacks.

## Trust Boundaries
Forthic apps typically run as web apps and so have the typical trust boundaries: end user to app server, app server to 3rd party servers (Jira, Confluence, Google, MS Graph).


## Risks
The most likely risk is for a user to try running the vanilla install so it's accessible to others. The correct way to do this is to build a proper production flask server and port any modules that make assumptions about storing files to disk:

* Credential management
* `cache` module
* `datasets` module