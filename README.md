# Forthic

Forthic is a stack-based language for making apps tweakable.

## Documentation

For a brief overview of Forthic, see [OVERVIEW.md](docs/OVERVIEW.md).
The [SYNTAX.md](docs/SYNTAX.md) file shows what the language looks like, including a brief overview of some of the standard global Forthic words.
The [IDIOMS.md](docs/IDIOMS.md) file gives pointers on how to use Forthic the way it was designed to be used.
The [ARCHITECTURE.md](docs/ARCHITECTURE.md) file shows how Forthic is meant to be embedded in your apps.

## Getting started

```
# On Mac and Linux
make

# On Windows
.\make-install.ps1
.\make-server.ps1
```

This will create a Python virtual environment, install Forthic into it, and run a
web server on port 8000 that can run some sample applications.

## Examples

The Forthic examples run as web apps. To see a list of the examples run the server using `make` and then go here: [http://localhost:8000](http://localhost:8000)

See [EXAMPLES.md](docs/EXAMPLES.md) for more info.

## Tests

```
# Tests the Python Forthic interpreter
make test

# Tests the JavaScript Forthic interpreter
make test-js

# Tests both
make test-all
```

## Deleting secrets

All credentials are stored encrypted in a JSON object in the `server/.secrets` file. To delete a particular secret, just remove it from the JSON record
and save the file. To delete all secrets along with the encryption key, delete `server/.secrets` and `server/.key` or

```
# On Mac and Linux
make delete-secrets

# On Windows
make-delete-secrets.ps1
```

## YouTube

-   [Coding Forthic with Rino](https://www.youtube.com/@codingforthic)

## Articles

-   [Categorical Coding](https://forthix.com/category/categorical-coding/)
-   [Forthic How To](https://forthix.com/category/how-to/)
-   LinkedIn Article on how to use the Jira module https://www.linkedin.com/pulse/hello-forthic-abdul-sheik
