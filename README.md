# Forthic

Forthic is a stack-based language for writing tweakable applications by coordinating code written in a host language.

## Documentation
For a brief overview of Forthic, see [OVERVIEW.md](docs/OVERVIEW.md). The [ARCHITECTURE.md](docs/ARCHITECTURE.md) provides more technical detail about Forthic, including a brief overview of some of the standard global Forthic words. The [IDIOMS.md](docs/IDIOMS.md) file gives pointers on how to use Forthic the way it was designed to be used. Also see the [THREAT_MODEL.md](docs/THREAT_MODEL.md) file for guidance on running Forthic securely.

To build browsable documentation (including the Forthic module docs):
```
make docs
```
Then load `./build/sphinx/html/index.html`

## Getting started
```
make
```
This will create a Python virtual environment, install Forthic into it, and run a
web server on port 8000 that can run some sample applications.

## Examples
The Forthic examples run as web apps. The urls for each sample app have the same structure, which matches the directory structure:
```
http://localhost:8000/examples/<example>
```
For instance, to see the `ex_cache` example, go to:
```
http://localhost:8000/examples/ex_cache
```

See [EXAMPLES.md](docs/EXAMPLES.md) for more info about each example.


## Tests
```
# Tests the Python Forthic interpreter
make test

# Tests the JavaScript Forthic interpreter
make test-js

# Tests both
make test-all
```
