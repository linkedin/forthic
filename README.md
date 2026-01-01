> **⚠️ ARCHIVED REPOSITORY**
>
> This repository has been archived and is no longer maintained.
>
> **The Forthic project has moved to individual repositories:**
> - **Index/Overview:** https://github.com/forthix/forthic
> - **Python Runtime:** https://github.com/forthix/forthic-py
> - **TypeScript Runtime:** https://github.com/forthix/forthic-ts
> - **Ruby Runtime:** https://github.com/forthix/forthic-rb
>
> All future development happens in the new repositories. Published packages will continue to work.
>
> See [MIGRATION.md](MIGRATION.md) for details.

---

# Forthic

Forthic is a stack-based language for making apps tweakable.

By embedding a Forthic interpreter in your application, you can make apps tweakable at runtime, even by end users.
LinkedIn has used this approach to build hundreds of internal Jira-based program management tools.

This repository contains two primary Forthic interpreters: a Python-based based one that can run within a Flask app in order to create
APIs that can be revised at runtime and a React-based one that can create user interfaces on-the-fly.

## Documentation

For a brief overview of Forthic, see [OVERVIEW.md](docs/OVERVIEW.md).
The [SYNTAX.md](docs/SYNTAX.md) file shows what the language looks like, including a brief overview of some of the standard global Forthic words.
The [IDIOMS.md](docs/IDIOMS.md) file gives pointers on how to use Forthic the way it was designed to be used.
The [ARCHITECTURE.md](docs/ARCHITECTURE.md) file shows how Forthic interpreters can work within apps.

Forthic modules are documented in the [modules](./forthic-py/docs/)

### YouTube

There are several YouTube videos for learning Forthic

-   [Coding Forthic with Rino](https://www.youtube.com/@codingforthic) goes over some of the example applications
-   [Learning Forthic](https://www.youtube.com/playlist?list=PLSnCkfp4FIBQJEM9SNeGLjt_VrPrHMzQF) teaches Forthic using [Forthix Jira Plugins](https://marketplace.atlassian.com/vendors/1225195/forthix-llc) and Jupyter notebooks.

### Articles

-   [Forthic How To](https://forthix.com/category/how-to/)
-   [LinkedIn Article on how to use the Jira module](https://www.linkedin.com/pulse/hello-forthic-abdul-sheik)
-   [Categorical Coding](https://forthix.com/category/categorical-coding/)

## Getting started

To get started, you can run an example Flask server with an embedded Forthic interpreter that
also serves React apps with embedded Forthic interpreters.

### Starting the example server

```
# On Mac and Linux
make

# On Windows
.\make-install.ps1
.\make-server.ps1
```

This creates a Python virtual environment, installs Forthic into it, and runs a
web server on [http://localhost:8000](http://localhost:8000)

See [EXAMPLES.md](docs/EXAMPLES.md) for more information.

### Deleting secrets

Some examples require credentials in order to work (e.g., Jira username and password/api-token).
The example server stores these on your computer as an encrypted in a JSON object in the `server/.secrets` file. To delete a particular secret, just remove it from this file and save. To delete all secrets along with the encryption key, delete `server/.secrets` and `server/.key` or

```
# On Mac and Linux
make delete-secrets

# On Windows
make-delete-secrets.ps1
```

## Tests

Each Forthic interpreter variant has its own test suite. To run the primary tests for `forthic-py` and `forthic-react` just run

```
make test
```

## Experimental Interpreters

In addition to the primary interpreters for Python and React, there are a number of experimental interpreters that
show how Forthic can be implemented in other languages.
Those who like learning different programming languages might find it interesting to compare the different implementations.
These proto-implementations can also serve as starting points for complete Forthic interpreters.

| Host Language                         | Comments                                                                                   |
| ------------------------------------- | ------------------------------------------------------------------------------------------ |
| [TypeScript](./forthic-ts/)           | Essentially complete and available on NPM at [@forthic/interp](https://www.npmjs.com/package/@forthic/interp) |
| [Ruby](./forthic-rb/)                 | Essentially complete|
| [C++](./experimental/forthic-cpp/)    | Proof of concept using C++ in .NET                                                         |
| [Haskell](./experimental/forthic-hs)  | An experiment to see if a Forthic interpreter could be built in a pure functional language |
| [Julia](./experimental/forthic-jl)    | Proof of concept in Julia                                                                  |
| [nvcc](./experimental/forthic-nvcc)   | Proof of concept for building a interpreter that could run on GPUs via CUDA                |
| [swift](./experimental/forthic-swift) | An attempt to build macOS apps that could be tweaked at runtime                            |
| [Rust](./experimental/forthic-rs)     | A Forthic tokenizer in Rust                                                                |
| [zig](./experimental/forthic-zig)     | WIP Forthic implmentation using zig                                                        |

### Pre-forthic Implementations

Prior to Forthic, there were several experiments at building FORTH-like interpreters in a variety of languages
| Host Language | Comments |
| ------------- | -------- |
| [asm](./experimental/pre-forthic/forrth-asm/) | Proof of concept in assembly language |
| [C#](./experimental/pre-forthic/forrth-cs/) | Proof of concept in C# |
| [Erlang](./experimental/pre-forthic/forrth-erl/) | Proof of concept in Erlang, which was one of the easiest Forth-like implementations |
| [Fortran](./experimental/pre-forthic/forrth-f90/) | Proof of concept in Fortran, which was the hardest Forth-like implementation |
