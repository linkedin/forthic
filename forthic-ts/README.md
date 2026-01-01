> **⚠️ DEPRECATED PACKAGE**
>
> This package is from an archived repository and will not receive updates.
>
> **New repository:** https://github.com/forthix/forthic-ts
>
> Watch the new repository for announcements about updated packages.

---

# @forthic/interp

A Forthic interpreter that runs within TypeScript.

## Description

This package provides a Forthic interpreter that allows you to execute Forthic code within your JavaScript/TypeScript projects. Forthic is a stack-based programming language inspired by Forth.

## Installation

You can install the package using npm:

```sh
npm install @forthic/interp
```

Or using yarn:

```sh
yarn add @forthic/interp
```

## Usage
Here's a basic example of how to use the Forthic interpreter:

```js
import { Interpreter } from "@forthic/interp"


(async () => {
    const interp = new Interpreter();
    await interp.run("[1 2 3] '3 *' MAP");
    const result = interp.stack_pop();
    console.log("Howdy!", {result});
})();

// Output:
//
// Howdy! { result: [ 3, 6, 9 ] }
```

## License
This project is licensed under the BSD-2-Clause License see the [LICENSE](./LICENSE) file for details.

## Author
Rino Jose

## Links
- [Repository](https://github.com/linkedin/forthic/forthic-ts)
- [Issues](https://github.com/linkedin/forthic/issues)