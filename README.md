# lambda-calculus

This is an unnamed **esoteric programming language** implemented as a preprocessor to raw, untyped *(subject to change)* **Lambda Calculus** 
implemented in Rust without any 3rd-party libraries.

## Examples

Examples can be found in the [examples](./examples) directory. They can be executed using:

```
$ cargo run -- ./examples/<example>.lc
```

or, with the executable directly:

```
$ /path/to/lambda-calculus ./examples/<example>.lc
```

## Expressions

Base expressions of lambda calculus:

- **`λx.y`** (or `\x.y`): Function/Abstraction with an argument `x` that resolves to `y`.
- **`x y`**: Function application.
- **`x`**: Variable (substituted on function application).

Additional expressions:

> The following expressions are implemented in the preprocessor and get expanded to one (or more) of the 3 expressions above before the reduction process.

- **`0-9*`**: Numbers -> expanded to church numerals (`λf.λx f (f (f ... (f x)))`)
- **`[a b c]`**: Lists -> expanded to linked lists (`pair a (pair b (... (pair x nil)))`)

## Statements

- **`include "file.lc";`**: File include; Adds the file `file.lc` to the current context.
- **`let x = y;`**: Let-bindings; All occurances of `x` get substituted with `y` before the reduction process.
- **`print x;`**: Print statement; Reduces `x` and prints it.
- **`assert x, "<message>";`**: Assert statement; Reduces `x` and checks it against `true` (see `prelude.lc`). If the expression does not match, execution fails with the message `<message>`.

## Standard Library

> Core functions can be found in the [prelude.lc](./prelude.lc) file.

## License

This project is licensed under the MIT License. See [LICENSE](./LICENSE) for more information.

