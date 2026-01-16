## Features

### Language features

- [x] Integers
- [x] Booleans
- [ ] Chars
- [ ] Floats
- [x] Strings
- [x] Enums (integer or string based)
- [x] Pointers
- [x] Typedefs
- [x] Functions
- [x] Conditions (if, else, else if)
- [ ] Switches
- [x] Loops (for, for-in, while)
- [ ] Records
- [ ] Arrays
- [ ] Generics

### Example

```
fn collatz(x: s64) -> s64 {
    steps := 0
    while x != 1 {
        steps += 1
        if !(x % 2) {
            x /= 2
        }
        else {
            x = 3 * x + 1
        }
        print("step {}: {}\n", steps, x)
    }
    return steps
}

return collatz(42) as s32
```

### Compiler features

- Diagnostics with location and line printing
- SSA conversion
- Constant folding
- Dead code elimination
- Inlining

## Build

Only Linux builds are supported. Parts of the code are working on Windows,
but it currently does not build with cl.exe. clang-cl might work.

The project should build with C++23-capable versions of clang and gcc,
although clang is preferred. If both are installed, xmake will ask which one to use.

1. [Download xmake](https://github.com/xmake-io/xmake/?tab=readme-ov-file#installation) (on Arch Linux, run `sudo pacman -S xmake`)
2. `git clone https://github.com/lstrsrt/compiler`
3. `cd compiler`
4. `xmake`

This builds in Release mode.
To switch to a Debug build, run `xmake config -m debug` before `xmake`.

To run: `./build/{debug,release}/compiler [args...]`.
Pass `--help` for help.

The `--exe` flag and tests require [nasm](https://www.nasm.us) and gcc to be installed.
