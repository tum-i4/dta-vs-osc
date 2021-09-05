# Self-Checksumming Virtualization Obfuscation

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

What things you need to install the software and how to install them

- CMake >= v3.4.3

### Installing

```
mkdir sc-virt-build && cd sc-virt-build
cmake ../sc-virt/src
make
```

### Marking Functions

To mark functions for virtualization in your source code add the `__attribute__((sc_virtualize))` attribute to it.
For C++, instead of `__attribute__((sc_virtualize))`, you can also use `[[clang::sc_virtualize]]`, however, that one requires the `-std=c++11` option.

To specify functions that should be checked, add the `__attribute__((sc_sensitive))` attribute. Functions marked sensitive are implicitly marked virtualize.
However, you can still put both attributes on your function, if you want to be explicit about it.

### Example

Take a sample program:
```
#include <cstdio>

__attribute__((sc_virtualize))
static void say_hello() {
    printf("Hello World!");
}

int main() {
    say_hello();
    return 0;
}
```

and compile to LLVM bitcode
```
clang++ -c -emit-llvm main.cpp -o main.bc
```

Now use `opt` to run the `reg2mem` pass first, and then the virtualization pass on it:
```
opt -reg2mem < main.bc > prep.bc
opt -load LLVMScVirt.so -scvirt [-connectivity=X] < prep.bc > out.bc
```

The connectivity parameter indicates the number of checkers for each function marked sensitive.

Last, you can compile the code to a native binary
```
clang++ -o main.out out.bc
```