This is the compiler I wrote as a part of my tutorial on writing a very simple compiler.

Currently only supports the X86_64 architecture and the Linux kernel.

To compile, just  `gcc main.c -o simple_compiler`.

Example usage `./simple_compiler example_programs/fib.uc`

You can pass arguments to the main method like this:

`./simple_compiler example_programs/calculator.uc "10 * 23"`
