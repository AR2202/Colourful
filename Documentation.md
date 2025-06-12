# Colourful Programming language Documentation 

## Compiler cli

Clone or download the repo. Make sure you have Stack installed.

Start the interface by typing `stack run`

enter "quit" to exit or one of the following commands:


`transpileREADME`

transpiles the README to an SKI expression.

Please do not try to evaluate the README file. It looks like it goes into infinite recursion. This might be a bug, but there are genuine Colourful programs that go into infinite recursion. Transpiling the README works fine though.

`transpileFile <filepath>`

transpiles the file at the provided filepath to an SKI expression

`evalFile <filepath>`

transpiles the file at the provided filepath to an SKI expression and evaluates it to a reduced SKI expression

`transpile <input string>`

transpiles the input string to an SKI expression

`eval <input string>`

transpiles the input string to an SKI expression and evaluates it to a reduced SKI expression

### Examples 

example programs are provided in the examples folder.

A program representing the expression `False and True` is provided in the file "andExample.colour" and can be transpiled by entering:

`transpileFile examples/andExample.colour`

or evaluated by entering:

`evalFile examples/andExample.colour`

The file contains a text consisting mostly of comments which explains what is going on in the program.

### Exceptions/Errors

While a Colourful program as such should never throw errors or crash (although it may never halt), the compiler cli *will* throw errors or crash under certain circumstances. A few examples are:
* trying to transpile or evaluate a file that doesn't exist
* entering an invalid command
* not providing the correct number of arguments required by the command
* not properly terminating a string
