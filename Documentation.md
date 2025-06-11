# Colourful Programming language Documentation 

## Compiler cli

Clone or download the repo. Make sure you have Stack installed.

Start the interface by typing `stack run`

enter "quit" to exit or one of the following commands:


`transpileREADME`

transpiles the README to an SKI expression

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