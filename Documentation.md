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
Another example that also corresponds to False and True, but uses user-defined Colours to actually
call the boolean literals `False` and `True` is the file andExampleWithDef.colour.
Transpile it like this:

`transpileFile examples/andExampleWithDef.colour`

or evaluated by entering:

`evalFile examples/andExampleWithDef.colour`

### Exceptions/Errors

While a Colourful program as such should never throw errors or crash (although it may never halt), the compiler cli *will* throw errors or crash under certain circumstances. A few examples are:
* trying to transpile or evaluate a file that doesn't exist
* entering an invalid command
* not providing the correct number of arguments required by the command
* not properly terminating a string

## Using Predefined Colours

To use the existing colours, all you need to do is write the Colours in the reverse order in which you woud like them to be applied.

### Example of Colour use

`Red Green Yellow`

applies Yellow to Green, and the result to Red.

## Using Comments

Everything that is not either a pre-defined or user-defined colour is treated as a Comment. Comments can appear anywhere in the program, including inside Colour definitions. 

### Comment Example

`This is a comment with no colours`

is a comment that does not evaluate to anything.

`The traffic lights use the colours Red, Green and Yellow`

is a program in which only `Red Green Yellow` is interpreted and the rest of the characters are treated as comments. So this program is equivalent to the example above.

## Colour definitions

Defining your own color uses the Colours Black and White.

`Black <Definition> <Name> White` 

is the syntax for Colour Definitions.

Colours have to be defined *after* they are used.

### Examples of Colour Definitions

`Mycolour Black Red Mycolour White`

will be interpreted as Red, but

`Black Red Mycolour White Mycolour`

will not evaluate to anything as Mycolour defined *before* it is used.

`Myothercolour Black Blue Mycolour Myothercolour White Black Red Mycolour White`

is interpreted as Blue Red, because the definitions are in the correct order.

But in the program

`Black Red Mycolour White Black Blue Mycolour Myothercolour White`

Myothercolour is not defined, because it uses Mycolour in its definition, which is defined *before* it, so it can't be used in the definition of Myothercolour.

