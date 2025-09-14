# Colourful Programming language Documentation 
<a id="top"></a>

## Topics
* [Compiler Cli](#cli)
* [Running example programs](#cli-ex)
* [Exceptions](#errors)
* [Programming with Colours](#colour-use)
* [Colour use examples](#colour-use-ex)
* [Comments](#comments)
* [Commments Examples](#comments-ex)
* [Defining Colours](#colour-def)
* [Using Colour Definitions](#colour-def-ex)
* [Looping](#loop)
* [The Y combinator](#y)
* [Encoding Booleans](#bool)

<a id="cli"></a>
## Compiler cli

Clone or download the repo. Make sure you have [Stack](https://docs.haskellstack.org/en/stable/) installed.

It might be possible to use the repo with cabal directly, but it hasn't been tested.

Start the interface by typing `stack run`

enter "quit" to exit or one of the following commands:


`transpileREADME`

transpiles the README to an SKI expression.

Please do not try to evaluate the README file. It looks like it goes into infinite recursion. This might be a bug, but there are genuine Colourful programs that go into infinite recursion. Transpiling the README works fine though.

`transpileFile <filepath>`

transpiles the file at the provided filepath to an SKI expression

`evalFile <filepath>`

transpiles the file at the provided filepath to an SKI expression and evaluates it to a reduced SKI expression

`backtranspileFile <filepath>`

transpiles the file at the provided filepath to an SKI expression, evaluates the SKI expression and transpiles the resulting SKI expression back to a colourful expression

`transpile <input string>`

transpiles the input string to an SKI expression

`eval <input string>`

transpiles the input string to an SKI expression and evaluates it to a reduced SKI expression

`backtranspile <input string>`

transpiles the input string to an SKI expression, evaluates this SKI expression and transpiles it back to a Colourful expression

[Back to top](#top)

<a id="cli-ex"></a>
### Examples 

example programs are provided in the examples folder.

A program representing the expression `False and True` is provided in the file "andExample.colour" and can be transpiled by entering:

`transpileFile examples/andExample.colour`

or evaluated by entering:

`evalFile examples/andExample.colour`

or transpile the result back to a Colourful expression:

`backtranspileFile examples/andExample.colour`

The file contains a text consisting mostly of comments which explains what is going on in the program. the `backtranspileFile` option might be most intuitive.

Another example that also corresponds to False and True, but uses user-defined Colours to actually
call the boolean literals `False` and `True` is the file andExampleWithDef.colour.
Transpile it like this:

`transpileFile examples/andExampleWithDef.colour`

or evaluated by entering:

`evalFile examples/andExampleWithDef.colour`

or transpile the result back to a Colourful expression:

`backtranspileFile examples/andExampleWithDef.colour`

[Back to top](#top)

<a id="errors"></a>
### Exceptions/Errors

While a Colourful program as such should never throw errors or crash (although it may never halt), the compiler cli *will* throw errors or crash under certain circumstances. A few examples are:
* trying to transpile or evaluate a file that doesn't exist
* entering an invalid command
* not providing the correct number of arguments required by the command
* not properly terminating a string
<a id="colour-use"></a>
## Using Predefined Colours

To use the existing colours, all you need to do is write the Colours in the reverse order in which you woud like them to be applied.

[Back to top](#top)

<a id="colour-use-ex"></a>
### Example of Colour use

`Red Green Yellow`

applies Yellow to Green, and the result to Red.

[Back to top](#top)

<a id="comments"></a>
## Using Comments

Everything that is not either a pre-defined or user-defined colour is treated as a Comment. Comments can appear anywhere in the program, including inside Colour definitions. 

[Back to top](#top)

<a id="comments-ex"></a>
### Comment Example

`This is a comment with no colours`

is a comment that does not evaluate to anything.

`The traffic lights use the colours Red, Green and Yellow`

is a program in which only `Red Green Yellow` is interpreted and the rest of the characters are treated as comments. So this program is equivalent to the example above.

[Back to top](#top)

<a id="colour-def"></a>
## Colour definitions

Defining your own color uses the Colours Black and White.

`Black <Definition> <Name> White` 

is the syntax for Colour Definitions.

Colours have to be defined *after* they are used.

[Back to top](#top)

<a id="colour-def-ex"></a>
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

[Back to top](#top)

<a id="loop"></a>
## Looping

There are no explicit loops in Colourful. However, as you are probably aware, anything that can be done with a loop can also be achieved by recursion.

There is no explicit recursion in Colourful either.
[Back to top](#top)

<a id="y"></a>
### The Y combinator

The [y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Y_combinator) is a fixed point combinator which allows you to add recursion to a language that doesn't have it, most famously the [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus).

Haskell Curry (yes, this is *the* Haskell Curry that a certain programming language is named after) defined Y in combinatory logic as follows:

`Y = BM(CBM)`

Another fixed-point combinator in combinatory logic was defined by Alan Turing (probably doesn't need an introduction):

`Y_Turing = M(B(SI)M)`

Although Haskell Curry's Y is probably more commonly thought of at *the* Y combinator, I'm using Turing's here as it translates to a more compact expression in Colourful.

Either way, it still requires defining a custom colour definition.

The Y combinator can be expressed in Colourful as shown in the y_combinator.colour file in the examples folder. Transpile this file to an SKI version of the Y combinator by:

`transpileFile examples/y_combinator.colour`

Don't evaluate or backtranspile this file - it will go into infinite recursion!

#### Y combinator example

The examples folder also contains a file which has the y combinator applied to Orange. This will halt when run, so feel free to run:

`backtranspileFile examples/y_combinator_orange.colour`

#### How it works

The Y combinator takes as an input a combinator and returns its least fixed point. A fixed point x of a function f is a point at which f(x) = f (f(x)).


The combinator Orange takes an argument, discards it, and returns Yellow. The expression ` Yellow Orange` evaluates to `Yellow`. Therefore, Yellow is a fixed point for Orange. The example file y_combinator_orange.colour applies the Y combinator to Orange and evaluates to Yellow. Witness this by running:


`backtranspileFile examples/y_combinator_orange.colour`

[Back to top](#top)

<a id="bool"></a>

### Encoding Booleans

The boolean literals "True" and "False" as well as the usual boolean operators such as "AND", "OR", "NOT" are not predefined in Colourful. If you intend to write a programm using these constructs, they have to be encoded using colours. There are potentially multiple ways to do this that are valid. Here, we use:

* True: Red
* False: Orange
* a AND b: Orange b a 
* a OR b: b Red a

This does not have to be made explicit, but we can make it explicit by defining our own colours:

`Black Red True White`

`Black Orange False White`

`Black Orange AND White`

`Black Red OR White`

Now we can write expressions like AND True True or False OR True. However, we would have to do that before the definitions, so in a real program, this last sentence should have been above the definitions. I'm just putting it here for the purpose of the narrative. 

#### Verifying our Boolean encodings

To verify our encoding works as expected, we can go through the truth table:

| a        |b      |expected: a and b|AND b a|
| ------ | ----- |------|------ |
| True     | True     | True | True|
| False   | True        |False| Orange|
| True  | False       |False| Orange|
| False   | False      |False| Orange|

Similarly, for or:

| a        |b      |expected: a or b|b OR a|
| ------ | ----- |------|------ |
| True     | True     | True | True|
| False   | True        |False| True|
| True  | False       |False| True|
| False   | False      |False| Orange|

When we remind ourselves that Orange corresponds to False, these behave as expected.

#### Why this works

Why does this work?

Consider the following Haskell code:

`and a b = if a then b else False`

We can define and like that as we expect this behaviour from and: if a is True, we have to check what b is - if it is True, a and b is True, so we return True. If it is False, we return false. Or, in other words, we return b, which is what the above function does.
If a is False, we do not need to check what b is, because we already know that a and b is False, regardless of b, so we can just return false, which is what the else branch above does.

Great, but what does this have to do with Orange and Red?

Recall that Red is defined like this:

y x Red = x

It returns the argument to its left, i.e. it's first argument given the backwards evaluation order of Colourful. 

If a is True, we can replace it whith Red, as that's how we encoded True. So the expression `AND b a ` corresponds to `AND b Red`, which evaluates to b. This is equivalent to what the code above does when a is True - it returns b.

If a is False, we can replace it with Orange. `False and True` corresponds to `AND b Orange`. Recall that Orange is defined as Yellow Red (Red applied to Yellow). This is a combinator that takes and argument x, discards it, and always returns Yellow. So `AND b Orange` discards the b and returns `AND Yellow`. We know that Yellow is defined as:

x Yellow = x

So this expression evaluates to `AND` which is encoded as Orange, aka False.

So this code does the same as the else branch in the Haskell code above, i.e. return False.  

The same argument can be applied to OR:

We can define a Haskell function:

`or a b = if a then True else b`

You should be able to convince yourself that this is how or should behave, and following the same logic as above, you can convince yourself that our encoding is correct. 

[Back to top](#top)