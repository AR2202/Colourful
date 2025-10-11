# Colourful

Welcome to Colourful - the language that brings colours into programming!

## Overview

Colourful is an esoteric/satirical programming language based on [combinatory logic](https://en.wikipedia.org/wiki/Combinatory_logic). Instead of single letters, combinators are named after colours. Basic combinators are pre-defined as keywords in the language. User defined colours can be introduced *after* they are used. Everything that is not a valid colour is a comment, every string is a syntactically valid program. The language has "reverse lexical scope", i.e. everything has to be defined *after* it is used. Evaluation happens bottom to top and right to left. 

The language was designed as a project in a 2-week programming languages jam.

## Playground

Colourful now has an [online playground](https://ar2202.github.io/Colourful_playground/) that lets you program visually with colours.
## Syntax

In Colourful, only pre-defined or user-defined colours have meaning. All other characters, including parentheses, line breaks, commas, dots, are treated as comments. Comments can appear anywhere in the code, including inside colour definitions. It is therefore possible to write a program as an entire story or poem, with only the colours having meaning. Due to these syntax rules, every program is valid Colourful syntax and it is impossible for syntax errors to occur. In a correct implementation of Colourful, parse errors are not possible either, nor are any compilation errors or runtime exceptions. 

Ideally, only ASCII characters should be used in a Colourful program.

### Pre-defined colours

x Yellow = x (identity colour)

y x Red = x (const colour)

z y x Blue = (z y) (z x)

Orange = Yellow Red

Green = Yellow Blue 

Purple = Blue Red

Pink = Red Red

Cyan = Blue Blue 

Violet = Red Blue

Lime = Yellow Yellow Blue

Teal = Blue Yellow Blue

### Equivalences

Some colours are equivalent and don't need a special definition

Red Yellow = Red 

Blue Yellow = Blue

Yellow Yellow = Yellow

Red Red Blue = Yellow

Blue Red Blue = Yellow

Yellow Red Blue = Yellow

### User defined colours

Colours have to be defined *after* they are used. Colour definitions should be placed at the end of the file, with any definitions that reference other definitions being placed *before* the definitons that it references. It is not strictly required to place definitions at the end of the entire program, but they need to be below any place where they are being used.

Colour definitions are introduced with the colour definition colours Black and White.

The syntax for definitions is:

`Black existingColours newName White`

where 
* existingColours is a sequence of existing colours that make up the definition (in the usual evaluation order, i.e. right to left)
* newName, the word immediately preceeding White, is the name of the new colour, which can be any alphanumeric string except a pre-defined colour or a colour defined later in the program.


newName is not checked for being a valid name commonly used for a colour in English or any other natural language. It should not be a substring or superstring of another colour as this would lead to unexpected behaviour. 

As ususal, additional words within the colour definitions are treated as comments. If there are no valid colours between the Black and White colour definition colours, the definition is ignored. Any Black not followed by White (before the next Black or the end of the Program) and any White not preceeded by Black is treated as a comment.

## Semantics

The semantics of Colourful are based on combinators from combinatory logic, with the following differences:
* in accordance with the theme of the jam, the program is evaluated end to start, so evaluation happens right to left
* there are no parentheses

### No errors or exceptions

Every possible string is a valid Colourful program, there are no errors or exceptions. Due to the absence of any static checks, it is very difficult to write correct programs. The user interface of the compiler could throw exceptions, though, if e.g. the user attempts to compile a file that doesn't exist.

Colour definitions have special syntax. However, syntactically incorrect definitions are just ignored.

### Everything is valid

Due to these evaluation rules, every string (including this README) is a valid Colourful program.

### Everything is one expression

With the exception of colour definitions, the entire Colourful program is a single expression and will be evaluated to a single expression in the SKI combinator calculus.


### Everything is immutable 

Colours are immutable. If re-defined, only the last definition is valid. The pre-defined colours cannot be re-defined. Any such attempt is ignored.

### Colour definitions

Due to the absence of parentheses, a change of evaluation order requires defining a new colour as the (partial) application of one colour to another. All user-defined colours count as being defined BEFORE the pre-defined colours.

Defining aliases for existing colours, either explicitly or implicitly, is allowed. Here, explicitly means assigning an existing colour to a new colour name, whereas implicitly means defining a new colour as an application of a colour to other colours, where there already exists a colour with this definition.

### Encoding literals

As the system only allows application of colours to colours, any literals such as True, False, Integers etc have to be encoded using something analogous to [church encodings](https://en.wikipedia.org/wiki/Church_encoding).

### Example: encoding booleans

Booleans need to be encoded as colours first.

True = Red 

False = Orange

a and b = Orange b a

a or b = b Red a 

So the expression  `True and False` would be represented as `Orange Orange Red`, which, by the evaluation rules, corresponds to Orange, i.e. False. 

But you could instead write:

` I was eating an Orange on my Orange bike, when a car ran the Red light and hit me.`

Which would evaluate to the same result.

Refer to the [Documentation](https://github.com/AR2202/Colourful/blob/main/Documentation.md) for a more detailed explanation of why this encoding works.

### Evaluation rules of user defined colours

All definitions are immutabale, with the *LAST* definition being the relevant one. Pre-defined colours are defined last, *AFTER* all user-defined colours. It is not possible to re-define colours. 

Please do not define colours that are substrings or superstrings of pre-defined or existing user-defined colours. This might mess up the parsing and therefore lead to unexpected behaviour (will be fixed in the future).

### "Reverse lexical scope"

All colours have to be defined *AFTER* they are used. Colours defined earlier in the program cannot be referenced. Colour definitions can only use other colours that are defined later in the program. Pre-defined colours are defined *AFTER* any user-defined colours. They would therefore probably be more adequately named "post-defined colours".

### Turing completeness

Colourful using only pre-defined colours is not Turing complete, due to the lack of parenthesized expressions. It should be  Turing complete with user-defined colours. A proof is not provided here.

## Compilation

Colourful compiles to an expression in the [SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus), which is the output of the program.

Optionally, this could be evaluated by an interpreter and the reduced expression translated back to a Colourful expression. 

The reason translating back to a Colourful expression is difficult is because due to the lack of parentheses, the language, when using only pre-defined colours, is not Turing complete. In order to have a target for any possible SKI expression in Colourful, it is necessary to add user-defined colours. The backwards compilation step from SKI to Colourful would need to define colours that the original program did not define. This is possible in theory, but may confuse the user.

With the exception of colour definitions, the entire program is treated as a single expression and transpiled to a single expression in the SKI combinator calculus.

## Implementation

1. The first step is a transpiler written in Haskell which translates an expression in Colourful to an expression in the SKI combinator calculus.

2. The next step is an interpreter for the SKI combinator calculus.

3. The ultimate goal is to also implement a backwards transpiler which translates the reduced (evaluated) SKI expression back to an expression in Colourful.

4. Stretch goal: use an LLM to create a story using the colours in the correct order such that the output is a story with evaluated colours.

5. A future goal (but beyond the current scope of the #language-makers jam) is an implementation with a GUI that can display the colours. For this purpose, colours will be assigned hex codes for display purposes.

The compilation process can be terminated at any of these steps, revealing the internal workings. For example, you can do only the first step to inspect what the Colourful program you've written looks like as an SKI expression. You can stop at step 2 if you want to know what this SKI expression evaluates to. Or you can go on to step 3 to get what you would expect to get if you just evaluated the Colourful program directly.

The main reason for this convoluted way of evaluating the expression is to increase absurdity. Another reason is to highlight the connection between Colourful and the SKI combinator calculus.

## #language-makers Theme

### The end is not the end

Colourful was designed for the #language-makers programming language jam with the theme "the end is not the end".

The programming language jam allocated one week for the design and one week for the implementation. Bear this in mind when trying to use Colourful.

### How Colourful fits this theme

Colourful compiles the program from end to beginning, i.e. in the opposite direction of most programming languages. Combinators are applied to each other from right to left and bottom to top. The language has "reverse lexical scope" (a term invented for Colourful specifically, as far as I'm aware), meaning colours have to be defined *AFTER* they are used.

## Usage

The Haskell tool stack is recommended.

Clone the repository (or download the code) and run it with

`stack run`

or

`stack ghci`

This project is work in progress. Instructions might change.

Please find more information in the [Documentation](https://github.com/AR2202/Colourful/blob/main/Documentation.md).

Also have a look at the examples in the examples folder to see some Colourful programs.
