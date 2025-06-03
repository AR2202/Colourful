# Colourful

Welcome to Colourful - the language that brings colours into programming!

## Overview

Colourful is an esotheric programming language based on combinatory logic. Instead of single letters, combinators are named after colours. Basic combinators are pre-defined as keywords in the language. User defined colours can be introduced at the end of the programm. The language has "reverse lexical scope", i.e. everything has to be defined AFTER it is used.

## Syntax

In Colourful, only pre-defined or user-defined colours have meaning. All other characters (with the exception of the special form for colour definitions), including parentheses, are treated as comments. It is therefore possible to write a program as an entire story or poem, with only the colours having meaning. Due to these syntax rules, every program is valid Colourful syntax and it is impossible for syntax errors to occur. In a correct implementation of Colourful, parse errors are not possible either, nor are any compilation errors or runtime exceptions. 

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

Colours have to be defined AFTER they are used. Colour definitions should be placed at the end of the file, with any definitions that reference other definitions being placed BEFORE the definiton that references them.

The syntax for definitions is:

`end existingColours = newName newColour`

where 
* existingColours is a sequence of existing colours that make up the definition (in the usual evaluation order, i.e. right to left)
* newName is the name of the new colour, which can be anything except a pre-defined colour or a colour defined later in the program, or one of the keywords or the = sign
* newColour is a mandatory keyword
* end is a mandatory keyword
* = is a mandatory assignment operator

## Semantics

The semantics of Colourful are based on combinators from combinatory logic, with the following differences:
* in accordance with the theme of the jam, the program is evaluated end to start, so evaluation happens right to left
* there are no parentheses

Every possible string is a valid Colourful program, there are no errors or exceptions. Due to the absence of any static checks, it is very difficult to write correct programs.

Colour definitions have special syntax. However, syntactically incorrect definitions are just ignored.

Colours are immutable. If re-defined, only the last definition is valid. The pre-defined colours cannot be re-defined. Any such attempt is ignored.

Due to the absence of parentheses, a change of evaluation order requires defining a new colour as the (partial) application of one colour to another. 

As the system only allows application of colours to colours, any literals such as True, False, Integers etc have to be encoded using something analogous to church encodings.

### Example: booleans

Booleans need to be encoded as colors first.

True = Red 

False = Orange

a and b = Orange b a

a or b = b Red a 
### Evaluation rules of user defined colours
All definitions are immutbale, with the LAST definition being the relevant one. Pre-defined colours are defined last, AFTER all user-defined colours. It is not possible to re-define colours. 

Please do not define colours that are substrings or superstrings of pre-defined or existing user-defined colours. This might mess up the parsing (will be fixed in the future).

## Compilation

Colourful compiles to an expression in the SKI combinator calculus, which is the output of the program.

In the future (but probably beyond the scope of the #language-makers jam), this could be evaluated and the reduced expression translated back to a Colourful expression.

## #language-makers Theme

### The end is not the end

Colourful was designed for the #language-makers programming language jam with the theme "the end is not the end"

### How Colourful fits this theme

Colourful compiles the program from beginning to end, i.e. in the opposite direction of most programming languages. Combinators are applied to each other from right to left and bottom to top. The language has reverse lexical scope, meaning colours have to be defined AFTER they are used.

## Usage