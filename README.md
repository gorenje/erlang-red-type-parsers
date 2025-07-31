Erlang-Red Parser Collection
=====

Besides the [JSONata](https://github.com/gorenje/erlang-red-jsonata) parser, there are a few other things that need parsing in Node-RED. This is needed because Node-RED is NodeJS and uses much Javascript syntax.

Hence this is a collection of parsers to better handle the specifics of Node-RED.

## 1. Attribute parser

Node-RED is based on NodeJS which has many access the keys inside an object, mixing array indices with hash keys:

    fubar[1]['1'].keyname // [1] is array, '1' is key name
    'fubar'.keyname["anotherkey"] // all keynames

There are many possible combinations which aren't really handled with a single regular expression.

## 2. Number Type parser

Again, Javascript and hence Node-RED are very literal in interpreting numbers, a semi complete [test](https://flows.red-erik.org/f/fb50bac16667fc54) has just some examples.

Node-RED also extends this interpretation to include numbers defined as hexadecimal and binary:

    0xfeedbabe // 4276992702
    0b10101110 // 174

So that all needs to be supported by some kind of parsing.

## 3. Buffer Type parser

This "secret" type of Node-RED - or rather rarely used in my line of business.

This type also defining JS Buffer which is basically just binary data, i.e. byte-wise data stored in a Buffer object.

This converts nicely to a binary value in Erlang, so this parser handles that.

Example of the strings that this parser excepts:

    [1,2,3,4,5]
    [0x34, 0b111]
    [0b111, 0b101, 0b110]

But that's where this parser goes beyond what Node-RED does. Node-RED assumes that the value defined in a buffer type is parsable JSON array string, meaning that hexadecimal and binary definitions aren't supported.

I find this restriction just that: a restriction. So this parser is more liberal in handling this.

This parser returns a list of Numbers, meaning floats are included and negative numbers also. The buffer type will reduce to this to a set of positive byte values. But that is done over at Erlang-Red, not here.

Why?
----

Because Erlang makes it too easy to create leex/yacc parsers, so might as well use them!

Build
-----

    $ rebar3 compile
