Erlang-Red Parser Collection
=====

Besides the [JSONata](https://github.com/gorenje/erlang-red-jsonata) parser, there are a few other things that need parsing in Node-RED. This is needed because Node-RED is NodeJS and uses much Javascript syntax.

Hence this is a collection of parsers to better handle the specifics of Node-RED.

## 1. Attribute Access parser

Node-RED is based on NodeJS which has many access the keys inside an object, mixing array indices with hash keys:

    fubar[1]['1'].keyname // [1] is array, '1' is key name
    'fubar'.keyname["anotherkey"] // all keynames

There are many possible combinations which aren't really handled with a single regular expression.

This attribute access can be used anywhere, both for retrieval and setting values:

![attribute acce](.images/attributeaccess.png)

## 2. Number Type parser

Again, Javascript and hence Node-RED are very literal in interpreting numbers, a semi complete [test](https://flows.red-erik.org/f/fb50bac16667fc54) has just some examples.

The field symbolic for this parser:

![number field](.images/numberfieldsymblic.png)

Node-RED also extends this interpretation to include numbers defined as hexadecimal and binary:

    0xfeedbabe // 4276992702
    0b10101110 // 174

So that all needs to be supported by some kind of parser. My favourite valid number in Node-RED is negative zero: `-.0e-1`.

## 3. Buffer Type parser

A rarely used type for my usage, it is hidden behind the buffer field:

![buffer field symbolic](.images/bufferfieldsymbolic.png)

This type defines a Javascript Buffer object which is basically just binary data, i.e. byte-wise data stored in a Buffer object.

This matches nicely with binary values in Erlang, so this parser handles that. It generates a list of numbers and it is up to the caller to convert that to whatever they like.

Example of the strings that this parser expects:

    [1,2,3,4,5]
    [0x34, 0b111]
    [0b111, 0b101, 0b110]

But that's where this parser goes beyond what Node-RED does. Node-RED assumes that the value defined in a buffer type is parsable JSON array string, meaning that hexadecimal and binary definitions aren't supported.

Node-RED errors out on the last two examples above.

I find this restriction just that: a restriction. So this parser is more liberal and can handling this hexadecimal and binary specifications for numbers.

This parser returns a list of Numbers, meaning floats are included and negative numbers also. The buffer type will reduce to this to a set of positive byte values. But that is done over at Erlang-Red, not here.

## 4. Packet Type parser

An implementation of the [Packet](https://bigeasy.github.io/packet) binary specification from NodeJS ecosystem. It will provide the basis for the [binary node](https://flows.nodered.org/node/node-red-contrib-binary) that is coming to Erlang-Red.

The purpose emulate an Erlang representation, for example:

    <<_:8/bits, 1:1, Len:7/bits, 1:1, Len2:7/bits, 1:1, Len3:7/bits, 0:1, Len4:7/bits, Rest/bytes>>

which is understandable but could be made even more low-code by using a Packet representation:

    x8,
    x1 => 1,
    b7 => len,
    x1 => 1,
    b7 => len2,
    x1 => 1,
    b7 => len3,
    x1 => 0,
    b7 => len4

that would then result in a map containing each value:

    #{ <<"len">> => ..., <<"len2">> => ..., <<"len3">> => ..., <<"len4">> => ..., }

This is what the Packet Type parser does - it takes a Packet representation and transforms that to an Erlang representation and returns hash values containing the individual specified values.

Packet type parser example
----

Reading the chunks from an PNG image:

```erlang
collect_png_chunks(<<>>, Acc, _ChunkFunc) ->
    lists:reverse(Acc);
collect_png_chunks(Data, Acc, ChunkFunc) ->
    {Chunk, Rest} = ChunkFunc(Data),
    collect_png_chunks(Rest, [Chunk | Acc], ChunkFunc).

check_parsing_of_png_image_test() ->
    HeaderDef = "
       x8 => 0x89,
       x8 => 0x50,
       x8 => 0x4E,
       x8 => 0x47,
       x8 => 0x0D,
       x8 => 0x0A,
       x8 => 0x1A,
       x8 => 0x0A
    ",

    ChunkDef = "
       b32         => length,
       b8[4]       => type,
       b8[$length] => data,
       b32         => crc
    ",

    {ok, HeaderFunc} = erl_packetparser:erlang_func_for_packetdef(HeaderDef),
    {ok, ChunkFunc} = erl_packetparser:erlang_func_for_packetdef(ChunkDef),

    {ok, PngData} =
        file:read_file(code:priv_dir(erlang_red_parsers) ++ "/test.png"),

    % skim off the header.
    {#{}, RestData} = HeaderFunc(PngData),

    % retrieve all chunks defined in the png
    Chunks = collect_png_chunks(RestData, [], ChunkFunc),
    Types = [T || #{ <<"type">> := T } <- Chunks ],

    ?assertEqual(["IHDR","iCCP","eXIf","iTXt","IDAT","IEND"], Types).
```

This takes advantage of the variable reference which is an extension to the Packet definition language. The reference makes this kind of data handling super simple since the packets defined their lengths.

Alternative would be a multi step: read first part of packet, create a new packet defintion, read data chunk, rinse and repeat.

By using a reference to a length already defined, the packet can be parsed in one go:

```
b32         => length,
b8[4]       => type,
b8[$length] => data,
b32         => crc
```

Use a `$` prefix for the referenced variable name to make this work.

Build
-----

    $ rebar3 compile
