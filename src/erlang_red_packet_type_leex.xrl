%
% This is an implementation of the packet specification used by the binary
% node.
%
% Binary Node --> https://flows.nodered.org/node/node-red-contrib-binary
% Packet Spec --> https://bigeasy.github.io/packet
%
% Example:
%   x8,
%   b8 => len,
%   l24a => id,
%   b8[4] => tag,
%   b8,
%   b16 => volt,
%   b16 => temp,
%   b8 => hum,
%   b8 => crc,
%   x8
%
% Simple and expressive.
%
Definitions.

WHITESPACE = [\s\t\n\r]

ARROW    = [\=][\>]
COLON    = [\:]
COMMA    = [,]
NUM      = [0-9]+
LCHARS   = [a-z_][a-z0-9A-Z_-]*
HEXCHARS = 0x[a-f0-9A-F]+
DOLLAR   = [$]

BRACKET_OPEN  = [\[]
BRACKET_CLOSE = [\]]

CURLY_OPEN  = [\{]
CURLY_CLOSE = [\}]

NEG        = [-]
ENDIANNESS = [blx]
POSTFIX    = [zfa]

Rules.

{WHITESPACE}+ : skip_token.

{NEG}{ENDIANNESS}{NUM}{POSTFIX} : {token, {signed,   signed_postfixed(TokenChars)}}.
{NEG}{ENDIANNESS}{NUM}          : {token, {signed,   signed(TokenChars)}}.
{ENDIANNESS}{NUM}{POSTFIX}      : {token, {unsigned, unsigned_postfixed(TokenChars)}}.
{ENDIANNESS}{NUM}               : {token, {unsigned, unsigned(TokenChars)}}.

{BRACKET_OPEN}{NUM}{BRACKET_CLOSE}            : {token, {array_spec, {size, array_size(TokenChars)}}}.
{BRACKET_OPEN}{DOLLAR}{LCHARS}{BRACKET_CLOSE} : {token, {array_spec, {var_ref, var_ref(TokenChars)}}}.

{ARROW}       : {token, {'=>', TokenLine}}.
{CURLY_OPEN}  : {token, {'{', TokenLine}}.
{CURLY_CLOSE} : {token, {'}', TokenLine}}.
{COMMA}       : {token, {',', TokenLine}}.
{COLON}       : {token, {':', TokenLine}}.

{LCHARS}   : {token, {name, TokenChars}}.
{NUM}      : {token, {number, TokenChars}}.
{HEXCHARS} : {token, {hex, TokenChars}}.

Erlang code.

signed_postfixed([$-|Str]) ->
    unsigned_postfixed(Str).

signed([$-|Str]) ->
    unsigned(Str).

unsigned_postfixed(Str) ->
    [Endianness | S2] = Str,
    [Postfix | S3] = lists:reverse(S2),
    {Endianness, list_to_integer(lists:reverse(S3)), Postfix}.

unsigned(Str) ->
    [Endianness | S2] = Str,
    {Endianness, list_to_integer(S2), nopf}.

array_size([$[|Str]) ->
    [$]|S2] = lists:reverse(Str),
    list_to_integer(lists:reverse(S2)).

var_ref([$[|Str]) ->
    [$]|S2] = lists:reverse(Str),
    [$$|S3] = lists:reverse(S2),
    S3.
