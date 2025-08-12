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

ARROW      = [\=][\>]
COLON      = [\:]
COMMA      = [,]
NUM        = [0-9][0-9_]*
LCHARS     = [a-z_][a-z0-9A-Z_-]*
DOLLAR     = [$]
NEG        = [-]
PLUS       = [+]
MULTIPLE   = [*]
DIVIDE     = [/]
ENDIANNESS = [blx]
POSTFIX    = [f]

HEXCHARS   = 0[xX][a-f0-9A-F][a-f0-9A-F_]*
OCTALCHARS = 0[oO][0-7][0-7_]*
BINCHARS   = 0[bB][01][01_]*

BRACKET_OPEN  = [\[]
BRACKET_CLOSE = [\]]

CURLY_OPEN  = [\{]
CURLY_CLOSE = [\}]

Rules.

{WHITESPACE}+ : skip_token.

{NEG}{ENDIANNESS}{NUM}{POSTFIX} : {token, {signed,   signed_postfixed(TokenChars)}}.
{NEG}{ENDIANNESS}{NUM}          : {token, {signed,   signed(TokenChars)}}.
{ENDIANNESS}{NUM}{POSTFIX}      : {token, {unsigned, unsigned_postfixed(TokenChars)}}.
{ENDIANNESS}{NUM}               : {token, {unsigned, unsigned(TokenChars)}}.

{ARROW}       : {token, {'=>', TokenLine}}.
{CURLY_OPEN}  : {token, {'{', TokenLine}}.
{CURLY_CLOSE} : {token, {'}', TokenLine}}.
{COMMA}       : {token, {',', TokenLine}}.
{COLON}       : {token, {':', TokenLine}}.

{BRACKET_OPEN}  : {token, {'[', TokenLine}}.
{BRACKET_CLOSE} : {token, {']', TokenLine}}.
{DOLLAR}        : {token, {'$', TokenLine}}.

{LCHARS}     : {token, {name,   TokenChars}}.
{NUM}        : {token, {number, remove_underscores(TokenChars)}}.
{HEXCHARS}   : {token, {hex,    remove_underscores(TokenChars)}}.
{OCTALCHARS} : {token, {octal,  remove_underscores(TokenChars)}}.
{BINCHARS}   : {token, {bin,    remove_underscores(TokenChars)}}.

{NEG} : {token, {'-', TokenLine}}.
{PLUS} : {token, {'+', TokenLine}}.
{DIVIDE} : {token, {'/', TokenLine}}.
{MULTIPLE} : {token, {'*', TokenLine}}.

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

remove_underscores(Chars) ->
    string:lowercase(string:join(string:replace(Chars, "_", "", all),"")).
