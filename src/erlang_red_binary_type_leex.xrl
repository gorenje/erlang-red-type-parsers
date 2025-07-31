%
% The binary type in Node-RED converts a JSON array to binary values.
% It does it like this: JSON.parse(rule.to);
% Which means arrays like this: [1,2,3,4] work fine but arrays with
% Numbers don't, e.g. [0b101,0xef,12,1.23] <--- this is what I want.
%
% A proper binary type should be able to support hexadecimal and binary
% definitions. After all the number field in Node-RED can do that.
%
% All this parser does is assume an JSON array structure, i.e [ ..., ..., ]
% and generate an list of strings. That list of strings is then converted
% to numbers using the num parser, finally returning a list of numbers which is
% then converted to integers - after all this is binary values.
%
Definitions.

WHITESPACE = [\s\t\n\r]

HEX           = [0-9a-fA-F]+
NUMS          = [0-9]+
NEG           = [-]
FULLSTOP      = [\.]
COMMA         = [,]
HEXADECIMAL   = 0[xX]{HEX}
BINARY        = 0[bB][01]+
EXPONENT      = [eE]
BRACKET_OPEN  = [\[]
BRACKET_CLOSE = [\]]

Rules.

{WHITESPACE}+ : skip_token.

{HEXADECIMAL} : {token, {hexadecimal, TokenLine, TokenChars}}.
{BINARY}      : {token, {binary, TokenLine, TokenChars}}.
{NUMS}        : {token, {integer, TokenLine, TokenChars}}.

{NEG}      : {token, {'-', TokenLine}}.
{FULLSTOP} : {token, {'.', TokenLine}}.
{EXPONENT} : {token, {'e', TokenLine}}.
{COMMA}    : {token, {',', TokenLine}}.

{BRACKET_OPEN}  : {token, {'[', TokenLine}}.
{BRACKET_CLOSE} : {token, {']', TokenLine}}.

Erlang code.

%% Nothing here.
