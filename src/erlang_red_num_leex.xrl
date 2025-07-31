Definitions.

WHITESPACE  = [\s\t\n\r]
HEX         = [0-9a-fA-F]+
NUMS        = [0-9]+
NEG         = [-]
FULLSTOP    = [\.]
HEXADECIMAL = 0[xX]{HEX}
BINARY      = 0[bB][01]+
EXPONENT    = [eE]

Rules.

{WHITESPACE}+ : skip_token.

{HEXADECIMAL} : {token, {hexadecimal, TokenLine, TokenChars}}.
{BINARY}      : {token, {binary, TokenLine, TokenChars}}.
{NUMS}        : {token, {integer, TokenLine, TokenChars}}.
{NEG}         : {token, {'-', TokenLine}}.
{FULLSTOP}    : {token, {'.', TokenLine}}.
{EXPONENT}    : {token, {'e', TokenLine}}.

Erlang code.

%% Nothing here.
