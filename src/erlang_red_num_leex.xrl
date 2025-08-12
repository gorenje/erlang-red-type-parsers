Definitions.

WHITESPACE  = [\s\t\n\r]
NUMS        = [0-9]+
NEG         = [-]
FULLSTOP    = [\.]
HEXADECIMAL = 0[xX][0-9a-fA-F][0-9a-fA-F_]*
BINARY      = 0[bB][01][01_]*
EXPONENT    = [eE]

Rules.

{WHITESPACE}+ : skip_token.

{HEXADECIMAL} : {token, {hexadecimal, TokenLine, remove_underscore(TokenChars)}}.
{BINARY}      : {token, {binary, TokenLine, remove_underscore(TokenChars)}}.
{NUMS}        : {token, {integer, TokenLine, TokenChars}}.
{NEG}         : {token, {'-', TokenLine}}.
{FULLSTOP}    : {token, {'.', TokenLine}}.
{EXPONENT}    : {token, {'e', TokenLine}}.

Erlang code.

remove_underscore(Chars) ->
    string:join(string:replace(Chars, "_", "", all),"").
