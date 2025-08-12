Definitions.

WHITESPACE  = [\s\t\n\r]
NUMS        = [0-9_]+
NEG         = [-]
FULLSTOP    = [\.]
HEXADECIMAL = 0[xX][0-9a-fA-F][0-9a-fA-F_]*
BINARY      = 0[bB][01][01_]*
OCTAL       = 0[oO][0-7][0-7_]*
EXPONENT    = [eE]

Rules.

{WHITESPACE}+ : skip_token.

{HEXADECIMAL} : {token, {hexadecimal, TokenLine, remove_underscores(TokenChars)}}.
{BINARY}      : {token, {binary,      TokenLine, remove_underscores(TokenChars)}}.
{OCTAL}       : {token, {octal,       TokenLine, remove_underscores(TokenChars)}}.
{NUMS}        : {token, {integer,     TokenLine, remove_underscores(TokenChars)}}.
{NEG}         : {token, {'-', TokenLine}}.
{FULLSTOP}    : {token, {'.', TokenLine}}.
{EXPONENT}    : {token, {'e', TokenLine}}.

Erlang code.

remove_underscores(Chars) ->
    string:join(string:replace(Chars, "_", "", all),"").
