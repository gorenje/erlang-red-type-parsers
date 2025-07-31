%
% Part of a parser to parse access to map attributes.
% Node-RED has strings such as:
%   fubar.snauf.key - or
%   fubar["name-minus"]["name-plus"]
%   ["123ad"].finar["SNAFU"]
% are all legal ways to access the underlying hashmap that is the
% Message object
% This parser is designed to remove code like this -->
%   https://github.com/gorenje/erlang-red/blob/4ac143f4fc45ce8c3727e04767cc945c15231d18/src/ered_msg_handling.erl#L174-L181
%

Definitions.

NAME       = [a-zA-Z_-][a-zA-Z0-9_-]*
WHITESPACE = [\s\t\n\r]
DSTRING    = ["][^\"]*["]
SSTRING    = \'[^\']+\'
UCHARS     = [A-Z][a-z0-9A-Z_-]*
LCHARS     = [a-z][a-z0-9A-Z_-]*
NUMS       = [0-9]+

Rules.

{WHITESPACE}+ : skip_token.

{NAME}        : {token, {atom, TokenLine, TokenChars}}.
{DSTRING}     : {token, {dqstring, TokenLine, TokenChars}}.
{SSTRING}     : {token, {sqstring, TokenLine, TokenChars}}.
{UCHARS}      : {token, {uchars, TokenLine, TokenChars}}.
{LCHARS}      : {token, {lchars, TokenLine, TokenChars}}.
{NUMS}        : {token, {nums, TokenLine, TokenChars}}.

\.    : {token, {'.', TokenLine}}.
\[    : {token, {'[', TokenLine}}.
\]    : {token, {']', TokenLine}}.

Erlang code.

%% Nothing here.
