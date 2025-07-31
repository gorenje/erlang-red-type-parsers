-module(erl_numparser).

-export([to_number/1]).

%%
%% A parser for handling the num type in Node-RED, this field can have
%% many different values:
%%
%%  0b1010111 - binary
%%  1213 - integer
%%  12.12 - float
%%  0xfeedbabe - hexadecimal
%%  123e112 - exponents
%%  123.e112 - dot & exponents
%%
%% and all their negative counterparts:
%%
%%  -0b1010111 - binary
%%  -1213 - integer
%%  -12.12 - float
%%  -0xfeedbabe - hexadecimal
%%  -123e112 - exponents
%%
%% Not forgetting negative exponents:
%%
%%  -123e-112 - exponents
%%  -123.e-112 - dot & negative exponents
%%
%% Floats that start with a fullstop:
%%
%%  .123
%%  .23e12
%%  .121e-12
%%
%% and plenty more that I have missed.
%%
%% My favourite float: '-.0e-1' negative zero
%%
%% So what seems like a simple case becomes complex, hence this parser.
%%

-spec to_number(Value :: list()) ->
    {error, Reason :: any()} | {ok, Number :: float() | integer()}.
to_number(Value) when is_binary(Value) ->
    to_number(binary_to_list(Value));
to_number(Value) ->
    case erlang_red_num_leex:string(Value) of
        {ok, Tokens, _} ->
            case erlang_red_num_parser:parse(Tokens) of
                {ok, Result} ->
                    {ok, Result};
                {error, _} = ParserError ->
                    ParserError
            end;
        {error, E, _} ->
            {error, E}
    end.
