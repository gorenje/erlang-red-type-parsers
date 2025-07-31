-module(erl_binarytypeparser).

%%
%% Buffer/Binary types are definable in Node-RED and these are assumed to be
%% parsable JSON arrays. But this implies that
%%    [0b01011, 0xda, 12]
%% isn't a binary/buffer type. This I find incomplete.
%%
%% This parser then supports more complex types of buffer defintions.
-export([
    to_list/1
]).

to_list(Str) when is_binary(Str) ->
    to_list(binary_to_list(Str));
to_list("") ->
    {ok, []};
to_list(Str) ->
    case erlang_red_binary_type_leex:string(Str) of
        {ok, Tokens, _} ->
            case erlang_red_binary_type_parser:parse(Tokens) of
                {ok, Result} ->
                    {ok, Result};
                {error, Error} ->
                    {error, Error}
            end;
        R ->
            R
    end.
