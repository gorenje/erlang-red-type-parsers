-module(erl_attributeparser).

-export([
    attributes_to_array/1
]).

%%
%% This is a special convertor for Erlang. It handles all possible
%% combinations of Javascript object access but returns different keys
%% depending on structure:
%%
%%   - ['key'] ['Key'] ['123-key']
%%       single quotes come back as atoms: key, 'Key', '123-key' in this case.
%%   - ["key"] ["Key"] ["1234-key"]
%%       double quotes come back as <<binaries>>: <<"key">>, <<"Key">> and
%%       <<"1234-key">> in this case
%%   - key key123 Key123
%%       just on their own, keys come back as binaries: <<"key">> <<"key123">>...
%%       in this case.
%%
%% this means that key.['123-fubar']["Bnana"].CapitalKey would become
%%    [<<"key">>, '123-fubar', <<"Bnana">>, <<"CapitalKey">>]
%%
%% Input here is a property specification string: "key.key3.key4" or a combination
%% of bracketss and dots: "key[\"Key2\"]['key3'].Fubar"
%%
%% Returned is an array of goodness or error of failure.
%%
%% NOTE: because all values are assumed to be prefixed by `msg.` (a Node-RED
%%       thing), anything starting with a bracket (as the two examples above)
%%       is illegal. An expression can start with quotes or atoms.
%%
attributes_to_array(Str) when is_binary(Str) ->
    attributes_to_array(binary_to_list(Str));
attributes_to_array("") ->
    {ok, []};
attributes_to_array(Str) ->
    case erlang_red_attr_leex:string(Str) of
        {ok, Tokens, _} ->
            case erlang_red_attr_parser:parse(Tokens) of
                {ok, Result} ->
                    {ok, Result};
                {error, Error} ->
                    {error, Error}
            end;
        R ->
            R
    end.
