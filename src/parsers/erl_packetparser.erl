-module(erl_packetparser).

-export([
    execute/2
]).

execute(PacketDefinition, BinaryData) ->
    try
        case packetdef_to_erlang(PacketDefinition) of
            {ok, ErlangCode} ->
                case evaluate_erlang(binary_to_list(ErlangCode)) of
                    {ok, Func} ->
                        {ok, Func(BinaryData)};
                    Error ->
                        ErrMsg = io_lib:format(
                            "Stanza: {{{ ~p }}} Error: ~p",
                            [ErlangCode, Error]
                        ),
                        {error, ErrMsg}
                end;
            Error ->
                {error, Error}
        end
    catch
        E:M:S ->
            {exception, {E,M,S}}
    end.


packetdef_to_erlang(PacketDefString) ->
    case erlang_red_packet_type_leex:string(PacketDefString) of
        {ok, Tokens, _} ->
            case erlang_red_packet_type_parser:parse(Tokens) of
                {ok, Result} ->
                    {ok, Result};
                {error, Error} ->
                    {error, Error}
            end;
        R ->
            R
    end.

handle_local_function(Func, Arg) -> ok.

evaluate_erlang(Expression) ->
    case erl_scan:string(Expression) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Parsed} ->
                    case
                        erl_eval:exprs(
                            Parsed,
                            [],
                            {value, fun handle_local_function/2}
                        )
                    of
                        {value, Result, _} ->
                            {ok, Result}
                    end;
                Error ->
                    {error, Error}
            end;
        Error ->
            {error, Error}
    end.
