% @noformat
% Nice Here.
%
-file("/code/src/erlang_red_packet_type_parser.yrl", 0).
-module(erlang_red_packet_type_parser).
-file("/code/src/erlang_red_packet_type_parser.erl", 6).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/code/src/erlang_red_packet_type_parser.yrl", 102).

%%
%% Take the list of arguments and convert to a function containing binary
%% matchers and map definition.
convert(Args) ->
    %% ensure that all definitions have a name, assigning internale field names
    {Args3, _IgnoreCnt} = add_field_names(Args),

    %% construct the main Binary matcher which also takes a UnmatchedBytes
    %% field name.
    Args4 = create_binary_matcher(Args3, []) ++ ["UnmatchedBits/bits"],
    MainBinaryMatcher = lists:join(", ", Args4),

    %% For each strcture defined, we add an extra binary matcher.
    StructMatcher =
        lists:filter(
            fun(Elem) -> Elem =/= <<>> end,
            [
                structure_binary_matchers(Structure, NameStr)
            || [_SigEnd, _Array, {Structure}, {name, NameStr}] <- Args3
            ]
        ),

    %% this also adds the structure variable names to the hash.
    HashmapDef = lists:join(", ", create_hashmap_def(Args3, [])),

    %% put it all together.
    function_stanza(MainBinaryMatcher, StructMatcher, HashmapDef).


%%
%%
function_stanza(BinaryMatcher, [], HashMapDef) ->
    list_to_binary(io_lib:format(
       "fun (Binary) ->
             <<~s>> = Binary,

             <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                                    UnmatchedBits/bits>> = Binary,

             {ok, #{ ~s }, MatchedBits, UnmatchedBits}
        end.", [BinaryMatcher, HashMapDef]));
function_stanza(BinaryMatcher, StructMatcher, HashMapDef) ->
    list_to_binary(io_lib:format(
       "fun (Binary) ->
             <<~s>> = Binary,
             ~s,

             <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                                    UnmatchedBits/bits>> = Binary,

             {ok,  #{ ~s }, MatchedBits, UnmatchedBits}
        end.", [BinaryMatcher, lists:join(",\n", StructMatcher), HashMapDef])).

%%
%%
structure_binary_matchers([], _NameStr) ->
    <<>>;
structure_binary_matchers(Structure, NameStr) ->
    io_lib:format("<<~s>> = V~s", [
        lists:join(", ", create_binary_matcher(Structure, [])), NameStr
    ]).

%%
%%
create_hashmap_def([], Acc) ->
    lists:reverse(Acc);

create_hashmap_def(
  [
    [{_Signedness, {$x, _Size, _Postfix}},
     _Array,
     _Structure,
     _Name
    ] | Rest
  ],
  Acc
) ->
    create_hashmap_def(Rest, Acc);
create_hashmap_def(
  [
    [_SigEnd,
     {array_spec, {size, ArySize}},
     {[]},
     {name, NameStr}
    ] | Rest
  ],
  Acc
) when ArySize =:= 1 ->
    case is_internal_field_name(NameStr) of
        true ->
            create_hashmap_def(Rest, Acc);
        false ->
            Str = io_lib:format("<<\"~s\">> => V~s", [NameStr, NameStr]),
            create_hashmap_def(Rest, [Str | Acc])
    end;
create_hashmap_def(
  [
    [ {Signed, {Endian, BitSize, Postfix}},
      {array_spec, {size, ArySize}},
      {[]},
      {name, NameStr}
    ] | Rest
  ],
  Acc
) when ArySize > 1 ->
    case is_internal_field_name(NameStr) of
        true ->
            create_hashmap_def(Rest, Acc);
        false ->
            Str = io_lib:format("<<\"~s\">> => [ X || <<X:~s>> <= V~s]",
                                [NameStr,
                                 typespec(Endian, Signed, BitSize,
                                          Postfix, {[]}, false), NameStr]),
            create_hashmap_def(Rest, [Str | Acc])
    end;
create_hashmap_def(
  [
    [_SigEnd,
     {array_spec, {var_ref, _Name}},
     {[]},
     {name, NameStr}
    ] | Rest
  ],
  Acc
) ->
    case is_internal_field_name(NameStr) of
        true ->
            create_hashmap_def(Rest, Acc);
        false ->
            Str = io_lib:format("<<\"~s\">> => V~s", [NameStr, NameStr]),
            create_hashmap_def(Rest, [Str | Acc])
    end;
create_hashmap_def(
  [
    [_SigEnd,
     {array_spec, {operation, _Operation}},
     {[]},
     {name, NameStr}
    ] | Rest
  ],
  Acc
) ->
    case is_internal_field_name(NameStr) of
        true ->
            create_hashmap_def(Rest, Acc);
        false ->
            Str = io_lib:format("<<\"~s\">> => V~s", [NameStr, NameStr]),
            create_hashmap_def(Rest, [Str | Acc])
    end;
create_hashmap_def(
  [
    [ SigEndTuple,
      ArrayTuple,
      {Structure},
      NameTuple
    ] | Rest
  ],
  Acc
) ->
    create_hashmap_def([[SigEndTuple, ArrayTuple, {[]}, NameTuple] | Rest],
                       create_hashmap_def(Structure, Acc)).

%%
%%
create_binary_matcher([], Acc) ->
    lists:reverse(Acc);

create_binary_matcher(
  [
    [{_Signedness, {$x, Size, _Postfix}},
     {array_spec, {size, Cnt}},
     _Structure,
     {name, NameStr}
    ] | Rest
  ],
  Acc
) ->
    %% 'x' is ignore a value however if it has a name, then it's an expected
    %% value. This has a proper defined name, that is then an expected value.
    %% This is intended for binary values, i.e. a bit is either one or zero.
    %% This isn't intended for integer of larger values.
    case is_internal_field_name(NameStr) of
        false ->
            create_binary_matcher(
              Rest,
              [io_lib:format("~b:~b", [list_to_integer(NameStr),
                                       Size * Cnt]) | Acc]
             );
        true ->
            create_binary_matcher(
              Rest,
              [io_lib:format("_V~s:~b/bits", [NameStr, Size * Cnt]) | Acc]
             )
    end;

create_binary_matcher(
  [
    [
     {Signedness, {Endianness, Size, Postfix}},
     {array_spec, {var_ref, VarName}},
     {[]},
     {name, NameStr}
    ] | Rest
  ],
  Acc
) ->
    create_binary_matcher(
      Rest,
      [
       io_lib:format("V~s:~s",
                     [NameStr,
                        typespec(Endianness,
                                 Signedness,
                                 {Size, VarName},
                                 Postfix,
                                 {[]},
                                 var_ref)]) | Acc
      ]
     );

create_binary_matcher(
  [
    [
     {Signedness, {Endianness, Size, Postfix}},
     {array_spec, {operation, OperationSpec}},
     {[]},
     {name, NameStr}
    ] | Rest
  ],
  Acc
) ->
    create_binary_matcher(
      Rest,
      [
       io_lib:format("V~s:~s",
                     [NameStr,
                        typespec(Endianness,
                                 Signedness,
                                 {Size, oper_to_expr(OperationSpec)},
                                 Postfix,
                                 {[]},
                                 oper)]) | Acc
      ]
     );

create_binary_matcher(
  [
    [
     {Signedness, {Endianness, Size, Postfix}},
     {array_spec, {size, Cnt}},
     Structure,
     {name, NameStr}
    ] | Rest
  ],
  Acc
) ->
    create_binary_matcher(
      Rest,
      [
       io_lib:format("V~s:~s",
                     [NameStr,
                        typespec(Endianness,
                                 Signedness,
                                 Size * Cnt,
                                 Postfix,
                                 Structure,
                                 Cnt > 1)]) | Acc
      ]
     ).


%%
%%
oper_to_expr([{name, VarName1}, Op1, {operation, [{name, VarName2}, Op2, {size, Num}]}]) ->
    io_lib:format("(V~s ~s V~s ~s ~b)", [VarName1, Op1, VarName2, Op2, Num]);
oper_to_expr([{name, VarName}, Operator, {size, Num}]) ->
    io_lib:format("(V~s ~s ~b)", [VarName, Operator, Num]);
oper_to_expr(Operation) ->
    io_lib:format("Unknown Operation: ~p", [Operation]).


%% when creating the typespec, leave anything that has a structure or defines
%% an array of values as a bitstring - these things get matched further, hence
%% they need to remain binaries
typespec(_Endianness, _Signedness, {Size, Expression}, _Postfix, {[]}, oper) ->
    io_lib:format("(~b*~s)/bits", [Size, Expression]);
typespec(_Endianness, _Signedness, {Size, VarName}, _Postfix, {[]}, var_ref) ->
    io_lib:format("(~b*V~s)/bits", [Size,VarName]);

typespec($l, signed, Size, $f, {[]}, false) ->
    io_lib:format("~b/float-little-signed", [Size]);
typespec($l, unsigned, Size, $f, {[]}, false) ->
    io_lib:format("~b/float-little-unsigned", [Size]);
typespec($b, signed, Size, $f, {[]}, false) ->
    io_lib:format("~b/float-big-signed", [Size]);
typespec($b, unsigned, Size, $f, {[]}, false) ->
    io_lib:format("~b/float-big-unsigned", [Size]);

typespec($l, signed, Size, _Postfix, {[]}, false) ->
    io_lib:format("~b/integer-little-signed", [Size]);
typespec($l, unsigned, Size, _Postfix, {[]}, false) ->
    io_lib:format("~b/integer-little-unsigned", [Size]);
typespec($b, signed, Size, _Postfix, {[]}, false) ->
    io_lib:format("~b/integer-big-signed", [Size]);
typespec($b, unsigned, Size, _Postfix, {[]}, false) ->
    io_lib:format("~b/integer-big-unsigned", [Size]);

typespec(_Endianness, _Signedness, Size, _Postfix, _Structure, _IsArray) ->
    io_lib:format("~b/bits", [Size]).

%%
%% Assign internal field names to definitions that have no name. This allows
%% us to reference these fields when creating the binary matcher. If these
%% fields then have structures or arrays, these internal field names can be
%% used to match the values.
add_field_names(Args) ->
    add_field_names(Args, [], 1).

add_field_names( [], Acc, Cnt ) ->
    {lists:reverse(Acc), Cnt};

add_field_names( [ [SigEnd, Array, {[]}, nonm] | Rest ], Acc, Cnt ) ->
    New = [SigEnd, Array, {[]}, {name, internal_field_name(Cnt)}],
    add_field_names(Rest, [New | Acc], Cnt + 1);

add_field_names( [ [SigEnd, Array, {Structure}, nonm] | Rest ], Acc, Cnt ) ->
    {NewStruct, NewCnt} = add_field_names(Structure, [], Cnt),
    New = [SigEnd, Array, {NewStruct}, {name, internal_field_name(Cnt)}],
    add_field_names(Rest, [New | Acc], NewCnt + 1);

add_field_names( [ [_SigEnd, _Array, {[]}, _Name] = Whole | Rest ], Acc, Cnt ) ->
    add_field_names(Rest, [Whole | Acc], Cnt);

add_field_names( [ [SigEnd, Array, {Structure}, Name] | Rest ], Acc, Cnt ) ->
    {NewStruct,NewCnt} = add_field_names(Structure, [], Cnt),
    New = [SigEnd, Array, {NewStruct}, Name],
    add_field_names(Rest, [New | Acc], NewCnt + 1);

add_field_names( [ Whole | Rest ], Acc, Cnt ) ->
    add_field_names(Rest, [Whole | Acc], Cnt).

%%
%%
internal_field_name(Cnt) ->
    io_lib:format("internalField~b", [Cnt]).

is_internal_field_name(Name) ->
    string:prefix(Name, "internalField") =/= nomatch.

%%
%%
to_name({unsigned, {E, L, nopf}}) ->
    {name, [E] ++ integer_to_list(L)};
to_name({unsigned, {E, L, P}}) ->
    {name, [E] ++ integer_to_list(L) ++ [P]};
to_name({number,Num}) ->
    {name, Num};
to_name({hex, HexStr}) ->
    {name, convert_hex(HexStr)}.

%%
%%
convert_hex([$0 | V]) ->
    integer_to_list(convert_hex_remove_x(V)).

convert_hex_remove_x([$X | V]) ->
    hexstring_to_number(V, length(V) rem 2);
convert_hex_remove_x([$x | V]) ->
    hexstring_to_number(V, length(V) rem 2).

hexstring_to_number(V, 1) ->
    hexstring_to_number([$0 | V], 0);
hexstring_to_number(V, 0) ->
    binary:decode_unsigned(binary:decode_hex(list_to_binary(V))).

-file("/usr/local/lib/erlang/lib/parsetools-2.6/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-ifdef (YECC_PARSE_DOC).
-doc ?YECC_PARSE_DOC.
-endif.
-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_location}, 0, [], []).

-ifdef (YECC_PARSE_AND_SCAN_DOC).
-doc ?YECC_PARSE_AND_SCAN_DOC.
-endif.
-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_location}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_location}, 0, [], []).

-ifdef (YECC_FORMAT_ERROR_DOC).
-doc ?YECC_FORMAT_ERROR_DOC.
-endif.
-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(erl_anno:location(), any()) -> no_return().
return_error(Location, Message) ->
    throw({error, {Location, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Location, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Location}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, EndLocation} ->
            yeccpars1(Tokens, {{F, A}, EndLocation}, State, States, Vstack);
        {eof, EndLocation} ->
            yeccpars1([], {no_func, EndLocation}, State, States, Vstack);
        {error, Descriptor, _EndLocation} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_location}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, EndLocation}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(EndLocation), [],
              {no_func, EndLocation}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Location}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_location}) ->
    Location = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Location), [], {no_func, Location});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Location}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Location), [], {no_func, Location}).

%% For internal use only.
yecc_end(Location) ->
    {'$end', Location}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string(Token) ->
    try
        yecctoken2string1(Token)
    catch
        _:_ ->
            io_lib:format("~tp", [Token])
    end.

-compile({nowarn_unused_function, yecctoken2string1/1}).
yecctoken2string1({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string1({integer,_,N}) -> io_lib:write(N);
yecctoken2string1({float,_,F}) -> io_lib:write(F);
yecctoken2string1({char,_,C}) -> io_lib:write_char(C);
yecctoken2string1({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string1({string,_,S}) -> io_lib:write_string(S);
yecctoken2string1({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string1({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string1({dot, _}) -> "'.'";
yecctoken2string1({'$end', _}) -> [];
yecctoken2string1({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string1(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("/code/src/erlang_red_packet_type_parser.erl", 573).

-dialyzer({nowarn_function, yeccpars2/7}).
-compile({nowarn_unused_function,  yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, 'signed', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'unsigned', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_0/7}).
-compile({nowarn_unused_function,  yeccpars2_0/7}).
yeccpars2_cont_0(S, 'hex', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, 'name', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, 'number', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_1/7}).
-compile({nowarn_unused_function,  yeccpars2_1/7}).
yeccpars2_1(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_2/7}).
-compile({nowarn_unused_function,  yeccpars2_2/7}).
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccgoto_root(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_3/7}).
-compile({nowarn_unused_function,  yeccpars2_3/7}).
yeccpars2_3(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccgoto_statements(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_4/7}).
-compile({nowarn_unused_function,  yeccpars2_4/7}).
yeccpars2_4(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_4(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_5/7}).
-compile({nowarn_unused_function,  yeccpars2_5/7}).
yeccpars2_5(S, '=>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccgoto_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).
-compile({nowarn_unused_function,  yeccpars2_6/7}).
yeccpars2_6(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccgoto_head(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_7/7}).
-compile({nowarn_unused_function,  yeccpars2_7/7}).
yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccgoto_tail(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
-compile({nowarn_unused_function,  yeccpars2_8/7}).
yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_tail(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_9/7}).
-compile({nowarn_unused_function,  yeccpars2_9/7}).
yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccgoto_tail(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_10/7}).
-compile({nowarn_unused_function,  yeccpars2_10/7}).
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_binary_spec(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_11/7}).
-compile({nowarn_unused_function,  yeccpars2_11/7}).
yeccpars2_11(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_11_$end'(Stack),
 yeccgoto_binary_spec(hd(Ss), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_11(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_11_,'(Stack),
 yeccgoto_binary_spec(hd(Ss), ',', Ss, NewStack, T, Ts, Tzr);
yeccpars2_11(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_11_=>'(Stack),
 yeccgoto_binary_spec(hd(Ss), '=>', Ss, NewStack, T, Ts, Tzr);
yeccpars2_11(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_11_['(Stack),
 yeccgoto_binary_spec(hd(Ss), '[', Ss, NewStack, T, Ts, Tzr);
yeccpars2_11(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_11_{'(Stack),
 yeccgoto_binary_spec(hd(Ss), '{', Ss, NewStack, T, Ts, Tzr);
yeccpars2_11(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_11_}'(Stack),
 yeccgoto_binary_spec(hd(Ss), '}', Ss, NewStack, T, Ts, Tzr);
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccgoto_tail(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_12/7}).
-compile({nowarn_unused_function,  yeccpars2_12/7}).
yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_head(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_13/7}).
-compile({nowarn_unused_function,  yeccpars2_13/7}).
yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_head(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_14/7}).
-compile({nowarn_unused_function,  yeccpars2_14/7}).
yeccpars2_14(S, '$', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'number', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_15: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_16/7}).
-compile({nowarn_unused_function,  yeccpars2_16/7}).
yeccpars2_16(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_17/7}).
-compile({nowarn_unused_function,  yeccpars2_17/7}).
yeccpars2_17(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_18/7}).
-compile({nowarn_unused_function,  yeccpars2_18/7}).
yeccpars2_18(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_statements_nost(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_19/7}).
-compile({nowarn_unused_function,  yeccpars2_19/7}).
yeccpars2_19(S, '=>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_statement_nost(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_20/7}).
-compile({nowarn_unused_function,  yeccpars2_20/7}).
yeccpars2_20(S, '=>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_21(S, 'unsigned', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_22/7}).
-compile({nowarn_unused_function,  yeccpars2_22/7}).
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_statement_nost(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_23/7}).
-compile({nowarn_unused_function,  yeccpars2_23/7}).
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_tail(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_24: see yeccpars2_21

-dialyzer({nowarn_function, yeccpars2_25/7}).
-compile({nowarn_unused_function,  yeccpars2_25/7}).
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_statement_nost(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_26: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_27/7}).
-compile({nowarn_unused_function,  yeccpars2_27/7}).
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_27_(Stack),
 yeccgoto_statements_nost(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_28/7}).
-compile({nowarn_unused_function,  yeccpars2_28/7}).
yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_structure(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_29/7}).
-compile({nowarn_unused_function,  yeccpars2_29/7}).
yeccpars2_29(S, 'signed', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, 'unsigned', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_30/7}).
-compile({nowarn_unused_function,  yeccpars2_30/7}).
yeccpars2_30(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_statement_nost(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_31/7}).
-compile({nowarn_unused_function,  yeccpars2_31/7}).
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_binary_spec(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
-compile({nowarn_unused_function,  yeccpars2_32/7}).
yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_32_(Stack),
 yeccgoto_statement_nost(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_33/7}).
-compile({nowarn_unused_function,  yeccpars2_33/7}).
yeccpars2_33(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_34/7}).
-compile({nowarn_unused_function,  yeccpars2_34/7}).
yeccpars2_34(S, 'name', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_35/7}).
-compile({nowarn_unused_function,  yeccpars2_35/7}).
yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_inside_array_spec(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_36/7}).
-compile({nowarn_unused_function,  yeccpars2_36/7}).
yeccpars2_36(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_inside_array_spec(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_37: see yeccpars2_14

-dialyzer({nowarn_function, yeccpars2_38/7}).
-compile({nowarn_unused_function,  yeccpars2_38/7}).
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_arithmetic(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_39/7}).
-compile({nowarn_unused_function,  yeccpars2_39/7}).
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_arithmetic(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_40/7}).
-compile({nowarn_unused_function,  yeccpars2_40/7}).
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_arithmetic(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_41/7}).
-compile({nowarn_unused_function,  yeccpars2_41/7}).
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_arithmetic(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_42/7}).
-compile({nowarn_unused_function,  yeccpars2_42/7}).
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_inside_array_spec(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_43/7}).
-compile({nowarn_unused_function,  yeccpars2_43/7}).
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_array_spec(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_44: see yeccpars2_21

-dialyzer({nowarn_function, yeccpars2_45/7}).
-compile({nowarn_unused_function,  yeccpars2_45/7}).
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_46/7}).
-compile({nowarn_unused_function,  yeccpars2_46/7}).
yeccpars2_46(S, 'hex', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, 'name', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, 'number', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, 'signed', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, 'unsigned', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_statements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_47/7}).
-compile({nowarn_unused_function,  yeccpars2_47/7}).
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_statements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_48: see yeccpars2_29

-dialyzer({nowarn_function, yeccpars2_49/7}).
-compile({nowarn_unused_function,  yeccpars2_49/7}).
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_statement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_arithmetic/7}).
-compile({nowarn_unused_function,  yeccgoto_arithmetic/7}).
yeccgoto_arithmetic(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(37, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_array_spec/7}).
-compile({nowarn_unused_function,  yeccgoto_array_spec/7}).
yeccgoto_array_spec(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_array_spec(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_array_spec(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_binary_spec/7}).
-compile({nowarn_unused_function,  yeccgoto_binary_spec/7}).
yeccgoto_binary_spec(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_spec(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_spec(26, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_spec(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_spec(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_spec(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_head/7}).
-compile({nowarn_unused_function,  yeccgoto_head/7}).
yeccgoto_head(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_head(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_head(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_inside_array_spec/7}).
-compile({nowarn_unused_function,  yeccgoto_inside_array_spec/7}).
yeccgoto_inside_array_spec(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_inside_array_spec(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_root/7}).
-compile({nowarn_unused_function,  yeccgoto_root/7}).
yeccgoto_root(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_statement/7}).
-compile({nowarn_unused_function,  yeccgoto_statement/7}).
yeccgoto_statement(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_statement_nost/7}).
-compile({nowarn_unused_function,  yeccgoto_statement_nost/7}).
yeccgoto_statement_nost(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement_nost(26, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_statements/7}).
-compile({nowarn_unused_function,  yeccgoto_statements/7}).
yeccgoto_statements(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statements(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_statements_nost/7}).
-compile({nowarn_unused_function,  yeccgoto_statements_nost/7}).
yeccgoto_statements_nost(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statements_nost(26=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_structure/7}).
-compile({nowarn_unused_function,  yeccgoto_structure/7}).
yeccgoto_structure(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tail/7}).
-compile({nowarn_unused_function,  yeccgoto_tail/7}).
yeccgoto_tail(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tail(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tail(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tail(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tail(26, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tail(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tail(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_2_/1}).
-dialyzer({nowarn_function, yeccpars2_2_/1}).
-compile({nowarn_unused_function,  yeccpars2_2_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 50).
yeccpars2_2_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                     convert(___1)
  end | __Stack].

-compile({inline,yeccpars2_3_/1}).
-dialyzer({nowarn_function, yeccpars2_3_/1}).
-compile({nowarn_unused_function,  yeccpars2_3_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 83).
yeccpars2_3_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          [lists:flatten(___1)]
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-dialyzer({nowarn_function, yeccpars2_5_/1}).
-compile({nowarn_unused_function,  yeccpars2_5_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 79).
yeccpars2_5_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                              [___1, nonm]
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-dialyzer({nowarn_function, yeccpars2_6_/1}).
-compile({nowarn_unused_function,  yeccpars2_6_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 74).
yeccpars2_6_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                 [___1, {array_spec, {size, 1}}, {[]}]
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-dialyzer({nowarn_function, yeccpars2_7_/1}).
-compile({nowarn_unused_function,  yeccpars2_7_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 58).
yeccpars2_7_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   to_name(___1)
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-dialyzer({nowarn_function, yeccpars2_8_/1}).
-compile({nowarn_unused_function,  yeccpars2_8_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 55).
yeccpars2_8_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   ___1
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-dialyzer({nowarn_function, yeccpars2_9_/1}).
-compile({nowarn_unused_function,  yeccpars2_9_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 57).
yeccpars2_9_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   to_name(___1)
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-dialyzer({nowarn_function, yeccpars2_10_/1}).
-compile({nowarn_unused_function,  yeccpars2_10_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 52).
yeccpars2_10_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,'yeccpars2_11_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_11_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_11_$end'/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 53).
'yeccpars2_11_$end'(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,'yeccpars2_11_,'/1}).
-dialyzer({nowarn_function, 'yeccpars2_11_,'/1}).
-compile({nowarn_unused_function,  'yeccpars2_11_,'/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 53).
'yeccpars2_11_,'(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,'yeccpars2_11_=>'/1}).
-dialyzer({nowarn_function, 'yeccpars2_11_=>'/1}).
-compile({nowarn_unused_function,  'yeccpars2_11_=>'/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 53).
'yeccpars2_11_=>'(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,'yeccpars2_11_['/1}).
-dialyzer({nowarn_function, 'yeccpars2_11_['/1}).
-compile({nowarn_unused_function,  'yeccpars2_11_['/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 53).
'yeccpars2_11_['(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,'yeccpars2_11_{'/1}).
-dialyzer({nowarn_function, 'yeccpars2_11_{'/1}).
-compile({nowarn_unused_function,  'yeccpars2_11_{'/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 53).
'yeccpars2_11_{'(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,'yeccpars2_11_}'/1}).
-dialyzer({nowarn_function, 'yeccpars2_11_}'/1}).
-compile({nowarn_unused_function,  'yeccpars2_11_}'/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 53).
'yeccpars2_11_}'(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_11_/1}).
-dialyzer({nowarn_function, yeccpars2_11_/1}).
-compile({nowarn_unused_function,  yeccpars2_11_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 56).
yeccpars2_11_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   to_name(___1)
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-dialyzer({nowarn_function, yeccpars2_12_/1}).
-compile({nowarn_unused_function,  yeccpars2_12_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 76).
yeccpars2_12_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                 [___1, {array_spec, {size, 1}}, {___2}]
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-dialyzer({nowarn_function, yeccpars2_13_/1}).
-compile({nowarn_unused_function,  yeccpars2_13_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 75).
yeccpars2_13_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                 [___1, ___2, {[]}]
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-dialyzer({nowarn_function, yeccpars2_18_/1}).
-compile({nowarn_unused_function,  yeccpars2_18_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 68).
yeccpars2_18_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                    [___1]
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-dialyzer({nowarn_function, yeccpars2_19_/1}).
-compile({nowarn_unused_function,  yeccpars2_19_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 62).
yeccpars2_19_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                                     [___1, {array_spec, {size, 1}}, {[]}, nonm]
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-dialyzer({nowarn_function, yeccpars2_22_/1}).
-compile({nowarn_unused_function,  yeccpars2_22_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 63).
yeccpars2_22_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                     [___1, {array_spec, {size, 1}}, {[]}, ___3]
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-dialyzer({nowarn_function, yeccpars2_23_/1}).
-compile({nowarn_unused_function,  yeccpars2_23_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 56).
yeccpars2_23_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   to_name(___1)
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-dialyzer({nowarn_function, yeccpars2_25_/1}).
-compile({nowarn_unused_function,  yeccpars2_25_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 64).
yeccpars2_25_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                     [___1, ___2, {[]}, ___4]
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-dialyzer({nowarn_function, yeccpars2_27_/1}).
-compile({nowarn_unused_function,  yeccpars2_27_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 69).
yeccpars2_27_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        [___1 | ___3]
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-dialyzer({nowarn_function, yeccpars2_28_/1}).
-compile({nowarn_unused_function,  yeccpars2_28_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 71).
yeccpars2_28_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                       ___2
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-dialyzer({nowarn_function, yeccpars2_30_/1}).
-compile({nowarn_unused_function,  yeccpars2_30_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 65).
yeccpars2_30_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                     [___3, {array_spec, {size, 1}}, {[]}, ___1]
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-dialyzer({nowarn_function, yeccpars2_31_/1}).
-compile({nowarn_unused_function,  yeccpars2_31_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 53).
yeccpars2_31_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_32_/1}).
-dialyzer({nowarn_function, yeccpars2_32_/1}).
-compile({nowarn_unused_function,  yeccpars2_32_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 66).
yeccpars2_32_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                     [___3, ___4, {[]}, ___1]
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-dialyzer({nowarn_function, yeccpars2_35_/1}).
-compile({nowarn_unused_function,  yeccpars2_35_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 94).
yeccpars2_35_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                              {size, list_to_integer(element(2,___1))}
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-dialyzer({nowarn_function, yeccpars2_36_/1}).
-compile({nowarn_unused_function,  yeccpars2_36_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 95).
yeccpars2_36_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                {var_ref, element(2,___2)}
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-dialyzer({nowarn_function, yeccpars2_38_/1}).
-compile({nowarn_unused_function,  yeccpars2_38_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 92).
yeccpars2_38_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    ___1
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-dialyzer({nowarn_function, yeccpars2_39_/1}).
-compile({nowarn_unused_function,  yeccpars2_39_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 90).
yeccpars2_39_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    ___1
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-dialyzer({nowarn_function, yeccpars2_40_/1}).
-compile({nowarn_unused_function,  yeccpars2_40_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 89).
yeccpars2_40_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    ___1
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-dialyzer({nowarn_function, yeccpars2_41_/1}).
-compile({nowarn_unused_function,  yeccpars2_41_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 91).
yeccpars2_41_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    ___1
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-dialyzer({nowarn_function, yeccpars2_42_/1}).
-compile({nowarn_unused_function,  yeccpars2_42_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 96).
yeccpars2_42_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {operation, [___2, element(1,___3), ___4]}
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-dialyzer({nowarn_function, yeccpars2_43_/1}).
-compile({nowarn_unused_function,  yeccpars2_43_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 87).
yeccpars2_43_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                          {array_spec, ___2}
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-dialyzer({nowarn_function, yeccpars2_45_/1}).
-compile({nowarn_unused_function,  yeccpars2_45_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 80).
yeccpars2_45_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              [___1, ___3]
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-dialyzer({nowarn_function, yeccpars2_46_/1}).
-compile({nowarn_unused_function,  yeccpars2_46_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 84).
yeccpars2_46_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              [lists:flatten(___1)]
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-dialyzer({nowarn_function, yeccpars2_47_/1}).
-compile({nowarn_unused_function,  yeccpars2_47_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 85).
yeccpars2_47_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         [lists:flatten(___1) | ___3]
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-dialyzer({nowarn_function, yeccpars2_49_/1}).
-compile({nowarn_unused_function,  yeccpars2_49_/1}).
-file("/code/src/erlang_red_packet_type_parser.yrl", 81).
yeccpars2_49_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                              [___3, ___1]
  end | __Stack].


-file("/code/src/erlang_red_packet_type_parser.yrl", 478).
