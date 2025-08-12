Header
"% @noformat"
"% Nice Here."
"%".

Nonterminals
  root
  binary_spec
  head
  statements
  statement
  statements_nost
  statement_nost
  structure
  tail
  array_spec
  inside_array_spec
  arithmetic
.

Terminals
  '=>'
  '{'
  '}'
  ','
  ':'
  '['
  ']'
  '$'
  '*'
  '-'
  '+'
  '/'
  unsigned
  signed
  name
  number
  hex
  bin
  octal
.

Rootsymbol
  root
.

%%
%% When things are missing ... Convention is to use noXX for when
%% stuff isn't defined:
%%
%%  - nopf - no post fix defined
%%  - nonm - no name defined, i.e., variable label
%%
%% These are used in the spot where the definitions should be so that
%% data structures are all the same length even if they are missing details.
root -> statements : convert('$1').

binary_spec -> signed   : '$1'.
binary_spec -> unsigned : '$1'.

tail -> name     : '$1'.
tail -> unsigned : to_name('$1').
tail -> number   : to_name('$1').
tail -> hex      : to_name('$1').
tail -> bin      : to_name('$1').
tail -> octal    : to_name('$1').

% structure aren't recursive, it is not possible to have a structure inside
% a structure - prevent that.
statement_nost -> binary_spec                      : ['$1', {array_spec, {size, 1}}, {[]}, nonm].
statement_nost -> binary_spec '=>' tail            : ['$1', {array_spec, {size, 1}}, {[]}, '$3'].
statement_nost -> binary_spec array_spec '=>' tail : ['$1', '$2', {[]}, '$4'].
statement_nost -> tail ':' binary_spec             : ['$3', {array_spec, {size, 1}}, {[]}, '$1'].
statement_nost -> tail ':' binary_spec array_spec  : ['$3', '$4', {[]}, '$1'].

statements_nost -> statement_nost : ['$1'].
statements_nost -> statement_nost ',' statements_nost : ['$1' | '$3'].

structure -> '{' statements_nost '}' : '$2'.

%%
head -> binary_spec            : ['$1', {array_spec, {size, 1}}, {[]}].
head -> binary_spec array_spec : ['$1', '$2', {[]}].
head -> binary_spec structure  : ['$1', {array_spec, {size, 1}}, {'$2'}].

%% either an array or a structure but not both.
statement -> head           : ['$1', nonm].
statement -> head '=>' tail : ['$1', '$3'].
statement -> tail ':' head  : ['$3', '$1'].

statements -> statement : [lists:flatten('$1')].
statements -> statement ',' : [lists:flatten('$1')].
statements -> statement ',' statements : [lists:flatten('$1') | '$3'].

array_spec -> '[' inside_array_spec ']' : {array_spec, '$2'}.

arithmetic -> '-' : '$1'.
arithmetic -> '+' : '$1'.
arithmetic -> '/' : '$1'.
arithmetic -> '*' : '$1'.

inside_array_spec -> bin : {size, convert_numeric_to_int(element(2,'$1'))}.
inside_array_spec -> hex : {size, convert_numeric_to_int(element(2,'$1'))}.
inside_array_spec -> octal : {size, convert_numeric_to_int(element(2,'$1'))}.
inside_array_spec -> number : {size, list_to_integer(element(2,'$1'))}.
inside_array_spec -> '$' name : {var_ref, element(2,'$2')}.
inside_array_spec -> '$' name arithmetic inside_array_spec :
                         {operation, ['$2', element(1,'$3'), '$4']}.

Erlang code.

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
oper_to_expr(
  [{name, VarName1}, Op1, {operation, [{name, VarName2}, Op2, {size, Num}]}]
) ->
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
to_name({number, Num}) ->
    {name, Num};
to_name({hex, HexStr}) ->
    {name, convert_numeric(HexStr)};
to_name({bin, BinStr}) ->
    {name, convert_numeric(BinStr)};
to_name({octal, OctalStr}) ->
    {name, convert_numeric(OctalStr)}.

%%
%%
convert_numeric([$0, $x | V]) ->
    integer_to_list(list_to_integer_with_base(V, 16));
convert_numeric([$0, $o | V]) ->
    integer_to_list(list_to_integer_with_base(V, 8));
convert_numeric([$0, $b | V]) ->
    integer_to_list(list_to_integer_with_base(V, 2)).

%%
%%
convert_numeric_to_int([$0, $x | V]) ->
    list_to_integer_with_base(V, 16);
convert_numeric_to_int([$0, $o | V]) ->
    list_to_integer_with_base(V, 8);
convert_numeric_to_int([$0, $b | V]) ->
    list_to_integer_with_base(V, 2).

%%
%%
list_to_integer_with_base(V, B) ->
    binary_to_integer(list_to_binary(V), B).
