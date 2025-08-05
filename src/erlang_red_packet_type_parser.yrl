Header
"%"
"% Nice Here."
"%".

Nonterminals
  root
  statements
  binary_spec
  head
  tail
  statement
  structure
.

Terminals
  '=>'
  '{'
  '}'
  ','
  ':'
  unsigned
  signed
  array_spec
  name
.

Rootsymbol
  root
.

root -> statements : convert('$1').

binary_spec -> signed : '$1'.
binary_spec -> unsigned : '$1'.

head -> binary_spec            : {'$1', noas}.
head -> binary_spec array_spec : {'$1', '$2'}.

tail -> name : '$1'.
tail -> unsigned : to_name('$1').

structure -> '{' statements '}' : '$2'.

statement -> head                     : ['$1', nost, nonm].
statement -> head structure           : ['$1', '$2', nonm].
statement -> head '=>' tail           : ['$1', nost, '$3'].
statement -> head structure '=>' tail : ['$1', '$2', '$4'].
statement -> tail ':' head            : ['$3', nost, '$1'].
statement -> tail ':' head structure  : ['$3', '$4', '$1'].

statements -> statement : ['$1'].
statements -> statement ',' statements : ['$1' | '$3'].


Erlang code.

convert(Args) ->
    Args2 = add_field_names( Args, [], 1 ),
    Args3 = add_array_specs( Args2, [] ),
    Acc = lists:join(", ", create_binary_matcher(Args3, [])),
    HashmapDef = lists:join(", ", create_hashmap_def(Args3, [])),

    list_to_binary(io_lib:format(
       "fun (Binary) -> <<~s>> = Binary, #{ ~s } end.",
              [Acc,HashmapDef])).

%%
%%
create_hashmap_def([], Acc) ->
    Acc ++ ["<<\"unmatched\">> => Unmatched"];

create_hashmap_def(
  [
    [{{_Signedness, _LN, {$x, _Size, _Postfix}}, _Array}, _Structure,
     {name, _LN2, _NameStr}
    ] | Rest
  ],
  Acc
) ->
    create_hashmap_def(Rest, Acc);
create_hashmap_def(
  [
    [{{_Signedness, _LN, {_Endianness, _Size, _Postfix}}, _Array}, _Structure,
     {name, _LN2, NameStr}
    ] | Rest
  ],
  Acc
) ->
    Str = io_lib:format("<<\"~s\">> => V~s", [NameStr,NameStr]),
    create_hashmap_def(Rest, [Str | Acc]).

%%
%%
create_binary_matcher([], Acc) ->
    lists:reverse(Acc) ++ ["Unmatched/bytes"];

create_binary_matcher(
  [
    [{{_Signedness, _LN, {$x,Size,_Postfix}}, {array_spec, _LN3, Cnt}},
     _Structure,
     {name, _LN2, NameStr}
    ] | Rest
  ],
  Acc
) ->
    create_binary_matcher(
      Rest,
      [io_lib:format("_V~s:~b/bits", [NameStr, Size * Cnt]) | Acc]
     );

create_binary_matcher(
  [
    [
     {{_Signedness, _LN, {_Endianness, Size, _Postfix}}, {array_spec, _L3, Cnt}},
     _Structure,
     {name, _LN2, NameStr}
    ] | Rest
  ],
  Acc
) ->
    create_binary_matcher(
      Rest,
      [io_lib:format("V~s:~b/bits", [NameStr, Size * Cnt]) | Acc]
     ).

%%
%%
add_array_specs( [], Acc ) ->
    lists:reverse(Acc);
add_array_specs(
  [
    [{T, noas}, Structure, Name] | Rest
  ],
  Acc
) ->
    New = [{T, {array_spec, -1, 1}}, Structure, Name],
    add_array_specs(Rest, [New | Acc]);
add_array_specs( [ Whole | Rest ], Acc ) ->
    add_array_specs(Rest, [Whole | Acc]).

%%
%%
add_field_names( [], Acc, _Cnt ) ->
    lists:reverse(Acc);
add_field_names( [ [H1, H2, nonm] | Rest ], Acc, Cnt ) ->
    New = [H1, H2, {name, -1, io_lib:format("field~b",[Cnt])}],
    add_field_names(Rest, [New | Acc], Cnt + 1);
add_field_names( [ Whole | Rest ], Acc, Cnt ) ->
    add_field_names(Rest, [Whole | Acc], Cnt).

%%
%%
to_name({unsigned, LineNum, {E, L, nopf}}) ->
    {name, LineNum, [E] ++ integer_to_list(L)};
to_name({unsigned, LineNum, {E, L, P}}) ->
    {name, LineNum, [E] ++ integer_to_list(L) ++ [P]}.
